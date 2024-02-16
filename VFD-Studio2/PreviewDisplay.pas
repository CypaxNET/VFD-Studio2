unit PreviewDisplay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, StudioCommon, VFDisplay, Glyphs;


{ PreviewDisplay }
type
  TPreviewDisplay = class(TVFDisplay)
  private
  protected
    FDisplayColor: TColor;
    FLayerMode: TLayerMode;   // OR, AND or XOR combination of the layers

    FGraphicsLayer: TBitmap; // graphics layer of preview display
    FTextLayer: TBitmap;     // text layer of preview display
    FCombinedBitmap: TBitmap;

    FWidth, FHeight: Integer;

  public

    property DisplayColor: TColor read FDisplayColor write FDisplayColor;
    property LayerMode: TLayermode read FLayerMode write SetLayerMode;

    property GraphicsLayer: TBitmap read FGraphicsLayer write FGraphicsLayer;
    property TextLayer: TBitmap read FTextLayer write FTextLayer;
    property CombinedBitmap: TBitmap read FCombinedBitmap write FCombinedBitmap;


    { Constructor / Destructor }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Overloaded methods }
    procedure Connect(AInterface: string); override;
    procedure DspInit(XRes, YRes: Word); override;
    procedure ClearScreen; override;
    procedure ShowScreen(ALayer: Word); override;
    procedure PaintString(Text: string; X, Y: Integer); override;
    procedure PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word); override;
    procedure PaintPixel(X, Y: Word; IsInverted: Boolean); override;
    procedure PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean); override;
    procedure SetBrightness(Percent: Byte); override;
    procedure SetLayerMode(ALayerMode: TLayerMode); override;
    procedure Dbg(Value: Byte); override;

    procedure SetSize(AWidth, AHeight: Integer);
    procedure CombineVirtualLayers(CombinedImage: TBitmap);

  end;

implementation

constructor TPreviewDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraphicsLayer:= TBitmap.Create;
  FTextLayer:= TBitmap.Create;
  FCombinedBitmap:= TBitmap.Create;
  FGraphicsLayer.Monochrome:= True;
  FTextLayer.Monochrome:= True;
  FLayerMode:= lmXOR;
  FDisplayType:= 'PREVIEW';
  FNumLayers:= 2; // this display has two layers
end;

destructor TPreviewDisplay.Destroy;
begin
  FGraphicsLayer.Free;
  FTextLayer.Free;
  FCombinedBitmap.Free;
  inherited; // Also called parent class destroyer
end;

procedure TPreviewDisplay.SetSize(AWidth, AHeight: Integer);
begin
  FWidth:= AWidth;
  FHeight:= AHeight;
  FGraphicsLayer.Width:= FWidth;
  FGraphicsLayer.Height:= FHeight;
  FTextLayer.Width:= FWidth;
  FTextLayer.Height:= FHeight;
  FCombinedBitmap.Width:= FWidth;
  FCombinedBitmap.Height:=FHeight;
end;

procedure TPreviewDisplay.ClearScreen;
begin
  FGraphicsLayer.Canvas.Brush.Color:= clWhite;
  FGraphicsLayer.Canvas.FillRect(0, 0, FWidth, FHeight);
  FTextLayer.Canvas.Brush.Color:= clWhite;
  FTextLayer.Canvas.FillRect(0, 0, FWidth, FHeight);
end;

procedure TPreviewDisplay.PaintString(Text: string; X, Y: Integer);
var
  C: Char;         // current character
  Pixels: Byte;    // one vertical block of pixels within a glyph
  DX, DY: Integer;   // position of pixel in virtual display layer
  Gx, Gy: Integer; // position within a glyph
  CurrentCol: Integer;  // current column
  CValue: Byte;         // Ord(C)
  NumberOfRows: Integer; // number of visible text rows in the display
begin
  NumberOfRows:= FGraphicsLayer.Height div GLYPH_H;

  Text:= TGlyphs.Adapt2Charmap(Text);

  CurrentCol:= X;
  for C in Text do begin
    if (CurrentCol >= (FGraphicsLayer.Width div (GLYPH_W + GLYPH_GAP))) then
      Break;

    CValue:= Ord(C);
    for Gx:= 0 to (GLYPH_W - 1) do begin
      Pixels:= charMap8x6[CValue, Gx];
      DX:= CurrentCol * (GLYPH_W + GLYPH_GAP) + Gx;
      for Gy:= 0 to (NumberOfRows - 1) do begin
        DY:= Y * GLYPH_H + Gy;
        if ((Pixels and (1 shl Gy)) <> 0) then begin
          FTextLayer.Canvas.Pixels[DX, DY]:= clBlack;
        end else begin
          FTextLayer.Canvas.Pixels[DX, DY]:= clWhite;
        end;
      end;
    end;
    Inc(CurrentCol);
  end;

  CombineVirtualLayers(CombinedBitmap);
end;

procedure TPreviewDisplay.Connect(AInterface: string);
begin
  // nothing to do here
end;

procedure TPreviewDisplay.DspInit(XRes, YRes: Word);
begin
  SetSize(XRes, YRes);
end;

procedure TPreviewDisplay.ShowScreen(ALayer: Word);
begin
  // nothing to do here
end;

procedure TPreviewDisplay.PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word);
begin
  FGraphicsLayer.Canvas.Draw(XPos, YPos, ABitmap);
  CombineVirtualLayers(CombinedBitmap);
end;

procedure TPreviewDisplay.PaintPixel(X, Y: Word; IsInverted: Boolean);
begin
  if (IsInverted) then
    FGraphicsLayer.Canvas.Pixels[X, Y]:= clWhite
  else
    FGraphicsLayer.Canvas.Pixels[X, Y]:= clBlack;
  CombineVirtualLayers(CombinedBitmap);
end;

procedure TPreviewDisplay.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
begin
  if (IsInverted) then
    FGraphicsLayer.Canvas.Pen.Color:= clWhite
  else
    FGraphicsLayer.Canvas.Pen.Color:= clBlack;
  FGraphicsLayer.Canvas.Line(X0, Y0, X1, Y1);
  CombineVirtualLayers(CombinedBitmap);
end;

procedure TPreviewDisplay.SetBrightness(Percent: Byte);
begin
  // nothing to do here
end;

procedure TPreviewDisplay.SetLayerMode(ALayerMode: TLayerMode);
begin
  FLayerMode:= ALayerMode;
  CombineVirtualLayers(FCombinedBitmap);
end;

procedure TPreviewDisplay.Dbg(Value: Byte);
begin

end;

procedure TPreviewDisplay.CombineVirtualLayers(CombinedImage: TBitmap);
var
  TmpBitmap: TBitmap;
begin
  TmpBitmap:= TBitmap.Create;
  TmpBitmap.Width:= CombinedImage.Width;
  TmpBitmap.Height:= CombinedImage.Height;

  CombinedImage.Canvas.CopyMode:= cmNotSrcCopy;
  CombinedImage.Canvas.Draw(0, 0, FGraphicsLayer);

  TmpBitmap.Canvas.CopyMode:= cmNotSrcCopy;
  TmpBitmap.Canvas.Draw(0, 0, FTextLayer);

  if (lmOR = FLayerMode) then
    CombinedImage.Canvas.CopyMode:= cmSrcPaint
  else if (lmXOR = FLayerMode) then
    CombinedImage.Canvas.CopyMode:= cmSrcInvert
  else
    CombinedImage.Canvas.CopyMode:= cmSrcAnd;

  CombinedImage.Canvas.Draw(0, 0, TmpBitmap);

  // ink the combined black&white image
  TmpBitmap.Canvas.CopyMode:= cmSrcCopy;
  TmpBitmap.Width:= CombinedImage.Width;
  TmpBitmap.Height:= CombinedImage.Height;
  TmpBitmap.Canvas.Brush.Color:= FDisplayColor;
  TmpBitmap.Canvas.FillRect(0, 0, TmpBitmap.Width, TmpBitmap.Height);
  CombinedImage.Canvas.CopyMode:= cmSrcAnd;
  //TmpBitmap.Canvas.Pixels[0,0]:= clYellow;
  CombinedImage.Canvas.Draw(0, 0, TmpBitmap);

  TmpBitmap.Free;
end;

end.

