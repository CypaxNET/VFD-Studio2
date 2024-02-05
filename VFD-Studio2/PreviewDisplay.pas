unit PreviewDisplay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, StudioCommon, Glyphs;


{ PreviewDisplay }
type
  TPreviewDisplay = class(TComponent)
  private
  protected
    FDisplayColor: TColor;
    FLayerMode: TLayerMode;   // OR, AND or XOR combination of the layers

    FGraphicsLayer: TBitmap; // graphics layer of preview display
    FTextLayer: TBitmap;     // text layer of preview display

    FWidth, FHeight: Integer;

  public
    property DisplayColor: TColor read FDisplayColor write FDisplayColor;
    property LayerMode: TLayermode read FLayerMode write FLayerMode;

    property GraphicsLayer: TBitmap read FGraphicsLayer write FGraphicsLayer;
    //property TextLayer: TBitmap read FTextLayer write FTextLayer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetSize(AWidth, AHeight: Integer);

    procedure ClearScreen;

    procedure CombineVirtualLayers(CombinedImage: TBitmap);
    procedure PaintString(AText: string; Col, Row: Integer);


  end;

implementation

constructor TPreviewDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraphicsLayer:= TBitmap.Create;
  FTextLayer:= TBitmap.Create;
  FGraphicsLayer.Monochrome:= True;
  FTextLayer.Monochrome:= True;
  FLayerMode:= lmXOR;
end;

destructor TPreviewDisplay.Destroy;
begin
  FGraphicsLayer.Free;
  FTextLayer.Free;
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
end;

procedure TPreviewDisplay.ClearScreen;
begin
  FGraphicsLayer.Canvas.Brush.Color:= clWhite;
  FGraphicsLayer.Canvas.FillRect(0, 0, FWidth, FHeight);
  FTextLayer.Canvas.Brush.Color:= clWhite;
  FTextLayer.Canvas.FillRect(0, 0, FWidth, FHeight);
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
  TmpBitmap.Canvas.Pixels[0,0]:= clYellow;
  CombinedImage.Canvas.Draw(0, 0, TmpBitmap);

  TmpBitmap.Free;

end;

procedure TPreviewDisplay.PaintString(AText: string; Col, Row: Integer);
var
  C: Char;         // current character
  Pixels: Byte;    // one vertical block of pixels within a glyph
  X, Y: Integer;   // position of pixel in virtual display layer
  Gx, Gy: Integer; // position within a glyph
  CurrentCol: Integer;  // current column
  CValue: Byte;         // Ord(C)
  NumberOfRows: Integer; // number of visible text rows in the display
begin
  NumberOfRows:= FGraphicsLayer.Height div GLYPH_H;

  AText:= TGlyphs.Adapt2Charmap(AText);

  CurrentCol:= Col;
  for C in AText do begin
    if (CurrentCol >= (FGraphicsLayer.Width div (GLYPH_W + GLYPH_GAP))) then
      Break;

    CValue:= Ord(C);
    for Gx:= 0 to (GLYPH_W - 1) do begin
      Pixels:= charMap8x6[CValue, Gx];
      X:= CurrentCol * (GLYPH_W + GLYPH_GAP) + Gx;
      for Gy:= 0 to (NumberOfRows - 1) do begin
        Y:= Row * GLYPH_H + Gy;
        if ((Pixels and (1 shl Gy)) <> 0) then begin
          FTextLayer.Canvas.Pixels[X, Y]:= clBlack;
        end else begin
          FTextLayer.Canvas.Pixels[X, Y]:= clWhite;
        end;
      end;
    end;
    Inc(CurrentCol);
  end;
end;

end.

