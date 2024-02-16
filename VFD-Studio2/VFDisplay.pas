unit VFDisplay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, ExtCtrls, StudioCommon;

type
  TIntferfaceType = (itNONE, itCOM);

  TInterfaceConfig = record
    IfaceType: TIntferfaceType;
    IfaceName: string;
    IsConnected: Boolean;
  end;

  PChar = ^Char;



type
  TLoggingProcedure = procedure(const LogLevel: TLogLevel; const Text: string; const Timestamp: TDateTime) of object;


  { VFDisplay }
  TVFDisplay = class(TComponent)
  private
  protected

    FDisplayType: string;           // type of display (e.g. 'NTK800'); must be set by child class

    { display layers }
    FNumLayers: Byte;               // number of layers the display has
    FSelectedLayer: Byte;           // currently selected display layer to draw on
    
    { display dimensions }
    FGfxWidth: Word;                // width of the display in pixels
    FGfxHeight: Word;               // height of the display in pixels
    FTxtWidth: Word;                // number of text columns the display can show  with the selected glyph table
    FTxtHeight: Word;               // number of text rows the display can show with the selected glyph table
    
    FIsLineBreak: Boolean;          // do a line break when printing text outside visible area?
    
    FLoggingCallback: TLoggingProcedure;        // callback procedure to report error strings

    FInterfaceConfig: TInterfaceConfig; // interface configuration

    { Other / helper methods }
    function ReverseBits(Bits: Byte): Byte;
    procedure MsDelay(const Milliseconds: QWord);

  public
    property OnDbgMessage: TLoggingProcedure read FLoggingCallback write FLoggingCallback;

    property DisplayType: string read FDisplayType;

    property IsConnected: Boolean read FInterfaceConfig.IsConnected;

    property GfxWidth: Word read FGfxWidth;
    property GfxHeight: Word read FGfxHeight;
    property TextWidth: Word read FTxtWidth;
    property TextHeight: Word read FTxtHeight;
    
    property CountOfLayers: Byte read FNumLayers;
    property SelectedScreen: Byte read FSelectedLayer;
    
    property LineBreak: Boolean read FIsLineBreak write FIsLineBreak;
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure Connect(AInterface: string); virtual; abstract;
    procedure DspInit(XRes, YRes: Word); virtual; abstract;
    procedure ClearScreen; virtual; abstract;
    procedure ShowScreen(ALayer: Word); virtual; abstract;
    procedure PaintString(Text: string; X, Y: Integer); virtual; abstract;
    procedure PaintBitmap(ABitmap: TBitmap; X, Y: Word); virtual; abstract;
    procedure PaintPixel(X, Y: Word; IsInverted: Boolean); virtual; abstract;
    procedure PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean); virtual; abstract;
    procedure PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
    procedure SetBrightness(Percent: Byte); virtual; abstract;
    procedure SetLayerMode(LayerMode: TLayerMode); virtual abstract;

    procedure Dbg(Value: Byte); virtual; abstract;

  end;

const
  LAYER_0     = 1;
  LAYER_1     = 2;
  BOTH_LAYERS = 3;

  GREY_VALUE_THRESHOLD = 160; // grey value of a pixel in a bitmap to be considered as 'on'

implementation

constructor TVFDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterfaceConfig.IsConnected:= False;
  FIsLineBreak:= False;
end;

destructor TVFDisplay.Destroy;
begin
  inherited; // Also called parent class destroyer
end;

procedure TVFDisplay.PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
begin
  PaintLine(X0, Y0, X1, Y0, IsInverted);
  PaintLine(X1, Y0, X1, Y1, IsInverted);
  PaintLine(X0, Y1, X1, Y1, IsInverted);
  PaintLine(X0, Y0, X0, Y1, IsInverted);
end;


{
 Helper function to reverse the order of bits in a Byte.
}
function TVFDisplay.ReverseBits(Bits: Byte): Byte;
var
  I: Integer;
begin
  Result:= 0;
  for I:= 1 to 8 do
  begin
    Result:= (Result shl Byte(1)) or (Bits and Byte(1));
    Bits:= Bits shr 1;
  end;
end;


{
 Helper procedure to slow down LPT I/O access.
}
procedure TVFDisplay.MsDelay(const Milliseconds: QWord);
var
  FirstTickCount: QWord;
begin
  FirstTickCount:= GetTickCount64;
  while ((GetTickCount64 - FirstTickCount) < Milliseconds) do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end;

end.

