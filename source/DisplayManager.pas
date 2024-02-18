unit DisplayManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, ExtCtrls, RegExpr, DateUtils,
  Math, StudioCommon, uSMBIOS, SysInfo, WinampControl,
  Glyphs, VFDisplay, PreviewDisplay, NTK800, NTK300;

type
  TDisplayConfig = record
    DisplayType: String; // e.g. 'NTK800'
    ResX: Word;          // display resolution in x direction
    ResY: Word;          // display resolution in y direction
    IntName: String;     // interface name (e.g. 'COM5')
    Baudrate: Cardinal;  // baudrate for serial display connection
  end;

type
  TPreviewChangedEvent = procedure(NewPreview: TBitmap) of object;

type
  { DisplayManager }
  TDisplayManager = class(TComponent)
  private
  protected
    FGfxWidth: Word;  // width of the display in pixels
    FGfxHeight: Word; // height of the display in pixels
    FTxtWidth: Word;  // number of text columns the display can show  with the selected glyph table
    FTxtHeight: Word; // number of text rows the display can show with the selected glyph table

    FPreviewColor: TColor;

    FDisplays: array of TVFDisplay;

    FLoggingCallback: TLoggingProcedure;        // callback procedure to report error strings

    FOnPreviewChangedEvent: TPreviewChangedEvent; // triggered when a preview display has changed its content

    // objects to gather system information from
    FSysInfo: TSysInfo;
    FSMBios: TSMBios;
    FWinampControl: TWinampControl;

    // storage for system information
    FVariableInfo: array[0..(MAX_VARIABLE_INFO - 1)] of TVariableInfo;  // holds textual information to be refreshed periodically
    FCpuUsageData: TCpuUsageData;   // hold information about current and average CPU load and a history of recent CPU usage
    FMemUsageData: TMemUsageData;   // hold information about current and average RAM load and a history of recent RAM usage

    FScrollStringIndex: Word;      // used for scolling texts


  public
    property OnDbgMessage: TLoggingProcedure read FLoggingCallback write FLoggingCallback;
    property OnPreviewChanged: TPreviewChangedEvent read FOnPreviewChangedEvent write FOnPreviewChangedEvent;
    procedure SetPreviewColor(const AColor: TColor);
    property PreviewColor: TColor read FPreviewColor write SetPreviewColor;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddDisplay(DisplayConfig: TDisplayConfig);
    procedure AddDisplay(DisplayType: String; ResX, ResY: Word; IntName: String; Baudrate: Cardinal);

    function GetVariableInfoCount: Integer;

    // utility methods
    procedure ForcePreviewUpdate;
    function GetAverageCpuLoad: Byte;
    procedure TrimBitmap(var Bmp: TBitmap);

    // display features
    procedure ClearScreen;
    procedure ShowScreen(ALayer: Word);
    procedure SetBrightness(Percent: Byte);
    procedure SetLayerMode(LayerMode: TLayerMode);
    procedure PaintString(AText: string; Col, Row: Integer);
    procedure PaintBitmap(ABitmap: TBitmap; X, Y: Word);
    procedure PaintBitmapFromFile(FileName: string; X, Y: Word);
    procedure PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
    procedure PaintPixel(X, Y: Word; IsInverted: Boolean);
    procedure PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);

    // methods related to text output
    procedure ClearInfoStrings;
    function DrawFontedText(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): TPoint;
    function AddVariableInfo(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): Boolean;
    procedure HandleTextOutput(AText: string; X, Y: Byte; FontName: string; FontSize: Integer);
    procedure RefreshTextOutputs;
    function SubstituteStaticInfo(AText: string): string;
    function SubstituteVariableInfo(AText: string): string;

    // methods related to usage monitors
    procedure UpdateUsageMonitors(IsCpuMonitorDisplayed, IsMemMonitorDisplayed: Boolean);
    procedure DrawCpuMonitor;
    procedure ConfigureCpuMonitor(NumRows, BottomRow: Integer);
    procedure DrawMemMonitor;
    procedure ConfigureMemMonitor(NumRows, BottomRow: Integer);
  end;

implementation

resourcestring
  { Day of the week }
  RsDoWSunday =    'Sunday';
  RsDoWMonday =    'Monday';
  RsDoWTuesday =   'Tuesday';
  RsDoWWednesday = 'Wednesday';
  RsDoWThursday =  'Thursday';
  RsDoWFriday =    'Friday';
  RsDoWSaturday =  'Saturday';

  { system information }
  RsInformationUnknown = '???';

constructor TDisplayManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGfxWidth:= 0;
  FGfxHeight:= 0;
  FTxtWidth:= 0;
  FTxtHeight:= 0;
  FSMBios:= TSMBios.Create;
  FSysInfo:= TSysInfo.Create(Self);
  FWinampControl:= TWinampControl.Create(Self);
end;

destructor TDisplayManager.Destroy;
begin
  FSysInfo.Free;
  FSMBios.Free;
  FWinampControl.Free;
  SetLength(FDisplays, 0);
  inherited; // Also called parent class destroyer
end;

procedure TDisplayManager.SetPreviewColor(const AColor: TColor);
var
  DspIdx: Integer;
begin
  FPreviewColor:= AColor;
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') then
      TPreviewDisplay(FDisplays[DspIdx]).DisplayColor:= FPreviewColor;
  end;
end;

function TDisplayManager.GetVariableInfoCount: Integer;
var
  I: Integer;
begin
  Result:= 0;
  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    if (FVariableInfo[I].Text <> '') then begin
      Inc(Result);
    end;
  end;
end;

function TDisplayManager.GetAverageCpuLoad: Byte;
begin
  Result:= FCpuUsageData.AverageCpuUsage;
end;



procedure TDisplayManager.TrimBitmap(var Bmp: TBitmap);
var
  X, Y: Integer;
  NumberOfWhiteLines: Integer;
  IsNonWhitePixelFound: Boolean;
begin
  // trim bottom
  // start from bottom and count all horizontal lines which are completely white
  NumberOfWhiteLines:= 0;
  for Y:= Bmp.Height - 1 downto 1 do begin // 'downto 1' and not 'downto 0' because the reaining image should be at least 1 pixel in height
    IsNonWhitePixelFound:= False;
    for X:= 0 to (Bmp.Width - 1) do begin
      if (Bmp.Canvas.Pixels[X, Y] <> clWhite) then begin
        IsNonWhitePixelFound:= True;
        Break; // abort X loop
      end;
    end; // for X
    if (IsNonWhitePixelFound) then begin
      Break; // abort Y loop
    end else begin
      Inc(NumberOfWhiteLines);
    end;
  end; // for Y
  Bmp.Height:= Bmp.Height - NumberOfWhiteLines;

  // trim right
  // start from right and count all vertical lines which are completely white
  NumberOfWhiteLines:= 0;
  for X:= Bmp.Width - 1 downto 1 do begin // 'downto 1' and not 'downto 0' because the reaining image should be at least 1 pixel in width
    IsNonWhitePixelFound:= False;
    for Y:= 0 to (Bmp.Height - 1) do begin
      if (Bmp.Canvas.Pixels[X, Y] <> clWhite) then begin
        IsNonWhitePixelFound:= True;
        Break; // abort Y loop
      end;
    end; // for Y
    if (IsNonWhitePixelFound) then begin
      Break; // abort X loop
    end else begin
      Inc(NumberOfWhiteLines);
    end;
  end; // for Y
  Bmp.Width:= Bmp.Width - NumberOfWhiteLines;
end;


procedure TDisplayManager.AddDisplay(DisplayConfig: TDisplayConfig);
const
  THIS_METHOD_NAME: String = 'AddDisplay';
begin
  if ((FGfxWidth > 0) and (DisplayConfig.ResX <> FGfxWidth)) or ((FGfxHeight > 0) and (DisplayConfig.ResY <> FGfxHeight)) then
    if (Assigned(FLoggingCallback)) then
      FLoggingCallback(lvERROR, Self.ClassName + '.' + THIS_METHOD_NAME + ': Display dimensions already set to ' + IntToStr(FGfxWidth) + 'x' + IntToStr(FGfxHeight), Now);

  FGfxWidth:= DisplayConfig.ResX;
  FGfxHeight:= DisplayConfig.ResY;
  FTxtWidth:= FGfxWidth div (GLYPH_W + GLYPH_GAP);
  FTxtHeight:= FGfxHeight div GLYPH_H;

  if (DisplayConfig.DisplayType = 'PREVIEW') then
  begin
    if (Assigned(FLoggingCallback)) then
      FLoggingCallback(lvINFO, Self.ClassName + '.' + THIS_METHOD_NAME + ': Display type: ' + DisplayConfig.DisplayType, Now);
    SetLength(FDisplays, Length(FDisplays) + 1);
    FDisplays[High(FDisplays)]:= TPreviewDisplay.Create(Self);
    FDisplays[High(FDisplays)].OnDbgMessage:= FLoggingCallback;
    FDisplays[High(FDisplays)].DspInit(DisplayConfig.ResX, DisplayConfig.ResY);
    TPreviewDisplay(FDisplays[High(FDisplays)]).DisplayColor:= FPreviewColor;
  end
  else
  if (DisplayConfig.DisplayType = 'NTK300') then
  begin
    if (Assigned(FLoggingCallback)) then
      FLoggingCallback(lvINFO, Self.ClassName + '.' + THIS_METHOD_NAME + ': Display type: ' + DisplayConfig.DisplayType, Now);
    SetLength(FDisplays, Length(FDisplays) + 1);
    FDisplays[High(FDisplays)]:= TNTK300.Create(Self);
    FDisplays[High(FDisplays)].OnDbgMessage:= FLoggingCallback;
    FDisplays[High(FDisplays)].Connect(DisplayConfig.IntName);
    FDisplays[High(FDisplays)].DspInit(DisplayConfig.ResX, DisplayConfig.ResY);
  end
  else
  if (DisplayConfig.DisplayType = 'NTK800') then
  begin
    if (Assigned(FLoggingCallback)) then
      FLoggingCallback(lvINFO, Self.ClassName + '.' + THIS_METHOD_NAME + ': Display type: ' + DisplayConfig.DisplayType, Now);
    SetLength(FDisplays, Length(FDisplays) + 1);
    FDisplays[High(FDisplays)]:= TNTK800.Create(Self);
    FDisplays[High(FDisplays)].OnDbgMessage:= FLoggingCallback;
    FDisplays[High(FDisplays)].Connect(DisplayConfig.IntName);
    FDisplays[High(FDisplays)].DspInit(DisplayConfig.ResX, DisplayConfig.ResY);
  end
  else
  begin
    if (Assigned(FLoggingCallback)) then
      FLoggingCallback(lvERROR, Self.ClassName + '.' + THIS_METHOD_NAME + ': Unsupported display type: ' + DisplayConfig.DisplayType, Now);
  end;
end;

procedure TDisplayManager.AddDisplay(DisplayType: String; ResX, ResY: Word; IntName: String; Baudrate: Cardinal);
var
  DisplayConfig: TDisplayConfig;
begin
  DisplayConfig.DisplayType:= DisplayType;
  DisplayConfig.ResX:= ResX;
  DisplayConfig.ResY:= ResY;
  DisplayConfig.IntName:= IntName;
  DisplayConfig.Baudrate:= Baudrate;
  AddDisplay(DisplayConfig);
end;

procedure TDisplayManager.ClearScreen;
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].ClearScreen;
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.ShowScreen(ALayer: Word);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].ShowScreen(ALayer);
  end;
end;

procedure TDisplayManager.SetBrightness(Percent: Byte);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].SetBrightness(Percent);
  end;
end;

procedure TDisplayManager.SetLayerMode(LayerMode: TLayerMode);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then begin
      FDisplays[DspIdx].SetLayerMode(LayerMode);
      if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
        FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
    end;
  end;
end;

procedure TDisplayManager.PaintString(AText: string; Col, Row: Integer);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintString(AText, Col, Row);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.PaintPixel(X, Y: Word; IsInverted: Boolean);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintPixel(X, Y, IsInverted);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintLine(X0, Y0, X1, Y1, IsInverted);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintFrame(X0, Y0, X1, Y1, IsInverted);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.PaintBitmap(ABitmap: TBitmap; X, Y: Word);
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintBitmap(ABitmap, X, Y);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;
end;

procedure TDisplayManager.PaintBitmapFromFile(FileName: string; X, Y: Word);
var
  TmpBitmap: TBitmap;
  DspIdx: Integer;
begin
  TmpBitmap:= TBitmap.Create;
  TmpBitmap.LoadFromFile(FileName);
  //LogEvent(lvINFO, 'Loading bitmap. Size ' + IntToStr(TmpBitmap.Width) + 'x' + IntToStr(TmpBitmap.Height), Now);

  // clip bitmap to display if needed
  if (TmpBitmap.Width + X >= FGfxWidth) then
    TmpBitmap.Width:= TmpBitmap.Width - (TmpBitmap.Width + X - FGfxWidth);
  if (TmpBitmap.Height + Y >= FGfxHeight) then
    TmpBitmap.Height:= TmpBitmap.Height - (TmpBitmap.Height + Y - FGfxHeight);

  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (Assigned(FDisplays[DspIdx])) then
      FDisplays[DspIdx].PaintBitmap(TmpBitmap, X, Y);
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
  end;

  TmpBitmap.Free;
end;


procedure TDisplayManager.ForcePreviewUpdate;
var
  DspIdx: Integer;
begin
  for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
    if (FDisplays[DspIdx].DisplayType = 'PREVIEW') and (Assigned(FOnPreviewChangedEvent)) then begin
      TPreviewDisplay(FDisplays[DspIdx]).CombineVirtualLayers;
      FOnPreviewChangedEvent(TPreviewDisplay(FDisplays[DspIdx]).CombinedBitmap);
    end;
  end;

end;




// clear info strings
procedure TDisplayManager.ClearInfoStrings;
var
  I: Integer;
begin
  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    FVariableInfo[I].Text:= '';
    FVariableInfo[I].SubsText:= '';
    FVariableInfo[I].PrevWidth:= 0;
    FVariableInfo[I].PrevHeight:= 0;
    FVariableInfo[I].FontSize:= 0;
    FVariableInfo[I].FontName:= '';
    FVariableInfo[I].X:= 0;
    FVariableInfo[I].Y:= 0;
  end;
end;

function TDisplayManager.DrawFontedText(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): TPoint;
var
  DspIdx: Integer;
  TmpBitmap: TBitmap;
  ResultPoint: TPoint;
begin
  ResultPoint:= Point(0 ,0);

  TmpBitmap:= TBitmap.Create;
  try
    TmpBitmap.Monochrome:= True;
    TmpBitmap.Canvas.Font.Color:= clblack;
    TmpBitmap.Canvas.Font.Name:= FontName;
    TmpBitmap.Canvas.Font.Size:= FontSize;
    TmpBitmap.Canvas.Font.Bold:= False;
    TmpBitmap.Canvas.Font.Italic:= False;
    TmpBitmap.SetSize(TmpBitmap.Canvas.TextWidth(AText), TmpBitmap.Canvas.TextHeight(AText));
    TmpBitmap.Canvas.AntialiasingMode:= amOff;
    TmpBitmap.Canvas.TextOut(0, 0, AText);
    //TrimBitmap(TmpBitmap);

    // clip bitmap to display if needed
    if (TmpBitmap.Width + X >= FGfxWidth) then
      TmpBitmap.Width:= TmpBitmap.Width - (TmpBitmap.Width + X - FGfxWidth);
    if (TmpBitmap.Height + Y >= FGfxHeight) then
      TmpBitmap.Height:= TmpBitmap.Height - (TmpBitmap.Height + Y - FGfxHeight);

    for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
      if (Assigned(FDisplays[DspIdx])) then
        FDisplays[DspIdx].PaintBitmap(TmpBitmap, X, Y);
    end;
    TmpBitmap.Canvas.Pixels[0, 0]:= TmpBitmap.Canvas.Pixels[0, 0]; // this seems like nonsense but is required to actually load the bitmap in memory
    ResultPoint.X:= TmpBitmap.Width;
    ResultPoint.Y:= TmpBitmap.Height;
  finally
    TmpBitmap.Free;
  end;

  Result:= ResultPoint;
end;

function TDisplayManager.AddVariableInfo(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): Boolean;
var
  I: Integer;
  IsAdded: Boolean;
begin

  // is there a free slot?
  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    if (FVariableInfo[I].Text = '' ) then begin
      // store it in the slot
      FVariableInfo[I].Text:=       AText;
      FVariableInfo[I].SubsText:=   '';
      FVariableInfo[I].X:=          X;
      FVariableInfo[I].Y:=          Y;
      FVariableInfo[I].FontName:=   FontName;
      FVariableInfo[I].FontSize:=   FontSize;
      FVariableInfo[I].PrevHeight:= 0;
      if ('' <> FontName) then
        FVariableInfo[I].PrevWidth:= Length(AText)
      else
        FVariableInfo[I].PrevWidth:= 0;

      IsAdded:= True;
      Break;
    end;
  end; // end for I

  if (I >= MAX_VARIABLE_INFO ) then begin
    // no free slot found
    IsAdded:= False;
  end;

  Result:= IsAdded;
end;


//Infos, die sich nicht ständig ändern
procedure TDisplayManager.HandleTextOutput(AText: string; X, Y: Byte; FontName: string; FontSize: Integer);
var
  S: string;
  IsFreeSlotFound: Boolean;
begin
  S:= SubstituteStaticInfo(AText);

  if (Pos('$', S) <> 0) then begin
    // if there is still a '$' in the string, it is either information which needs to be
    // redrawn continuously (or there is an unsupported keyword).

    // Add string to list of variable texts
    IsFreeSlotFound:= AddVariableInfo(S, X, Y, FontName, FontSize);

    if (False =  IsFreeSlotFound) then begin
      // no free slot found -> just print it on screen (without continuously updating it)
      if (Assigned(FLoggingCallback)) then
        FLoggingCallback(lvERROR, 'No more free slots to handle variable information. Text will be displayed as static text.', Now);

      if (FontName = '') or (FontSize = 0) then begin
        Self.PaintString(S, X, Y);
      end else begin
        Self.DrawFontedText(S, X, Y, FontName, FontSize);
      end;
    end;

  end else begin
    // no (more) '$' in the string -> its just some staic text
    if (FontName = '') or (FontSize = 0) then begin
      Self.PaintString(S, X, Y);
    end else begin
      Self.DrawFontedText(S, X, Y, FontName, FontSize);
    end;
  end;

  RefreshTextOutputs;

end;

//Infos, die sich ständig ändern
procedure TDisplayManager.RefreshTextOutputs;
var
  I: Integer;
  OldText: string;
  S: string;
  TextWidth: Integer;
  Overlength: Integer;
  Subs: string;
  p0: Integer;
  BmpDimensions: TPoint;
  DspIdx: Integer;
begin

  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    S:= FVariableInfo[I].Text;

    if ('' = S) then
      Continue;

    OldText:= FVariableInfo[I].SubsText;
    S:= SubstituteVariableInfo(S);

    TextWidth:= FTxtWidth - FVariableInfo[I].X;
    Overlength:= Length(S) - TextWidth;

    if ((Trim(S) <> Trim(OldText)) or (Overlength > 0)) then begin    // repainting is only required if the text has changed or if it has overlength

      //while (Length(S)<43 ) do S:= S + ' ';   // string bis Displayende mit Leerstellen füllen
      if (FVariableInfo[I].FontName = '') or (FVariableInfo[I].FontSize = 0) then begin
        if (Overlength > 0) then begin  // the text is to long to be displayed

          Overlength:= Overlength + 6; // we add 6 'virtual' character to the overlength to stay a little longer at the start and the end

          P0:= FScrollStringIndex mod Word(Overlength);
          if (P0 < 3) then
            P0:= 0
          else
            P0:= P0 - 3;

          if (P0 >= Overlength - 6) then
            P0:= Overlength - 6;

          Subs:= S.Substring(P0, TextWidth);
          FVariableInfo[I].PrevWidth:= Length(Subs);

          for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
            if (Assigned(FDisplays[DspIdx])) then
              FDisplays[DspIdx].PaintString(Subs, FVariableInfo[I].X, FVariableInfo[I].Y);
          end;
        end else begin
          while (FVariableInfo[I].PrevWidth > Length(S)) do begin
           S:= S + ' '; // if the text was longer previously, we need to add whitespaces to clear the remainings of the previous text
          end;
          FVariableInfo[I].PrevWidth:= Length(S);
          for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
            if (Assigned(FDisplays[DspIdx])) then
              FDisplays[DspIdx].PaintString(S, FVariableInfo[I].X, FVariableInfo[I].Y);
          end;
        end;
      end else begin // it is text with font
        BmpDimensions:= DrawFontedText(S, FVariableInfo[I].X, FVariableInfo[I].Y, FVariableInfo[I].FontName, FVariableInfo[I].FontSize);
        if (BmpDimensions.X < FVariableInfo[I].PrevWidth) then begin
          // the previous text was longer
          // TODO
        end;
        if (BmpDimensions.Y < FVariableInfo[I].PrevHeight) then begin
          // the previous text was higher
          // TODO
        end;
        FVariableInfo[I].PrevWidth:= BmpDimensions.X;
        FVariableInfo[I].PrevHeight:= BmpDimensions.Y;
      end;
    end;

    FVariableInfo[I].SubsText:= S;
  end;  //for I

  Inc(FScrollStringIndex);

end;



function TDisplayManager.SubstituteStaticInfo(AText: string): string;
var
  I: Integer;
  Tmp: string;
  S: string;
  RegEx: TRegExpr;
  Match1: string;
  Match2: string;
begin

  RegEx:= TRegExpr.Create;
  S:= AText;

  if (Pos('$REGKEY_', S) <> 0 ) then begin
    RegEx.Expression:= '\$REGKEY_(.+)\/(.+)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      Match2:=RegEx.Match[2];
      S:= RegEx.Replace(S, FSysInfo.GetRegistryString(Match1, Match2), False);
    end;
  end;

  if (Pos('$SLOTUSAGE', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTUSAGE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].GetCurrentUsage, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

  if (Pos('$SLOTTYPE', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTTYPE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].GetSlotType, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

  if (Pos('$SLOTINFO', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTINFO(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].SlotDesignationStr, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

  while (Pos('$VERSION$', S) <> 0 ) do begin
    I:= Pos('$VERSION$', S);
    Delete(S, I, Length('$VERSION$'));
    Insert(FSysInfo.ResourceVersionInfo, S, I);
  end;

  while (Pos('$PCNAME$', S) <> 0 ) do begin
    I:= Pos('$PCNAME$', S);
    Delete(S, I, Length('$PCNAME$'));
    Insert(FSysInfo.Computername, S, I);
  end;

  while (Pos('$USERNAME$', S) <> 0 ) do begin
    I:= Pos('$USERNAME$', S);
    Delete(S, I, Length('$USERNAME$'));
    Insert(FSysInfo.GetCurrentUserName, S, I);
  end;

  while (Pos('$OS$', S) <> 0 ) do begin
    I:= Pos('$OS$', S);
    Delete(S, I, Length('$OS$'));
    Insert(FSysInfo.GetOperatingSystem, S, I);
  end;

  while (Pos('$OSNAME$', S) <> 0 ) do begin
    I:= Pos('$OSNAME$', S);
    Delete(S, I, Length('$OSNAME$'));
    Insert(FSysInfo.GetOperatingSystemFullName, S, I);
  end;

  while (Pos('$CPUCORES$', S) <> 0 ) do begin
    I:= Pos('$CPUCORES$', S);
    Delete(S, I, Length('$CPUCORES$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.CoreCount), S, I);
    end else begin
      Insert('?', S, I);
    end;
  end;

  while (Pos('$CPUMAX$', S) <> 0 ) do begin
    I:= Pos('$CPUMAX$', S);
    Delete(S, I, Length('$CPUMAX$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.MaxSpeed), S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$CPUVENDOR$', S) <> 0 ) do begin
    I:= Pos('$CPUVENDOR$', S);
    Delete(S, I, Length('$CPUVENDOR$'));
    Insert(FSysInfo.GetCPUVendorIdentifier, S, I);
  end;

  while (Pos('$CPUSOCKET$', S) <> 0 ) do begin
    I:= Pos('$CPUSOCKET$', S);
    Delete(S, I, Length('$CPUSOCKET$'));
    if (FSMBios.HasProcessorInfo) then begin
     Insert(FSMBios.ProcessorInfo[0].SocketDesignationStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$CPUNAME$', S) <> 0 ) do begin
    I:= Pos('$CPUNAME$', S);
    Delete(S, I, Length('$CPUNAME$'));
    Tmp:= FSysInfo.GetCPUName;
    Tmp:= Tmp.Replace('®', '').Replace('(R)', '').Replace('(TM)', '');
    Insert(Tmp, S, I);
  end;

  while (Pos('$CPUIDENT$', S) <> 0 ) do begin
    I:= Pos('$CPUIDENT$', S);
    Delete(S, I, Length('$CPUIDENT$'));
    Insert(FSysInfo.GetCPUIdentifier, S, I);
  end;

  while (Pos('$CPUFAMILY$', S) <> 0 ) do begin
    I:= Pos('$CPUFAMILY$', S);
    Delete(S, I, Length('$CPUFAMILY$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.ProcessorInfo[0].ProcessorFamilyStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$TIMEZONE$', S) <> 0 ) do begin
    I:= Pos('$TIMEZONE$', S);
    Delete(S, I, Length('$TIMEZONE$'));
    Insert(FSysInfo.GetTimeZone, S, I);
  end;

  while (Pos('$RECENTDD$', S) <> 0 ) do begin
    I:= Pos('$RECENTDD$', S);
    Delete(S, I, Length('$RECENTDD$'));
    Insert(FSysInfo.GetMostRecentDirectDraw, S, I);
  end;

  while (Pos('$RECENTD3D$', S) <> 0 ) do begin
    I:= Pos('$RECENTD3D$', S);
    Delete(S, I, Length('$RECENTD3D$'));
    Insert(FSysInfo.GetMostRecentDirect3D, S, I);
  end;


  while (Pos('$BIOSVENDOR$', S) <> 0 ) do begin
    I:= Pos('$BIOSVENDOR$', S);
    Delete(S, I, Length('$BIOSVENDOR$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.VendorStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSVERSION$', S) <> 0 ) do begin
    I:= Pos('$BIOSVERSION$', S);
    Delete(S, I, Length('$BIOSVERSION$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSDATE$', S) <> 0 ) do begin
    I:= Pos('$BIOSDATE$', S);
    Delete(S, I, Length('$BIOSDATE$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.ReleaseDateStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVENDOR$', S) <> 0 ) do begin
    I:= Pos('$BOARDVENDOR$', S);
    Delete(S, I, Length('$BOARDVENDOR$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].ManufacturerStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDPRODUCT$', S) <> 0 ) do begin
    I:= Pos('$BOARDPRODUCT$', S);
    Delete(S, I, Length('$BOARDPRODUCT$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].ProductStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVERSION$', S) <> 0 ) do begin
    I:= Pos('$BOARDVERSION$', S);
    Delete(S, I, Length('$BOARDVERSION$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  RegEx.Free;

  Result:= S;
end;


function TDisplayManager.SubstituteVariableInfo(AText: string): string;
var
  I: Integer;
  S: string;
  Sec: string;  //Seconds (Winamp)
  CurrentDateTime: TDateTime;
  DoW: Word;
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
  RegEx: TRegExpr;
  Match: string;
begin

  RegEx:= TRegExpr.Create;
  S:= AText;
  CurrentDateTime:= Now;
  DecodeDateTime(CurrentDateTime, Year, Month, Day, Hour, Minute, Second, MilliSecond);

  if (Pos('$TOTALDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$TOTALDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, IntToStr(Round(FSysInfo.GetDiskSpace(Match[1]) / 1073741824)), False);
    end;
  end;

  if (Pos('$FREEDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$FREEDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, IntToStr(Round(FSysInfo.GetFreeDiskSpace(Match[1]) / 1073741824)), False);
    end;
  end;

  if (Pos('$NAMEDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$NAMEDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, FSysInfo.GetPartitionName(Match[1]),  False);
    end;
  end;

  while (Pos('$MEMORY$', S) <> 0 ) do begin
    I:= Pos('$MEMORY$', S);
    Delete(S, I, Length('$MEMORY$'));
    Insert(IntToStr(FMemUsageData.PhysicalMemory div 1048576), S, I);
  end;

  while (Pos('$FREEMEM$', S) <> 0 ) do begin
    I:= Pos('$FREEMEM$', S);
    Delete(S, I, Length('$FREEMEM$'));
    Insert(IntToStr(FMemUsageData.FreeMemory div 1048576), S, I);
  end;

  while (Pos('$MEMUSAGE$', S) <> 0 ) do begin
    I:= Pos('$MEMUSAGE$', S);
    Delete(S, I, Length('$MEMUSAGE$'));
    Insert(IntToStr(FMemUsageData.CurrentMemUsage), S, I);
  end;

  while (Pos('$AVERAGECPU$', S) <> 0 ) do begin
    I:= Pos('$AVERAGECPU$', S);
    Delete(S, I, Length('$AVERAGECPU$'));
    Insert(IntToStr(FCpuUsageData.AverageCpuUsage), S, I);
  end;

  while (Pos('$CPUUSAGE$', S) <> 0 ) do begin
    I:= Pos('$CPUUSAGE$', S);
    Delete(S, I, Length('$CPUUSAGE$'));
    Insert(IntToStr(FCpuUsageData.CurrentCpuUsage), S, I);
  end;

  while (Pos('$IP$', S) <> 0 ) do begin
    I:= Pos('$IP$', S);
    Delete(S, I, Length('$IP$'));
    Insert(FSysInfo.GetIPAdress, S, I);
  end;

  while (Pos('$UPTIME$', S) <> 0 ) do begin
    I:= Pos('$UPTIME$', S);
    Delete(S, I, Length('$UPTIME$'));
    Insert(FSysInfo.GetUptime, S , I);
  end;

  while (Pos('$CPUSPEED$', S) <> 0 ) do begin
    I:= Pos('$CPUSPEED$', S);
    Delete(S, I, Length('$CPUSPEED$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.CurrentSpeed), S, I);
    end else begin
      Insert(FSysInfo.GetCPUSpeed, S, I);
    end;
  end;

  while (Pos('$RES$', S) <> 0 ) do begin
    I:= Pos('$RES$', S);
    Delete(S, I, Length('$RES$'));
    Insert(FSysInfo.GetDesktopResolution, S, I);
  end;

  while (Pos('$COLORS$', S) <> 0 ) do begin
    I:= Pos('$COLORS$', S);
    Delete(S, I, Length('$COLORS$'));
    Insert(FSysInfo.GetDesktopColors, S, I);
  end;

  while (Pos('$FREQ$', S) <> 0 ) do begin
    I:= Pos('$FREQ$', S);
    Delete(S, I, Length('$FREQ$'));
    Insert(FSysInfo.GetScreenFrequency, S, I);
  end;

  while (Pos('$WAVERSION$', S) <> 0 ) do begin
    I:= Pos('$WAVERSION$', S);
    Delete(S, I, Length('$WAVERSION$'));
    if (FWinampControl.IsWinampRunning ) then
      Insert(FWinampControl.GetVersion, S, I)
    else
      Insert(' - --', S, I);
  end;

  while (Pos('$WATITLE$', S) <> 0 ) do begin
    I:= Pos('$WATITLE$', S);
    Delete(S, I, Length('$WATITLE$'));
    if (FWinampControl.IsWinampRunning ) then
      Insert(FWinampControl.GetTrackTitle, S, I)
    else
      Insert(' - --', S, I);
  end;

  while (Pos('$WALENGTH$', S) <> 0 ) do begin
    I:= Pos('$WALENGTH$', S);
    Delete(S, I, Length('$WALENGTH$'));
    if (FWinampControl.IsWinampRunning ) then begin
      Sec:= IntToStr(FWinampControl.GetLength mod 60);
      if (Length(Sec) < 2) then
        Sec:= '0' + Sec;
      Insert(IntToStr(FWinampControl.GetLength div 60) + ':' + Sec, S, I)
    end else
      Insert('0:00', S, I);
  end;

  while (Pos('$WAPOS$', S) <> 0 ) do begin
    I:= Pos('$WAPOS$', S);
    Delete(S, I, Length('$WAPOS$'));
    if (FWinampControl.IsWinampRunning ) then begin
      Sec:= IntToStr( (FWinampControl.GetOffset div 1000000) mod 60);
      if (Length(Sec) < 2 ) then
        Sec:= '0' + Sec;
      Insert(IntToStr( (FWinampControl.GetOffset div 1000000) div 60) + ':' + Sec, S, I)
    end else
      Insert('0:00', S, I);
  end;

  while (Pos('$DOW$', S) <> 0 ) do begin
    I:= Pos('$DOW$', S);
    Delete(S, I, Length('$DOW$'));
    DoW:= DayOfWeek(CurrentDateTime);
    case DoW of
     1: Insert(RsDoWSunday,    S, I);
     2: Insert(RsDoWMonday,    S, I);
     3: Insert(RsDoWTuesday,   S, I);
     4: Insert(RsDoWWednesday, S, I);
     5: Insert(RsDoWThursday,  S, I);
     6: Insert(RsDoWFriday,    S, I);
     7: Insert(RsDoWSaturday,  S, I);
    end;
  end;

  while (Pos('$DATE$', S) <> 0 ) do begin
    I:= Pos('$DATE$', S);
    Delete(S, I, Length('$DATE$'));
    Insert(DateToStr(CurrentDateTime), S, I);
  end;

  while (Pos('$YEAR$', S) <> 0 ) do begin
    I:= Pos('$YEAR$', S);
    Delete(S, I, Length('$YEAR$'));
    Insert(Format('%.04d', [Year]), S, I);
  end;

  while (Pos('$MON$', S) <> 0 ) do begin
    I:= Pos('$MON$', S);
    Delete(S, I, Length('$MON$'));
    Insert(Format('%.02d', [Month]), S, I);
  end;

  while (Pos('$DAY$', S) <> 0 ) do begin
    I:= Pos('$DAY$', S);
    Delete(S, I, Length('$DAY$'));
    Insert(Format('%.02d', [Day]), S, I);
  end;

  while (Pos('$TIME$', S) <> 0 ) do begin
    I:= Pos('$TIME$', S);
    Delete(S, I, Length('$TIME$'));
    Insert(TimeToStr(CurrentDateTime), S, I);
  end;

  while (Pos('$HH$', S) <> 0 ) do begin
    I:= Pos('$HH$', S);
    Delete(S, I, Length('$HH$'));
    Insert(Format('%.02d', [Hour]), S, I);
  end;

  while (Pos('$MM$', S) <> 0 ) do begin
    I:= Pos('$MM$', S);
    Delete(S, I, Length('$MM$'));
    Insert(Format('%.02d', [Minute]), S, I);
  end;

  while (Pos('$SS$', S) <> 0 ) do begin
    I:= Pos('$SS$', S);
    Delete(S, I, Length('$SS$'));
    Insert(Format('%.02d', [Second]), S, I);
  end;

  while (Pos('$:$', S) <> 0 ) do begin
    I:= Pos('$:$', S);
    Delete(S, I, Length('$:$'));
    if (0 <> (Second mod 2)) then
      Insert(' ', S, I)
    else
      Insert(':', S, I);
  end;

  RegEx.Free;

  Result:= S;
end;

procedure TDisplayManager.UpdateUsageMonitors(IsCpuMonitorDisplayed, IsMemMonitorDisplayed: Boolean);
var
  CpuUsage: Byte;
  I: Integer;
  AvgValue: Single;
  TextWidth: Integer;
  PhysMem, FreeMem: QWord;
  MemUsage: DWord;
begin
  if Assigned(FSysInfo) then begin

    TextWidth:= FTxtWidth;

    // ---- CPU usage ------

    FSysInfo.UpdateCpuUsage;
    CpuUsage:= Round(FSysInfo.GetCpuUsage * 100.0);
    FCpuUsageData.CurrentCpuUsage:= CpuUsage;

    // for testing: CPUUsage:= Random(101);

    if (Length(FCpuUsageData.UsageHistory) < TextWidth) then begin
      // if the array length is < than the text width of the display, then add a new array element
      SetLength(FCpuUsageData.UsageHistory, Length(FCpuUsageData.UsageHistory) + 1);
    end else begin
      // otherwise shift all values left by one
      for I:= 0 to High(FCpuUsageData.UsageHistory) - 1 do
        FCpuUsageData.UsageHistory[I]:= FCpuUsageData.UsageHistory[I + 1];
    end;

    // write the new value to the last index
    FCpuUsageData.UsageHistory[High(FCpuUsageData.UsageHistory)]:= CpuUsage;

    // calculate new average
    AvgValue:= 0.0;
    for I:= 0 to High(FCpuUsageData.UsageHistory) do
      AvgValue:= AvgValue + FCpuUsageData.UsageHistory[I] / Length(FCpuUsageData.UsageHistory);
    FCpuUsageData.AverageCpuUsage:= Round(AvgValue);

    if (IsCpuMonitorDisplayed) then begin
      DrawCpuMonitor;
    end;

    // ---- Memory usage ------
    PhysMem:= FSysInfo.GetTotalMemory;
    FreeMem:= FSysInfo.GetFreeMemory;
    MemUsage:= Min(100, FSysInfo.GetMemoryUsage);

    FMemUsageData.CurrentMemUsage:= MemUsage;
    FMemUsageData.FreeMemory:= FreeMem;
    FMemUsageData.PhysicalMemory:= PhysMem;

    if (Length(FMemUsageData.UsageHistory) < TextWidth) then begin
      // if the array length is < than the text width of the display, then add a new array element
      SetLength(FMemUsageData.UsageHistory, Length(FMemUsageData.UsageHistory) + 1);
    end else begin
      // otherwise shift all values left by one
      for I:= 0 to High(FMemUsageData.UsageHistory) - 1 do
        FMemUsageData.UsageHistory[I]:= FMemUsageData.UsageHistory[I + 1];
    end;

    // write the new value to the last index
    FMemUsageData.UsageHistory[High(FMemUsageData.UsageHistory)]:= MemUsage;

    // calculate new average
    AvgValue:= 0.0;
    for I:= 0 to High(FMemUsageData.UsageHistory) do
      AvgValue:= AvgValue + FMemUsageData.UsageHistory[I] / Length(FMemUsageData.UsageHistory);
    FMemUsageData.AverageMemUsage:= Round(AvgValue);

    if (IsMemMonitorDisplayed) then begin
      DrawMemMonitor;
    end;

  end; // if FSysInfo is assigned

end;



procedure TDisplayManager.DrawCpuMonitor;
var
  I: Integer;
  DspIdx: Integer;
  Tmp: Integer;
  CpuUsage: Byte;
  C: Char;
  Row: Integer;
  IntervalSize, LowerBound, UpperBound: Byte;
begin
  if (Length(FCpuUsageData.UsageHistory) > 0) then begin
    for I:= High(FCpuUsageData.UsageHistory) downto 0 do begin
      CpuUsage:= FCpuUsageData.UsageHistory[I];

      IntervalSize:= 100 div FCpuUsageData.NumRows;

      for Row:= 0 to (FCpuUsageData.NumRows - 1) do begin
        // specify boundaries of this row
        LowerBound:= IntervalSize * Row;
        UpperBound:= LowerBound + IntervalSize - 1;

        // compare boundaries with CpuUsage
        if (CpuUsage < LowerBound) then
          C:= ' '
        else if (CpuUsage > UpperBound) then
          C:= Chr($87)
        else begin
          Tmp:= Trunc((CpuUsage - LowerBound) / IntervalSize * GLYPH_H);
          Tmp:= $80 + Tmp;
          C:= Chr(Tmp);
        end;

        for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
          if (Assigned(FDisplays[DspIdx])) then
            FDisplays[DspIdx].PaintString(C, I, FCpuUsageData.BottomRow - Row);
        end;

      end; // for Row

    end; // end for loop
  end;
end;

procedure TDisplayManager.ConfigureCpuMonitor(NumRows, BottomRow: Integer);
begin
  FCpuUsageData.NumRows:= NumRows;
  FCpuUsageData.BottomRow:= BottomRow;
end;

procedure TDisplayManager.DrawMemMonitor;
var
  I: Integer;
  DspIdx: Integer;
  Tmp: Integer;
  MemUsage: Byte;
  C: Char;
  Row: Integer;
  IntervalSize, LowerBound, UpperBound: Byte;
begin
  if (Length(FMemUsageData.UsageHistory) > 0) then begin
    for I:= High(FMemUsageData.UsageHistory) downto 0 do begin
      MemUsage:= FMemUsageData.UsageHistory[I];

      IntervalSize:= 100 div FMemUsageData.NumRows;

      for Row:= 0 to (FMemUsageData.NumRows - 1) do begin
        // specify boundaries of this row
        LowerBound:= IntervalSize * Row;
        UpperBound:= LowerBound + IntervalSize - 1;

        // compare boundaries with MemUsage
        if (MemUsage < LowerBound) then
          C:= ' '
        else if (MemUsage > UpperBound) then
          C:= Chr($87)
        else begin
          Tmp:= Trunc((MemUsage - LowerBound) / IntervalSize * GLYPH_H);
          Tmp:= $80 + Tmp;
          C:= Chr(Tmp);
        end;

        for DspIdx:= Low(FDisplays) to High(FDisplays) do begin
          if (Assigned(FDisplays[DspIdx])) then
            FDisplays[DspIdx].PaintString(C, I, FMemUsageData.BottomRow - Row);
        end;

      end; // for Row

    end; // end for loop
  end;
end;

procedure TDisplayManager.ConfigureMemMonitor(NumRows, BottomRow: Integer);
begin
  FMemUsageData.NumRows:= NumRows;
  FMemUsageData.BottomRow:= BottomRow;
end;



end.
