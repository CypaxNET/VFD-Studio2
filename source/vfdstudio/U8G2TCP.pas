unit U8G2TCP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, VFDisplay, Graphics, Math, Glyphs,
  GraphUtil, StudioCommon, blcksock, synsock;

type
  TGlyphConfig = record
    GlyphTable: array[0..255] of PChar;
    Width: Byte;
    Height: Byte;
    Gap: Byte;
    CurrentRow: Byte;
    CurrentCol: Byte;
  end;

  { TTCPAsyncSenderThread - Asynchroner Sender-Thread }
  TTCPAsyncSenderThread = class(TThread)
  private
    FTCP: TTCPBlockSocket;
    FCommandQueue: TThreadList;
    FActive: Boolean;
    FOnError: TNotifyEvent;
    FHost: String;
    FPort: String;
    procedure CallErrorHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(AHost, APort: String);
    destructor Destroy; override;
    procedure EnqueueCommand(const ACommand: String);
    procedure FlushCommands;
    procedure Stop;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  { U8G2UDP }
  TU8G2TCP = class(TVFDisplay)
  private
    FSenderThread: TTCPAsyncSenderThread;
    procedure HandleThreadError(Sender: TObject);
  protected
    FTCP: TTCPBlockSocket;
    FPosX, FPosY: Word;
    FNumBytesSent: Cardinal;
    FDbgLastSent: String;
    FGlyphConfig: TGlyphConfig;         // all data related to glyphs
    FHost: String;
    FPort: String;
  public
    { Constructor / Destructor }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Overloaded methods }
    procedure Connect(AInterface: String); override;
    procedure DspInit(XRes, YRes: Word); override;
    procedure ClearScreen; override;
    procedure ShowScreen(ALayer: Word); override;
    procedure PaintString(Text: String; X, Y: Integer); override;
    procedure PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word); override;
    procedure PaintPixel(X, Y: Word; IsInverted: Boolean); override;
    procedure PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean); override;
    procedure PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean); override;
    procedure SetBrightness(Percent: Byte); override;
    procedure SetLayerMode(LayerMode: TLayerMode); override;
    procedure Dbg(Value: Byte); override;

    { Own display methods }
    procedure UpdateDisplayFromBuffer;

    { Other / helper methods }
    function removeLeadingZeros(Text: String): String;
    procedure SelectScreen(ALayer: Word);
    procedure TcpOut(Text: String);
  end;

const
  CURSORGLYPH: array[0..5] of Byte = ($FF, $FF, $FF, $FF, $FF, $FF);
  VFD_BLACK = 0;
  VFD_WHITE = 1;

implementation

{ TTCPAsyncSenderThread }

constructor TTCPAsyncSenderThread.Create(AHost, APort: String);
begin
  inherited Create(True);
  FHost := AHost;
  FPort := APort;
  FCommandQueue := TThreadList.Create;
  FActive := True;
  FreeOnTerminate := False;

  // TCP Socket erstellen
  FTCP := TTCPBlockSocket.Create;
  FTCP.Family := SF_IP4;
  FTCP.Connect(FHost, FPort);
end;

destructor TTCPAsyncSenderThread.Destroy;
var
  List: TList;
  I: Integer;
begin
  FActive := False;

  // Leere Queue aufräumen
  List := FCommandQueue.LockList;
  try
    for I := 0 to List.Count - 1 do
      Dispose(PString(List[I]));
    List.Clear;
  finally
    FCommandQueue.UnlockList;
  end;

  FCommandQueue.Free;

  if Assigned(FTCP) then
    FTCP.Free;

  inherited;
end;

procedure TTCPAsyncSenderThread.EnqueueCommand(const ACommand: String);
var
  Cmd: PString;
  List: TList;
begin
  New(Cmd);
  Cmd^ := ACommand;

  List := FCommandQueue.LockList;
  try
    List.Add(Cmd);
  finally
    FCommandQueue.UnlockList;
  end;
end;

procedure TTCPAsyncSenderThread.CallErrorHandler;
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

procedure TTCPAsyncSenderThread.Execute;
var
  List: TList;
  Cmd: PString;
  CmdText: String;
begin
  while FActive and (not Terminated) do
  begin
    Cmd := nil;

    List := FCommandQueue.LockList;
    try
      if List.Count > 0 then
      begin
        Cmd := PString(List[0]);
        List.Delete(0);
      end;
    finally
      FCommandQueue.UnlockList;
    end;

    if Assigned(Cmd) then
    begin
      try
        CmdText := Cmd^ + #10;
        FTCP.SendString(CmdText);

        if FTCP.LastError <> 0 then
          Synchronize(@CallErrorHandler);
      except
        on E: Exception do
          Synchronize(@CallErrorHandler);
      end;

      Dispose(Cmd);
    end
    else
      Sleep(0);

    Sleep(0);
  end;
end;

procedure TTCPAsyncSenderThread.FlushCommands;
var
  List: TList;
begin
  List := FCommandQueue.LockList;
  try
    List.Clear;
  finally
    FCommandQueue.UnlockList;
  end;
end;

procedure TTCPAsyncSenderThread.Stop;
begin
  FActive := False;
  Terminate;
end;

{ TU8G2TCP }

constructor TU8G2TCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDisplayType := 'U8G2TCP';
  FNumBytesSent := 0;
  FDbgLastSent := '';
  FInterfaceConfig.IfaceType := itNONE;
  FNumLayers := 1;

  FGlyphConfig.Width := GLYPH_W;
  FGlyphConfig.Height := GLYPH_H;
  FGlyphConfig.gap := GLYPH_GAP;

  FSelectedLayer := 0;
  FSenderThread := nil;
  FTCP := nil;
end;

destructor TU8G2TCP.Destroy;
begin
  if Assigned(FSenderThread) then
  begin
    FSenderThread.Stop;
    FSenderThread.WaitFor;
    FSenderThread.Free;
  end;

  if Assigned(FTCP) then
    FTCP.Free;

  inherited;
end;

procedure TU8G2TCP.HandleThreadError(Sender: TObject);
begin
  FInterfaceConfig.IfaceType := itNONE;

  if Assigned(FLoggingCallback) then
    FLoggingCallback(lvERROR, Self.ClassName + ': TCP communication error in async thread', Now);

  if Assigned(FConnectionFailureCallback) then
    FConnectionFailureCallback('AsyncCommError', Now);
end;

procedure TU8G2TCP.Connect(AInterface: String);
var
  Delimiter: Integer;
begin
  // Format erwartet: "192.168.0.114:8888"
  Delimiter := Pos(':', AInterface);

  if Delimiter > 0 then
  begin
    FHost := Copy(AInterface, 1, Delimiter - 1);
    FPort := Copy(AInterface, Delimiter + 1, Length(AInterface));

    FInterfaceConfig.IfaceType := itTCP;
    try
      FTCP := TTCPBlockSocket.Create;
      FTCP.Family := SF_IP4;
      FTCP.Connect(FHost, FPort);

      if FTCP.LastError = 0 then
      begin
        FInterfaceConfig.IsConnected := True;
        FInterfaceConfig.IfaceName := AInterface;

        FSenderThread := TTCPAsyncSenderThread.Create(FHost, FPort);
        FSenderThread.OnError := @HandleThreadError;
        FSenderThread.Start;

        if Assigned(FLoggingCallback) then
          FLoggingCallback(lvINFO, Self.ClassName + '.Connect: TCP connected to ' + AInterface, Now);
      end
      else
        raise Exception.Create('TCP Socket Error: ' + IntToStr(FTCP.LastError));

    except
      on E: Exception do
      begin
        FInterfaceConfig.IsConnected := False;
        FInterfaceConfig.IfaceType := itNONE;
        if Assigned(FLoggingCallback) then
          FLoggingCallback(lvCRITICAL, Self.ClassName + '.Connect: Exception: ' + E.Message, Now);
      end;
    end;
  end
  else
  begin
    if Assigned(FLoggingCallback) then
      FLoggingCallback(lvERROR, Self.ClassName + '.Connect: Invalid format. Expected "IP:Port"', Now);
  end;
end;

procedure TU8G2TCP.TcpOut(Text: String);
begin
  if not Assigned(FSenderThread) then
    Exit;

  // Befehl in Queue einreihen (nicht-blockierend)
  FSenderThread.EnqueueCommand(Text);

  FNumBytesSent := FNumBytesSent + Length(Text) + 1; // +1 für #10

  FDbgLastSent := FDbgLastSent + Text.Replace(#10, 'n');
  if Length(FDbgLastSent) > 80 then
    FDbgLastSent := RightStr(FDbgLastSent, 80);
end;


procedure TU8G2TCP.Dbg(Value: Byte);
begin
  // DBG here
end;

procedure TU8G2TCP.ClearScreen;
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    if Assigned(FSenderThread) then
      FSenderThread.FlushCommands; // discard all queued commands in the list

    Cmd := 'X';
    TcpOut(Cmd);
  end;
end;

procedure TU8G2TCP.SelectScreen(ALayer: Word);
begin
  if (ALayer < FNumLayers) then
  begin
    FSelectedLayer := ALayer;
  end;
end;


procedure TU8G2TCP.ShowScreen(ALayer: Word);
begin
  // nothing to do, this display has only one layer
end;


procedure TU8G2TCP.PaintString(Text: String; X, Y: Integer);
var
  I: Integer;
  C: Char;
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    Text := TGlyphs.Adapt2Charmap(Text);
    for I := 1 to Length(Text) do begin
      C := Text[I];
      Cmd := 'L' + ' ' +
        removeLeadingZeros(IntToHex(Ord(C))) + ' ' +
        removeLeadingZeros(IntToHex(X + I-1)) + ' ' +
        removeLeadingZeros(IntToHex(Y));
      TcpOut(Cmd);
    end;
    UpdateDisplayFromBuffer;
  end;

end;

{
  Draws a bitmap on the display
}
procedure TU8G2TCP.PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word);
var
  X, Y: Integer;
  PixelColor: TColor;
  I: Integer;
  Pixels: Byte;
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    aBitmap.Canvas.Pixels[0, 0] := aBitmap.Canvas.Pixels[0, 0];  // this seems nonsense, but one way to actually assign memory to the bitmap canvas is by acessing its pixels

    for Y := 0 to Ceil(aBitmap.Height / 8) - 1 do begin

      if (YPos + Y*8) > FGfxHeight then
        Break;

      for X := 0 to aBitmap.Width-1 do begin

        if (XPos + X) > FGfxWidth then
          Break;

        // build a block of pixels
        Pixels := 0;
        for I := 0 to 7 do begin
          Pixels := Pixels shr 1;
          if ((Y*8 + I) < ABitmap.Height) then begin
            PixelColor := aBitmap.Canvas.Pixels[X, Y*8 + I];
            if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
            begin
              Pixels := Pixels or $80;
            end;
          end;
        end;

        // send the pixel block to the Arduino
        if (itTCP = FInterfaceConfig.IfaceType) then
        begin
          Cmd := 'B' + ' ' +
            removeLeadingZeros(IntToHex(Pixels)) + ' ' +
            removeLeadingZeros(IntToHex(XPos + X)) + ' ' +
            removeLeadingZeros(IntToHex(YPos + Y*8));
          TcpOut(Cmd);
        end;

      end;
    end;
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2TCP.PaintPixel(X, Y: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    if (IsInverted) then
      Cmd := 'CP'
    else
      Cmd := 'SP';
    Cmd := Cmd + ' ' +
      removeLeadingZeros(IntToHex(X)) + ' ' +
      removeLeadingZeros(IntToHex(Y));
    TcpOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2TCP.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    if (IsInverted) then
      Cmd := 'CL'
    else
      Cmd := 'SL';
    Cmd := Cmd + ' ' +
      removeLeadingZeros(IntToHex(X0)) + ' ' +
      removeLeadingZeros(IntToHex(Y0)) + ' ' +
      removeLeadingZeros(IntToHex(X1)) + ' ' +
      removeLeadingZeros(IntToHex(Y1));
    TcpOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;


procedure TU8G2TCP.PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    if (IsInverted) then
      Cmd := 'CF'
    else
      Cmd := 'SF';
    Cmd := Cmd + ' ' +
      removeLeadingZeros(IntToHex(X0)) + ' ' +
      removeLeadingZeros(IntToHex(Y0)) + ' ' +
      removeLeadingZeros(IntToHex(X1+1)) + ' ' +  // +1 because u8g2 lib wants the width as 3. param
      removeLeadingZeros(IntToHex(Y1+1));         // +1 because u8g2 lib wants the height as 4. param
    TcpOut(Cmd);
    UpdateDisplayFromBuffer;
  end;

end;

procedure TU8G2TCP.SetBrightness(Percent: Byte);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itTCP = FInterfaceConfig.IfaceType) then
  begin
    Cmd := 'T' + ' ' + removeLeadingZeros(IntToHex(Percent));
    TcpOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2TCP.SetLayerMode(LayerMode: TLayerMode);
begin
  // nothing to do here; display supports only one layer
end;

{
 Initializes class variables and starts the VFD.
}

procedure TU8G2TCP.DspInit(XRes, YRes: Word);
begin
  FGfxWidth := XRes;
  FGfxHeight := YRes;

  // calculate number of text columns and rows
  FTxtWidth := (FGfxWidth div (FGlyphConfig.Width + FGlyphConfig.Gap));
  FTxtHeight := (FGfxHeight div FGlyphConfig.Height);

end;


function TU8G2TCP.removeLeadingZeros(Text: String): String;
begin
  Result := Text;
  while Result.StartsWith('0') do
    Result := Result.Remove(0, 1);
  if Result = '' then
    Result := '0';
end;

{
  Request the Arduino to update the display
}
procedure TU8G2TCP.UpdateDisplayFromBuffer;
var
  Cmd: String;
begin
  Cmd := 'U';
  TcpOut(Cmd);
end;

end.
