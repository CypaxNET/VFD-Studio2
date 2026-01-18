unit U8G2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, VFDisplay, Graphics, Math, noresetsynaser, Glyphs,
  GraphUtil, StudioCommon;
type
  TGlyphConfig = record
    GlyphTable: array[0..255] of PChar;
    Width: Byte;
    Height: Byte;
    Gap: Byte;
    CurrentRow: Byte;
    CurrentCol: Byte;
  end;

type
  { U8G2 }
  TU8G2 = class(TVFDisplay)
  private

  protected

    FSerialInterface: TBlockSerialNoReset;

    FPosX, FPosY: Word;

    FNumBytesSent: Cardinal;
    FDbgLastSent: String;

    FGlyphConfig: TGlyphConfig;         // all data related to glyphs

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
    procedure SerialOut(Text: String);

  end;


const

  CURSORGLYPH: array[0..5] of Byte = ($FF, $FF, $FF, $FF, $FF, $FF);

  VFD_BLACK = 0;
  VFD_WHITE = 1;


implementation


constructor TU8G2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDisplayType := 'U8G2';

  FNumBytesSent := 0;
  FDbgLastSent := '';

  FInterfaceConfig.IfaceType := itNONE; // will be set during Connect()

  FNumLayers := 1; // this display has only one layer

  FGlyphConfig.Width := GLYPH_W;
  FGlyphConfig.Height := GLYPH_H;
  FGlyphConfig.gap := GLYPH_GAP;

  FSelectedLayer := 0;

end;

destructor TU8G2.Destroy;
begin
  if (nil <> FSerialInterface) then
  begin
    FSerialInterface.CloseSocket;
    FSerialInterface.Free;
  end;

  inherited; // Also called parent class destroyer
end;


procedure TU8G2.Dbg(Value: Byte);
begin
  // DGB here
end;

procedure TU8G2.ClearScreen;
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Cmd := 'X';
    SerialOut(Cmd);
  end;
end;

procedure TU8G2.SelectScreen(ALayer: Word);
begin
  if (ALayer < FNumLayers) then
  begin
    FSelectedLayer := ALayer;
  end;
end;


procedure TU8G2.ShowScreen(ALayer: Word);
begin
  // nothing to do, this display has only one layer
end;


procedure TU8G2.PaintString(Text: String; X, Y: Integer);
var
  I: Integer;
  C: Char;
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Text := TGlyphs.Adapt2Charmap(Text);
    for I := 1 to Length(Text) do begin
      C := Text[I];
      Cmd := 'L' + ' ' +
        removeLeadingZeros(IntToHex(Ord(C))) + ' ' +
        removeLeadingZeros(IntToHex(X + I-1)) + ' ' +
        removeLeadingZeros(IntToHex(Y));
      SerialOut(Cmd);
    end;
    UpdateDisplayFromBuffer;
  end;

end;

{
  Draws a bitmap on the display
}
procedure TU8G2.PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word);
var
  X, Y: Integer;
  PixelColor: TColor;
  I: Integer;
  Pixels: Byte;
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
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
        if (itCOM = FInterfaceConfig.IfaceType) then
        begin
          Cmd := 'B' + ' ' +
            removeLeadingZeros(IntToHex(Pixels)) + ' ' +
            removeLeadingZeros(IntToHex(XPos + X)) + ' ' +
            removeLeadingZeros(IntToHex(YPos + Y*8));
          SerialOut(Cmd);
        end;

      end;
    end;
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2.PaintPixel(X, Y: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    if (IsInverted) then
      Cmd := 'CP'
    else
      Cmd := 'SP';
    Cmd := Cmd + ' ' +
      removeLeadingZeros(IntToHex(X)) + ' ' +
      removeLeadingZeros(IntToHex(Y));
    SerialOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
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
    SerialOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;


procedure TU8G2.PaintFrame(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
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
    SerialOut(Cmd);
    UpdateDisplayFromBuffer;
  end;

end;

procedure TU8G2.SetBrightness(Percent: Byte);
var
  Cmd: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Cmd := 'T' + ' ' + removeLeadingZeros(IntToHex(Percent));
    SerialOut(Cmd);
    UpdateDisplayFromBuffer;
  end;
end;

procedure TU8G2.SetLayerMode(LayerMode: TLayerMode);
begin
  // nothing to do here; display supports only one layer
end;


procedure TU8G2.Connect(AInterface: String);
var
  RecvText: String;
begin
  if (AInterface.StartsWith('COM')) then
  begin
    FInterfaceConfig.IfaceType := itCOM;
    try
      FSerialInterface := TBlockSerialNoReset.Create;
      FSerialInterface.EnableRTSToggle(False);
      FSerialInterface.ConvertLineEnd := True;
      FSerialInterface.DeadlockTimeout := 100; // Wartezeit für den seriellen Port
      FSerialInterface.Connect(AInterface); // Setzen Sie den gewünschten COM - Port
      FSerialInterface.Config(115200, 8, 'N', SB1, True, False); // Setzen Sie die gewünschte Baudrate und Konfiguration
      Sleep(500);
      FInterfaceConfig.IsConnected := True;
      FInterfaceConfig.IfaceName := AInterface;

      if Assigned(FLoggingCallback) then
      begin
        if FSerialInterface.CanRead(1000) then
        begin
          RecvText := FSerialInterface.RecvPacket(1);
          if (RecvText.StartsWith('Err')) then
          begin
            FLoggingCallback(lvWARNING, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Received an error during connection: ' + Trim(RecvText), Now);
          end
          else
          if Length(RecvText) > 0 then
          begin
            FLoggingCallback(lvINFO, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Received: ' + Trim(RecvText), Now);
          end;
        end;
      end;

    except
      on E: Exception do
      begin
        FInterfaceConfig.IsConnected := False;
        FInterfaceConfig.IfaceType := itNONE;
        if Assigned(FLoggingCallback) then
          FLoggingCallback(lvCRITICAL, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Interface exception: ' + E.Message, Now);
      end;
    end;
  end;
end;

{
 Constructor
 Initializes class variables and starts the VFD.
}

procedure TU8G2.DspInit(XRes, YRes: Word);
begin
  FGfxWidth := XRes;
  FGfxHeight := YRes;

  // calculate number of text columns and rows
  FTxtWidth := (FGfxWidth div (FGlyphConfig.Width + FGlyphConfig.Gap));
  FTxtHeight := (FGfxHeight div FGlyphConfig.Height);

end;


function TU8G2.removeLeadingZeros(Text: String): String;
begin
  Result:= Text;
  while (Result.StartsWith('0')) do begin
    Result:= Result.Remove(0, 1);
  end;
  if ('' = Result) then
    Result:= '0';
end;

procedure TU8G2.SerialOut(Text: String);
var
  RecvText: String;
begin
  Text := Text + #10;

  try
    if FSerialInterface.CanWrite(-1) then
    begin
      FSerialInterface.SendString(Text);
      FNumBytesSent := FNumBytesSent + Length(Text);
      //Application.ProcessMessages;
      //FSerialInterface.Flush;
    end
    else
    begin
      FInterfaceConfig.IfaceType := itNONE;
      if Assigned(FLoggingCallback) then
      begin
        FLoggingCallback(lvERROR, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Write timeout on ' + FInterfaceConfig.IfaceName, Now);
      end;
      if Assigned(FConnectionFailureCallback) then
      begin
        FConnectionFailureCallback('WriteTimeout', Now);
      end;
    end;
  except
    on E: Exception do
    begin
      FInterfaceConfig.IfaceType := itNONE;
      if Assigned(FLoggingCallback) then
      begin
        FLoggingCallback(lvCRITICAL, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Interface exception: ' + E.Message, Now);
      end;
      if Assigned(FConnectionFailureCallback) then
      begin
        FConnectionFailureCallback('InterfaceException', Now);
      end;
    end;
  end;

  (*
  if Assigned(FLoggingCallback) then begin
    //if FSerialInterface.CanRead(100) then begin
      RecvText:= FSerialInterface.RecvPacket(1);
      if (RecvText.StartsWith('Err')) then begin
        FLoggingCallback('s ' + Text);
        FLoggingCallback('l ' + FDbgLastSent);
        FLoggingCallback('r ' + RecvText + ' @ ' + inttostr(FNumBytesSent) + ' bytes sent');
      end else if Length(RecvText) > 0 then begin
        FLoggingCallback('< ' + RecvText);
      end;
    //end;
  end;
  *)

  FDbgLastSent := FDbgLastSent + Text.Replace(#10, 'n');
  if Length(FDbgLastSent) > 80 then
  begin
    FDbgLastSent := RightStr(FDbgLastSent, 80);
  end;
end;

{
  Request the Arduino to update the display
}
procedure TU8G2.UpdateDisplayFromBuffer;
var
  Cmd: String;
begin
  Cmd := 'U';
  SerialOut(Cmd);
end;

end.
