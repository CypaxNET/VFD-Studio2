unit NTK300;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, VFDisplay, Graphics, Math, noresetsynaser, GraphUtil,
  StudioCommon, Glyphs;
type
  { NTK300 }
  TNTK300 = class(TVFDisplay)
  private

  protected

    // shadowed screen data
    FShadowLayer0: Array of Array of Byte;
    FShadowLayer1: Array of Array of Byte;

    FSerialInterface: TBlockSerialNoReset;

    FPosX, FPosY: Word;

    FNumBytesSent: Cardinal;
    FDbgLastSent: String;

    FIsAutoInc: Boolean;

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
    procedure SetBrightness(Percent: Byte); override;
    procedure SetLayerMode(LayerMode: TLayerMode); override;
    procedure Dbg(Value: Byte); override;

    { General display funtionality }
    procedure VFDInit();
    procedure VFDLayerConfig(IsL0On, IsL1On: Boolean; Comb: TLayerMode);
    { Positioning / addressing }
    procedure VFDSetAddress(Addr: Word; ALayer: Byte);
    procedure VFDAddressInc(DoInc: Boolean);
    procedure VFDSetXCoord(X: Byte);
    procedure VFDSetYCoord(Y: Byte);
    procedure VFDSetCoord(X, Y: Byte);
    { Display data write/read }
    procedure VFDWriteByte(Value: Byte);
    procedure VFDWriteByteXY(Value, X, Y: Byte);
    function VFDReadByte(Col, Row: Integer): Byte;
    { Low level display methods }
    procedure VFDWriteCommand(Cmd: Byte);
    procedure VFDWriteData(Dat: Byte);

    { Other / helper methods }
    procedure SelectScreen(ALayer: Word);
    procedure SendClearScreenRequest(ALayer: Word);
    procedure SerialOut(Text: String);

  end;


const

  CURSORGLYPH: array[0..5] of Byte = ($FF, $FF, $FF, $FF, $FF, $FF);

  VFD_BLACK = 0;
  VFD_WHITE = 1;

  START_ADDR_L0     = $0000;
  START_ADDR_L1     = $0800;

  CMD_DSP_ONOFF     = $00;    // display on/off
  BITS_L0           = $01;    // layer0 on
  BITS_L1           = $02;    // layer1 on

  CMD_CUR_INC       = $04;    // cursor increments automatically
  CMD_CUR_HOLD      = $05;    // cursor holds

  CMD_L1_TEXT       = $06;    // layer1 in text mode
  CMD_L1_GFX        = $07;    // layer1 in graphics mode

  CMD_DATA_WRITE    = $08;    // write data
  CMD_DATA_READ     = $09;    // read data

  CMD_SET_START0_L  = $0A;    // set lower byte of layer0 start address
  CMD_SET_START0_H  = $0B;    // set upper byte of layer0 start address

  CMD_SET_START1_L  = $0C;    // set lower byte of layer1 start address
  CMD_SET_START1_H  = $0D;    // set upper byte of layer1 start address

  CMD_SET_CURSOR_L  = $0E;    // set lower byte of cursor address
  CMD_SET_CURSOR_H  = $0F;    // set upper byte of cursor address

  CMD_OR_MODE       = $10;    // OR layer0 and layer1
  CMD_XOR_MODE      = $11;    // XOR layer0 and layer1
  CMD_AND_MODE      = $12;    // AND layer0 and layer1

  CMD_BRIGHT_100    = $18;
  CMD_BRIGHT_88     = $19;
  CMD_BRIGHT_75     = $1A;
  CMD_BRIGHT_63     = $1B;


implementation


constructor TNTK300.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDisplayType := 'NTK300';

  FNumBytesSent := 0;
  FDbgLastSent := '';

  FInterfaceConfig.IfaceType := itNONE; // will be set during Connect()

  FNumLayers := 2; // this display has two layers

  FSelectedLayer := 0;

  //VFDInit; // start/initialize the display

end;

destructor TNTK300.Destroy;
begin
  if (nil <> FSerialInterface) then
  begin
    FSerialInterface.CloseSocket;
    FSerialInterface.Free;
  end;

  SetLength(FShadowLayer0, 0, 0);
  SetLength(FShadowLayer1, 0, 0);

  inherited; // Also called parent class destroyer
end;


procedure TNTK300.Dbg(Value: Byte);
begin
  FLoggingCallback(lvINFO, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Debug procedure entered', Now);
  //VFDPutGlyphText('HALLO');
  //VFDSetCoord(1, 8);
  //VFDWriteCommand(CMD_DATA_WRITE);
  //VFDWriteByte(Value);
  PaintPixel(8, 3, False);
  PaintPixel(0, 0, False);
  PaintPixel(1, 1, False);
  PaintPixel(2, 2, False);
  PaintPixel(3, 3, False);
  PaintPixel(10, 10, False);
  (*
  PaintLine(8, 0, 16, 0, False);
  PaintLine(16, 0, 16, 20, False);
  *)

  SetBrightness(50);
  PaintString('Hallo Welt!', 2, 1);

end;

procedure TNTK300.SendClearScreenRequest(ALayer: Word);
var
  Text, RecvText: String;
  Row, Col: Word;
begin

  // sending all the 0-bytes to clear the screen would take a lot of bandwidth
  // so instead we instruct the Arduino to do it
  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    if (0 = ALayer) then
    begin
      Text := 'x0' + #10;
    end
    else
    begin
      Text := 'x1' + #10;
    end;

    try
      if FSerialInterface.CanWrite(100) then
      begin
        FSerialInterface.SendString(Text);
        Application.ProcessMessages;
        FSerialInterface.Flush;
        Sleep(100);
      end
      else
      begin
        FInterfaceConfig.IfaceType := itNONE;
        if Assigned(FLoggingCallback) then
        begin
          FLoggingCallback(lvERROR, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Write timeout on ' + FInterfaceConfig.IfaceName, Now);
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
      end;
    end;

    if Assigned(FLoggingCallback) then
    begin
      if FSerialInterface.CanRead(100) then
      begin
        RecvText := FSerialInterface.Recvstring(100);
        if (RecvText.StartsWith('Err')) then
        begin
          FLoggingCallback(lvWARNING, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Received an error when sending "' + Text + '":' + Trim(RecvText), Now);
        end;
      end;
    end;
  end;

  // also clear the shadow screen
  if (0 = ALayer) then
  begin
    for Row := 0 to 7 do
    begin
      for Col := 0 to FGfxWidth - 1 do
      begin
        FShadowLayer0[Col, Row] := 0;
      end;
    end;
  end;
  if (1 = ALayer) then
  begin
    for Row := 0 to 7 do
    begin
      for Col := 0 to FGfxWidth - 1 do
      begin
        FShadowLayer1[Col, Row] := 0;
      end;
    end;
  end;
end;

procedure TNTK300.ClearScreen;
begin
  SendClearScreenRequest(0);
  SendClearScreenRequest(1);
end;

procedure TNTK300.SelectScreen(ALayer: Word);
begin
  if (ALayer < FNumLayers) then
  begin
    FSelectedLayer := ALayer;
  end;
end;


procedure TNTK300.ShowScreen(ALayer: Word);
var
  L0, L1: Boolean;
begin
  L0 := (ALayer and LAYER_0) = LAYER_0;
  L1 := (ALayer and LAYER_1) = LAYER_1;

  VFDLayerConfig(L0, L1, lmXOR);
end;


procedure TNTK300.PaintString(Text: String; X, Y: Integer);
var
  Character: Char;
begin
  Text := TGlyphs.Adapt2Charmap(Text);

  SelectScreen(1);
  VFDSetCoord(X, Y);

  // enable auto inc mode
  VFDWriteCommand(CMD_CUR_INC);

  // enter data write mode
  VFDWriteCommand(CMD_DATA_WRITE);

  // loop through string
  for Character in Text do
  begin
    VFDWriteData(Byte(Character));
  end; // end for

  // disable auto inc mode
  VFDWriteCommand(CMD_CUR_HOLD);
end;

{
  Draws a bitmap on the display
}
procedure TNTK300.PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word);
var
  X, Y, Temp, I: Integer;
  Mask: Byte;
  NumPixels: Byte;
  Pixel: Byte;
  X0, X1, Y0, Y1: Word;
  IY: Word;
  PixelColor: TColor;
  Block: Byte;
  NumRemainingPixels: Byte;
begin
  SelectScreen(0);

  aBitmap.Canvas.Pixels[0, 0] := aBitmap.Canvas.Pixels[0, 0];  // this seems nonsense, but one way to actually assign memory to the bitmap canvas is by acessing its pixels

  X0 := XPos;
  Y0 := YPos;
  X1 := X0 + aBitmap.Canvas.Width - 1;
  Y1 := Y0 + aBitmap.Canvas.Height;

  if (X1 >= FGfxWidth) then
    X1 := FGfxWidth - 1;

  if (Y1 >= FGfxHeight) then
    Y1 := FGfxHeight - 1;


  for X := X0 to X1 do
  begin
    VFDSetCoord(X, Y0);
    VFDAddressInc(True);
    VFDWriteCommand(CMD_DATA_WRITE);

    NumPixels := 0;
    Y := Y0;

    // draw a partial first row of dots?
    if ((Y0 and 7) <> 0) then
    begin
      NumPixels := 8 - (Y0 and 7); // number of pixels to set in the first row
      Temp := 8 - NumPixels; // number of pixels we don't overwrite

      Block := VFDReadByte(X, Y); // get the existing data

      Pixel := 0;
      for IY := 0 to NumPixels - 1 do
      begin
        if (IY >= aBitmap.Canvas.Height) then
          Break;

        Pixel := Pixel shl 1;
        PixelColor := aBitmap.Canvas.Pixels[X - X0, IY];
        if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
        begin
          Pixel := Pixel or $01;
        end;
      end;

      // Block now contains the display data and Pixel the bitmap data
      // set the pixels we are overwriting
      for I := 0 to NumPixels - 1 do
      begin
        Mask := Byte($01) shl I;
        // take the pixels from the bitmap
        if (Mask and Pixel <> 0) then
        begin
          // bit is set
          Block := Block or Mask;
        end
        else
        begin
          // bit is not set
          Block := Block and (not Mask);
        end;
      end; // end for I

      VFDWriteData(Block);

    end;  // end: if draw partial first row

    // check if the first row already reached the canvas height
    if (NumPixels >= aBitmap.Canvas.Height) then
      Continue;

    Y := Y + NumPixels;

    // is there more to display?
    if Y < Y1 then
    begin
      NumRemainingPixels := Y1 - Y;     // how much is left?
      Temp := NumRemainingPixels shr 3; // divide by 8 to get number of full columns
      if (Temp > 0) then
      begin
        for I := 0 to (Temp - 1) do
        begin  // for each full column

          Pixel := 0;
          for IY := 0 to 7 do
          begin
            Pixel := Pixel shl 1;
            PixelColor := aBitmap.Canvas.Pixels[X - X0, Y - Y0 + IY];
            if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
            begin
              Pixel := Pixel or $01;
            end;
          end;

          VFDWriteData(Pixel);

          Y := Y + 8; // increment by one column in each loop

        end; // for I
      end; // if Temp > 0
    end; // Y < Y1

    // is there still more to display?
    if Y < Y1 then
    begin
      NumRemainingPixels := Y1 - Y;     // how much is left?
      Temp := 8 - NumRemainingPixels; // number of pixels we don't overwrite
      Block := VFDReadByte(X, Y); // get the existing data

      Pixel := 0;
      for IY := NumRemainingPixels downto 1 do
      begin
        Pixel := Pixel shl 1;
        PixelColor := aBitmap.Canvas.Pixels[X - X0, aBitmap.Canvas.Height - IY];
        if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
        begin
          Pixel := Pixel or $01;
        end;
      end;
      Pixel := Pixel shl Temp;

      // Block now contains the display data and Pixel the bitmap data

      // set the pixels we are overwriting
      for I := 0 to NumRemainingPixels - 1 do
      begin
        Mask := Byte($80) shr I;
        // take the pixels from the bitmap
        if (Mask and Pixel <> 0) then
        begin
          // bit is set
          Block := Block or Mask;
        end
        else
        begin
          // bit is not set
          Block := Block and (not Mask);
        end;
      end; // end for I

      VFDWriteData(Block);

    end; // still more to display?

  end; // for X

end;

procedure TNTK300.PaintPixel(X, Y: Word; IsInverted: Boolean);
var
  Pixel: Byte;
  Mask: Byte;
begin
  SelectScreen(0);

  Pixel := VFDReadByte(X, Y);
  Mask := 1 shl (7 - (Y and 7));

  // set or reset Pixel, depending on color
  if (IsInverted) then
  begin
    Pixel := Pixel and (not Mask);
  end
  else
  begin
    Pixel := Pixel or Mask;
  end; // endif

  // write modified Byte back to display
  VFDWriteByteXY(Pixel, X, Y);
end;

procedure TNTK300.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  X, Y, Temp: Word;
  I: Integer;
  DeltaX, DeltaY: Integer;
  S1, S2: Integer;
  //Temp: Integer;
  Direction: Shortint;
  Err: Integer;
  Pixel, Mask: Byte;
begin
  SelectScreen(0);

  if (X0 = X1) and (Y0 = Y1) then
  begin//  return if no line to draw
    PaintPixel(X0, Y0, IsInverted);
    Exit;
  end;

  // make sure X0 is < X1
  if (X0 > X1) then
  begin
    Temp := X0;
    X0 := X1;
    X1 := Temp;
    Temp := Y0;
    Y0 := Y1;
    Y1 := Temp;
  end;

  // initialize variables
  X := X0;
  Y := Y0;

  DeltaX := Abs(X1 - X0);
  DeltaY := Abs(Y1 - Y0);

  S1 := Sign(X1 - X0);
  S2 := Sign(Y1 - Y0);

  // exchange DeltaX and DeltaY depending on slope
  if (DeltaY > DeltaX) then
  begin
    Temp := DeltaX;
    DeltaX := DeltaY;
    DeltaY := Temp;
    Direction := 1;
  end
  else
  begin
    Direction := 0;
  end; // endif

  // initialize the error term to compensate for nonzero intercept
  Err := (DeltaY shl 1) - DeltaX;

  // draw the line
  for I := 0 to DeltaX do
  begin
    Pixel := VFDReadByte(X, Y);
    Mask := 1 shl (7 - (Y and 7));

    // set or reset Pixel, depending on color
    if (IsInverted) then
    begin
      Pixel := Pixel and (not Mask);
    end
    else
    begin
      Pixel := Pixel or Mask;
    end; // endif

    // write modified Byte back to display
    VFDWriteByteXY(Pixel, X, Y);

    while (Err >= 0) do
    begin
      if (Direction = 1) then
      begin
        X := X + S1;
      end
      else
      begin
        Y := Y + S2;
      end; // endif
      Err := Err - (DeltaX shl 1);
    end; // endwhile

    if (Direction = 1) then
    begin
      Y := Y + S2;
    end
    else
    begin
      X := X + S1;
    end; // endif
    Err := Err + (DeltaY shl 1);

  end; // endfor

end;

procedure TNTK300.SetBrightness(Percent: Byte);
var
  Cmd: Byte;
begin
  // 100%  = $18 = 24
  // 87,5% = $19 = 25
  // 75%   = $1A = 26
  // 62,5% = $1B = 27

  if (Percent > 87) then
    Cmd := $18
  else
  if (Percent > 75) then
    Cmd := $19
  else
  if (Percent > 62) then
    Cmd := $1A
  else
    Cmd := $1B;
  VFDWriteCommand(Cmd);
end;


procedure TNTK300.SetLayerMode(LayerMode: TLayerMode);
begin
  VFDLayerConfig(True, True, LayerMode);
end;

procedure TNTK300.Connect(AInterface: String);
var
  RecvText: String;
begin
  if (AInterface.StartsWith('COM')) then
  begin
    FInterfaceConfig.IfaceType := itCOM;
    try
      FSerialInterface := TBlockSerialNoReset.Create;
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

procedure TNTK300.DspInit(XRes, YRes: Word);
begin
  FGfxWidth := XRes;
  FGfxHeight := YRes;

  SetLength(FShadowLayer0, FGfxWidth, FGfxHeight div 8);
  SetLength(FShadowLayer1, FGfxWidth, FGfxHeight div 8);

  VFDInit;
end;


procedure TNTK300.SerialOut(Text: String);
var
  RecvText: String;
  TextLen: Integer;
begin
  Text := Text + #10;
  TextLen := Length(Text);
  if Assigned(FLoggingCallback) then
  begin
    if (TextLen < 4) then
    begin
      FLoggingCallback(lvERROR, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Invalid text length in command "' + Text + '"', Now);
    end;
    if (Text.ToUpper.Chars[0] <> 'C') and (Text.Chars[0] <> 'D') and (Text.Chars[0] <> 'R') then
    begin
      FLoggingCallback(lvERROR, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Invalid command "' + Text + '"', Now);
    end;
  end;

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
  Sends a command to the display.
}
procedure TNTK300.VFDWriteCommand(Cmd: Byte);
var
  Text: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Text := 'C' + inttohex(Cmd);
    //Text:= 'C' + Format('%.02x', [Cmd]);
    SerialOut(Text);
  end;
end;

{
  Send a Byte of data to the display.
}
procedure TNTK300.VFDWriteData(Dat: Byte);
var
  Text: String;
begin
  if (False = FInterfaceConfig.isConnected) then Exit;

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Text := 'D' + inttohex(Dat);
    //Text:= 'D' + Format('%.02x', [d]);
    SerialOut(Text);
  end;
end;


{
  Writes a Byte to the display GRAM at current position.
}
procedure TNTK300.VFDWriteByte(Value: Byte);
begin
  if (FSelectedLayer = 0) then
  begin
    FShadowLayer0[FPosX, FPosY shr 3] := Value;
  end
  else
  begin
    FShadowLayer1[FPosX, FPosY shr 3] := Value;
  end;

  VFDWriteData(Value);
end;


{
  X is a Pixel position 0..
  Y is a Pixel position 0.. but expected to match a Byte block address (0, 8, 16, ..)
}
procedure TNTK300.VFDSetCoord(X, Y: Byte);
var
  Addr: Word;
begin
  FPosX := X;
  FPosY := Y;

  Addr := 0;

  if (FSelectedLayer = 0) then
  begin
    Addr := START_ADDR_L0 + (FPosX * 8) + (FPosY shr 3);
  end
  else
  begin
    Addr := START_ADDR_L1 + FPosX + (FPosY * $80);
  end;

  VFDWriteCommand(CMD_SET_CURSOR_L);
  VFDWriteData(Addr and $FF);
  VFDWriteCommand(CMD_SET_CURSOR_H);
  VFDWriteData(Addr shr 8);

end;

{
  X is a Pixel position 0..
}
procedure TNTK300.VFDSetXCoord(X: Byte);
begin
  FPosX := X;
  if (FSelectedLayer = 0) then
  begin
    VFDSetAddress((FPosX * 8) + (FPosY shr 3), 0);
  end
  else
  begin
    VFDSetAddress(START_ADDR_L1 + FPosX + (FPosY * $80), 1);
  end;
end;

{
  Y is a Pixel position 0.. but expected to match a Byte block address (0, 8, 16, ..)
}
procedure TNTK300.VFDSetYCoord(Y: Byte);
begin
  FPosY := Y;
  if (FSelectedLayer = 0) then
  begin
    VFDSetAddress((FPosX * 8) + (FPosY shr 3), 0);
  end
  else
  begin
    VFDSetAddress(START_ADDR_L1 + FPosX + (FPosY * $80), 1);
  end;
end;



{
  Writes a Byte to the display GRAM.
  X is a Pixel position 0..,
  Y is a Pixel position 0.. but expected to match a Byte block address (0, 8, 16, ..)
}
procedure TNTK300.VFDWriteByteXY(Value, X, Y: Byte);
begin
  VFDSetCoord(X, Y); // expects Y to be 0, 8, 16, ..
  VFDWriteCommand(CMD_DATA_WRITE);
  VFDWriteByte(Value);
end;




{
  Reads display content at given position from shadow copy
}
function TNTK300.VFDReadByte(Col, Row: Integer): Byte;
var
  X, Y: Integer;
  Data: Byte;
begin
  Data := 0;
  X := Col;
  Y := Row shr 3;
  if (FSelectedLayer = 0) then
  begin
    Data := FShadowLayer0[X, Y];
  end
  else
  begin
    Data := FShadowLayer1[X, Y];
  end;

  Result := Data;
end;


{
  Configuration of display layers
}
procedure TNTK300.VFDLayerConfig(IsL0On, IsL1On: Boolean; Comb: TLayerMode);
var
  Cmd: Byte;
begin

  Cmd := CMD_DSP_ONOFF;
  if (IsL0On) then
    Cmd := Cmd or BITS_L0;
  if (IsL1On) then
    Cmd := Cmd or BITS_L1;

  VFDWriteCommand(Cmd);

  if (lmOR = Comb) then
    Cmd := CMD_OR_MODE
  else
  if (lmAND = Comb) then
    Cmd := CMD_AND_MODE
  else
  if (lmXOR = Comb) then
    Cmd := CMD_XOR_MODE
  else
    Exit;
  VFDWriteCommand(Cmd);

end;


{
Display initialization procedure
}
procedure TNTK300.VFDInit;
var
  Text, RecvText: String;
begin

  // clear port to idle state

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Text := '3 ' + IntToHex(FGfxWidth) + ' ' + IntToHex(FGfxHeight) + #10;

    try
      if FSerialInterface.CanWrite(100) then
      begin
        FSerialInterface.SendString(Text);
        Application.ProcessMessages;
        FSerialInterface.Flush;
        Sleep(100);
      end
      else
      begin
        FInterfaceConfig.IfaceType := itNONE;
        if Assigned(FLoggingCallback) then
        begin
          FLoggingCallback(lvERROR, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Write timeout on ' + FInterfaceConfig.IfaceName, Now);
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
      end;
    end;

    if Assigned(FLoggingCallback) then
    begin
      if FSerialInterface.CanRead(100) then
      begin
        RecvText := FSerialInterface.Recvstring(100);
        if (RecvText.StartsWith('Err')) then
        begin
          FLoggingCallback(lvWARNING, Self.ClassName + '.' + {$INCLUDE %CURRENTROUTINE%} + ': Received an error when sending "' + Text + '":' + Trim(RecvText), Now);
        end;
      end;
    end;
  end;

  ClearScreen;

  VFDLayerConfig(True, False, lmXOR);

  VFDWriteCommand(CMD_CUR_HOLD); // auto increment off

  VFDSetCoord(0, 0);

  // calculate number of text columns and rows (built-in character set is 8x6)
  FTxtWidth := (FGfxWidth div 6);
  FTxtHeight := (FGfxHeight div 8);

end;

procedure TNTK300.VFDSetAddress(Addr: Word; ALayer: Byte);
begin
  if (0 = ALayer) then
  begin
    VFDWriteCommand(CMD_SET_START0_L);
    VFDWriteData(Addr and $FF);
    VFDWriteCommand(CMD_SET_START0_H);
    VFDWriteData(Addr shr 8);
  end
  else
  if (1 = ALayer) then
  begin
    VFDWriteCommand(CMD_SET_START1_L);
    VFDWriteData(Addr and $FF);
    VFDWriteCommand(CMD_SET_START1_H);
    VFDWriteData(Addr shr 8);
  end;
end;

procedure TNTK300.VFDAddressInc(DoInc: Boolean);
begin
  FIsAutoInc := DoInc;
  if (FIsAutoInc) then
    VFDWriteCommand(CMD_CUR_INC)
  else
    VFDWriteCommand(CMD_CUR_HOLD);
end;

end.
