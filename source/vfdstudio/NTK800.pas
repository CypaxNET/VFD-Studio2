unit NTK800;

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
  { NTK800 }
  TNTK800 = class(TVFDisplay)
  private

  protected

    // shadowed screen data
    FShadowLayer0: Array of Array of Byte;
    FShadowLayer1: Array of Array of Byte;

    FSerialInterface: TBlockSerialNoReset;

    FPosX, FPosY: Word;

    FNumBytesSent: Cardinal;
    FDbgLastSent: String;

    FIsAutoIncX: Boolean;
    FIsAutoIncY: Boolean;

    FGlyphConfig: TGlyphConfig; // all data related to glyphs

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

    { Text related methods }
    procedure VFDSetupGlyphText(AWidth, AHeight, AGap: Byte);
    procedure VFDGlyphInit;
    procedure VFDSetGlyph(Glyph: PChar; Index: Byte);
    procedure VFDPutGlyphChar(AChar: Char);
    procedure VFDPutGlyphText(Text: String);

    { General display funtionality }
    procedure VFDInit();
    procedure VFDLayerConfig(IsL0On, IsL1On, IsInverted: Boolean; Comb: TLayerMode);
    { Positioning / addressing }
    procedure VFDAddressInc(DoIncX, DoIncY: Boolean);
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
    procedure SerialOut(Text: String);

  end;


const

  CURSORGLYPH: array[0..5] of Byte = ($FF, $FF, $FF, $FF, $FF, $FF);

  VFD_BLACK = 0;
  VFD_WHITE = 1;

  CMD_DSP_CLEAR     = $52;    // clear display and reset write data position
  BITS_HM           = $01;    // HM = initialize data write position address and display start position address (1) or do not initialize (0)
  BITS_G0C          = $04;    // G0C = GRAM area0 cleared (1) or GRAM area0 not cleared (0)
  BITS_G1C          = $08;    // G1C = GRAM area1 cleared (1) or GRAM area1 not cleared (0)

  CMD_BRIGHTNESS    = $40;    // lower four bits represent brightness volume

  CMD_AREA_SET      = $62;    // display area assignment
  BITS_AREA_GRAPHIC = $FF;    // display area assigned to graphics mode
  BITS_AREA_TEXT    = $00;    // display area assigned to text mode (NOT SUPPORTED by this display!)

  CMD_DSP_ONOFF     = $20;    // display on/off
  BITS_L1           = $08;    // GRAM layer1 active (1) or inactive (0); if both layers are active they will be OR - combined unless AND or XOR mode are enabled
  BITS_L0           = $04;    // GRAM layer0 active (1) or inactive (0); if both layers are active they will be OR - combined unless AND or XOR mode are enabled
  BITS_GS           = $40;    // graphic display area (GRAM) on (1) or off (0)
  BITS_GRV          = $10;    // graphic display area (GRAM) IsInverted (1) or normal (0)
  BITS_AND_MODE     = $08;    // AND combination of layer0 and layer1
  BITS_XOR_MODE     = $04;    // XOR combination of layer0 and layer1

  CMD_HSHIFT        = $70;    // horizontal shift (shift amount is set by 2nd command Byte)
  CMD_VSHIFT        = $B0;    // vertical shift
  BITS_VSHIFT_UD    = $08;    // display scrolled up (1) or down (0)
  BITS_VSHIFT_S1    = $04;    // S1=0, S0=1 - > shift by 8 pixels; S1=1, S0=0 - > shift by 1 Pixel; S1=1, S0=1 - > shift by 2 pixels
  BITS_VSHIFT_S0    = $02;

  CMD_ADRMODE       = $80;    // address mode set command
  BITS_AUTOINC_X    = $04;    // increment X address on writing
  BITS_AUTOINC_Y    = $02;    // increment Y address on writing

  CMD_SET_X         = $64;    // set X coordinate (in pixels); coordinate 0..127 is set by 2nd command Byte
  CMD_SET_Y         = $60;    // set Y coordinate (in 8 - dots) block units; coordinate 0..15 is set by 2 command Byte

implementation


constructor TNTK800.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  FDisplayType := 'NTK800';

  FNumBytesSent := 0;
  FDbgLastSent := '';

  FInterfaceConfig.IfaceType := itNONE; // will be set during Connect()

  FNumLayers := 2; // this display has two layers

  FSelectedLayer := 0;

  for I := 0 to 255 do
  begin
    FGlyphConfig.glyphTable[I] := nil;
  end;

  //VFDInit; // start/initialize the display

end;

destructor TNTK800.Destroy;
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


procedure TNTK800.Dbg(Value: Byte);
begin
  VFDPutGlyphText('HALLO');
end;

procedure TNTK800.ClearScreen;
var
  Cmd: Byte;
  Col, Row: Integer;
begin
  Cmd := CMD_DSP_CLEAR;
  Cmd := Cmd or BITS_HM;
  Cmd := Cmd or BITS_G0C;
  Cmd := Cmd or BITS_G1C;

  for Row := 0 to (FTxtHeight - 1) do
  begin
    for Col := 0 to (FGfxWidth - 1) do
    begin
      FShadowLayer0[Col, Row] := 0;
    end;
  end;

  for Row := 0 to (FTxtHeight - 1) do
  begin
    for Col := 0 to (FGfxWidth - 1) do
    begin
      FShadowLayer1[Col, Row] := 0;
    end;
  end;

  VFDWriteCommand(Cmd);
end;

procedure TNTK800.SelectScreen(ALayer: Word);
begin
  if (ALayer < FNumLayers) then
  begin
    FSelectedLayer := ALayer;
  end;
end;


procedure TNTK800.ShowScreen(ALayer: Word);
var
  L0, L1: Boolean;
begin
  L0 := (ALayer and LAYER_0) = LAYER_0;
  L1 := (ALayer and LAYER_1) = LAYER_1;

  VFDLayerConfig(L0, L1, False, lmXOR);
end;


procedure TNTK800.PaintString(Text: String; X, Y: Integer);
begin
  Text := TGlyphs.Adapt2Charmap(Text);

  SelectScreen(1);

  FGlyphConfig.currentCol := X;
  FGlyphConfig.currentRow := Y;
  VFDPutGlyphText(Text);
end;

{
  Draws a bitmap on the display
}
procedure TNTK800.PaintBitmap(ABitmap: TBitmap; XPos, YPos: Word);
var
  X, Y, Temp, I: Integer;
  Mask: Byte;
  NumPixels: Byte;
  Pixel: Byte;
  X0, X1, Y0, Y1: Word;
  IY: Word;
  PixelColor: TColor;
  Block: Byte;
  Col: Byte;
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

  NumPixels := 0;

  // draw partial first Row of dots
  if ((Y0 and 7) <> 0) then
  begin
    VFDSetCoord(X0, Y0);
    VFDAddressInc(True, False);
    // get the block index
    Col := Y0 shr 3; // "shr 3" equals division by 8
    for X := X0 to X1 do
    begin
      // Y - coordinate does not start at the begin of a dot block
      NumPixels := 8 - (Y0 and 7); // number of pixels to set in the first Row
      Block := VFDReadByte(X, Col * 8); // get the existing data
      Temp := 8 - NumPixels; // number of pixels we don't overwrite

      Pixel := 0;
      for IY := 0 to NumPixels - 1 do
      begin
        Pixel := Pixel shl 1;
        PixelColor := aBitmap.Canvas.Pixels[X - X0, IY];
        if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
        begin
          Pixel := Pixel or $01;
        end;
      end;
      Pixel := Pixel shl Temp;
      Pixel := ReverseBits(Pixel);
      Pixel := Pixel shl Temp;
      // Block now contains the display data and Pixel the bitmap data

      // set the pixels we are overwriting
      for I := 0 to NumPixels - 1 do
      begin
        if (I + Y0) > Y1 then
        begin
          Break;
        end;
        Mask := Byte(1) shl (I + Temp);

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

      // write out modified block
      FPosX := X;
      FPosY := Y0;
      VFDWriteByte(Block);
    end; // endfor X
    //Application.ProcessMessages;
    //Sleep(0);
    VFDAddressInc(False, False);
  end;

  Y := Y0 + NumPixels;

  // is there more to display?
  if Y < Y1 then
  begin
    NumRemainingPixels := Y1 - Y;     // how much is left?
    Temp := NumRemainingPixels shr 3;       // divide by 8 to get number of full columns
    if (Temp > 0) then
    begin
      for I := 0 to (Temp - 1) do
      begin  // for each full column
        VFDSetCoord(X0, Y);
        VFDAddressInc(True, False);
        Col := Y shr 3;
        for X := X0 to X1 do
        begin
          //get block index

          Pixel := 0;
          for IY := 0 to 7 do
          begin
            Pixel := Pixel shl 1;
            PixelColor := aBitmap.Canvas.Pixels[X - X0, NumPixels + IY];
            if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
            begin
              Pixel := Pixel or $01;
            end;
          end;
          block := ReverseBits(Pixel);

          // write out block
          FPosX := X;
          FPosY := Y;
          VFDWriteByte(block);

        end; // end for X
        //Application.ProcessMessages;
        //Sleep(0);
        VFDAddressInc(False, False);
        Y := Y + 8;
        NumPixels := NumPixels + 8;
      end; // end for I
    end;
  end;

  // is there still more to display?
  if Y < Y1 then
  begin
    NumRemainingPixels := Y1 - Y;     // how much is left?

    VFDSetCoord(X0, Y);
    VFDAddressInc(True, False);
    Col := Y shr 3;

    for X := X0 to X1 do
    begin
      // get the block index
      block := VFDReadByte(X, Col * 8); // get the existing data

      Pixel := 0;
      for IY := 0 to NumRemainingPixels - 1 do
      begin
        Pixel := Pixel shl 1;
        PixelColor := aBitmap.Canvas.Pixels[X - X0, (NumPixels - 1) + NumRemainingPixels - IY];
        if (ColorToGray(PixelColor) < GREY_VALUE_THRESHOLD) then
        begin
          Pixel := Pixel or $01;
        end;
      end;

      // Block now contains the display data and Pixel the bitmap data

      // set the pixels we are overwriting
      for I := 0 to NumRemainingPixels - 1 do
      begin
        Mask := Byte(1) shl I;
        // take the pixels from the bitmap
        if (Mask and Pixel <> 0) then
        begin
          // bit is set
          block := block or Mask;
        end
        else
        begin
          // bit is not set
          block := block and (not Mask);
        end;
      end; // end for I

      // write out modified block
      FPosX := X;
      FPosY := Y;
      VFDWriteByte(block);

    end; // end for X
    //Application.ProcessMessages;
    //Sleep(0);
    VFDAddressInc(False, False);
  end;

end;

procedure TNTK800.PaintPixel(X, Y: Word; IsInverted: Boolean);
var
  Pixel: Byte;
  Mask: Byte;
begin
  SelectScreen(0);

  Pixel := VFDReadByte(X, Y);
  Mask := 1 shl (Y and 7);

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

procedure TNTK800.PaintLine(X0, Y0, X1, Y1: Word; IsInverted: Boolean);
var
  X, Y, Temp: Word;
  OldX, OldY: Word;
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
  OldX := X; OldY := Y;

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

  VFDSetCoord(X, Y);
  VFDAddressInc(False, False);

  // draw the line
  for I := 0 to DeltaX do
  begin
    Pixel := VFDReadByte(X, Y);
    Mask := 1 shl (Y and 7);

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
    VFDWriteByte(Pixel);

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

    // send cmd to change X coordinate only if necessary
    if (X <> OldX) then
    begin
      // new X pos
      VFDSetXCoord(X);
      OldX := X;
    end;

    // send cmd to change Y coordinate only if necessary
    if ((Y shr 3) <> (OldY shr 3)) then
    begin
      // new Y pos
      VFDSetYCoord(Y);
      OldY := Y;
    end;

  end; // endfor

  VFDAddressInc(False, False);
end;

procedure TNTK800.SetBrightness(Percent: Byte);
var
  Bits: Byte;
  Mask: Byte;
begin
  Bits := 100 - Math.Min(100, Percent);
  Mask := Trunc(Single(Bits) / 6.666) and $0F;
  VFDWriteCommand(CMD_BRIGHTNESS or Mask);
end;

procedure TNTK800.SetLayerMode(LayerMode: TLayerMode);
begin
  VFDLayerConfig(True, True, False, LayerMode);
end;


procedure TNTK800.Connect(AInterface: String);
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

procedure TNTK800.DspInit(XRes, YRes: Word);
begin
  FGfxWidth := XRes;
  FGfxHeight := YRes;

  SetLength(FShadowLayer0, FGfxWidth, FGfxHeight div 8);
  SetLength(FShadowLayer1, FGfxWidth, FGfxHeight div 8);

  VFDInit;
end;

procedure TNTK800.VFDSetupGlyphText(AWidth, AHeight, AGap: Byte);
begin
  // store glyph character parameters
  FGlyphConfig.Width := AWidth;
  FGlyphConfig.Height := AHeight;
  FGlyphConfig.gap := AGap;
end;

procedure TNTK800.VFDGlyphInit;
var
  I: Word;
begin
  VFDSetupGlyphText(GLYPH_W, GLYPH_H, GLYPH_GAP);

  for I := 0 to 255 do
  begin
    VFDSetGlyph(@glyphs.charMap8x6[I, 0], I);     // load characters
  end;
  VFDSetGlyph(@CURSORGLYPH, 1);

end;


procedure TNTK800.VFDSetGlyph(Glyph: PChar; Index: Byte);
begin
  FGlyphConfig.glyphTable[Index] := Glyph;
end;


{
This function is used to display a single character to the screen.
This interface is similar to a terminal.
Each character written advances the cursor.
If the character is a carriage return (CR), the cursor moves back to the beginning of the current Row.
If it is a line feed (LF), it will advance to the next Row.
If at the bottom of the screen, a LF will result in the display scrolling one line.
}
procedure TNTK800.VFDPutGlyphChar(AChar: Char);
var
  GlyphIndex: Byte;
  I, J: Byte;
  Row: Byte;
  Col: Byte;
  Pixel: Byte;
begin
  // parse some control codes
  if (AChar = '\r') then
  begin
    FGlyphConfig.currentCol := 0;
  end
  else
  if (AChar = '\n') then
  begin
    if ((FGlyphConfig.currentRow + 1) * FGlyphConfig.Height = FGfxWidth) then
    begin
      FGlyphConfig.currentRow := 0;
      FGlyphConfig.currentCol := 0;
    end
    else
    begin
      Inc(FGlyphConfig.currentRow);
      FGlyphConfig.currentCol := 0;
    end; // endif
  end
  else
  begin
    if (FGlyphConfig.glyphTable[Byte(AChar)] <> nil) then
    begin
      // write character
      GlyphIndex := 0;
      for I := 0 to ((FGlyphConfig.Height shr 3) - 1) do
      begin
        Row := FGlyphConfig.currentRow * FGlyphConfig.Height + (I shl 3);
        VFDAddressInc(True, False);
        VFDSetCoord(FGlyphConfig.currentCol * (FGlyphConfig.Width + FGlyphConfig.gap), Row);
        for J := 0 to (FGlyphConfig.Width - 1) do
        begin
          Col := FGlyphConfig.currentCol * (FGlyphConfig.Width + FGlyphConfig.gap) + J;
          FPosX := Col;
          Pixel := Byte(FGlyphConfig.glyphTable[Byte(AChar), GlyphIndex]);
          VFDWriteByte(Pixel);
          Inc(GlyphIndex);
        end; // endfor
        VFDAddressInc(True, False);
      end; // endfor
    end; // endif
  end; // endif
end;

procedure TNTK800.VFDPutGlyphText(Text: String);
var
  Character: Char;
begin
  // loop through string
  for Character in Text do
  begin
    VFDPutGlyphChar(Character);

    // handle screen wrap
    if ((FGlyphConfig.currentCol + 2) * (FGlyphConfig.Width + FGlyphConfig.gap) >= FGfxWidth) then
    begin
      if (True = FIsLineBreak) then
      begin
        FGlyphConfig.currentCol := 0;
        Inc(FGlyphConfig.currentRow);
      end
      else
      begin
        break;
      end;
    end
    else
    begin
      Inc(FGlyphConfig.currentCol);
    end;
  end; // end for
end;




procedure TNTK800.SerialOut(Text: String);
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
procedure TNTK800.VFDWriteCommand(Cmd: Byte);
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
procedure TNTK800.VFDWriteData(Dat: Byte);
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
procedure TNTK800.VFDWriteByte(Value: Byte);
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
procedure TNTK800.VFDSetCoord(X, Y: Byte);
begin
  VFDSetXCoord(X);
  VFDSetYCoord(Y);
end;

{
  X is a Pixel position 0..
}
procedure TNTK800.VFDSetXCoord(X: Byte);
begin
  FPosX := X;
  VFDWriteCommand(CMD_SET_X);   // set X position
  VFDWriteCommand(X);
end;

{
  Y is a Pixel position 0.. but expected to match a Byte block address (0, 8, 16, ..)
}
procedure TNTK800.VFDSetYCoord(Y: Byte);
begin
  FPosY := Y;
  // set Y position
  if (FSelectedLayer = 0) then
  begin
    VFDWriteCommand(CMD_SET_Y);
    VFDWriteCommand(Y shr 3);     //layer0
  end
  else
  begin
    VFDWriteCommand(CMD_SET_Y);
    VFDWriteCommand((Y shr 3) + 8); // layer 1
  end;
end;



{
  Writes a Byte to the display GRAM.
  X is a Pixel position 0..(FGfxWidth-1),
  Y is a Pixel position 0..(FGfxHeight-1) but expected to match a Byte block address (0, 8, 16, ..)
}
procedure TNTK800.VFDWriteByteXY(Value, X, Y: Byte);
begin
  VFDSetCoord(X, Y); // expects Y to be 0, 8, 16, ..
  VFDWriteByte(Value);
end;




{
  Reads display content at given position from shadow copy
}
function TNTK800.VFDReadByte(Col, Row: Integer): Byte;
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
procedure TNTK800.VFDLayerConfig(IsL0On, IsL1On, IsInverted: Boolean; Comb: TLayerMode);
var
  Cmd: Byte;
begin

  // 1st command Byte
  Cmd := CMD_DSP_ONOFF;
  if (True = IsL0On) then Cmd := Cmd or BITS_L0;
  if (True = IsL1On) then Cmd := Cmd or BITS_L1;
  VFDWriteCommand(Cmd);

  // 2nd command Byte
  Cmd := BITS_GS; // GRAM always on
  if (True = IsInverted) then Cmd := Cmd or BITS_GRV;
  if (lmAND = Comb) then Cmd := Cmd or BITS_AND_MODE;
  if (lmXOR = Comb) then Cmd := Cmd or BITS_XOR_MODE;
  VFDWriteCommand(Cmd);

end;


{
Display initialization procedure
}
procedure TNTK800.VFDInit;
var
  Text, RecvText: String;
begin

  // clear port to idle state

  if (itCOM = FInterfaceConfig.IfaceType) then
  begin
    Text := '8 ' + IntToHex(FGfxWidth) + ' ' + IntToHex(FGfxHeight) + #10;

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


  VFDLayerConfig(True, False, False, lmXOR);

  VFDWriteCommand(CMD_HSHIFT);  // set horizontal shift to
  VFDWriteCommand($00);         // no shift
  VFDWriteCommand(CMD_VSHIFT);  // Vertical shift=0

  VFDWriteCommand(CMD_ADRMODE); // auto increment off

  VFDWriteCommand(CMD_SET_X);   // set X coord
  VFDWriteCommand(0);           // to 0

  VFDWriteCommand(CMD_SET_Y);   // set Y coord
  VFDWriteCommand(0);           // to 0

  VFDGlyphInit;

  // calculate number of text columns and rows
  FTxtWidth := (FGfxWidth div (FGlyphConfig.Width + FGlyphConfig.Gap));
  FTxtHeight := (FGfxHeight div FGlyphConfig.Height);

end;


procedure TNTK800.VFDAddressInc(DoIncX, DoIncY: Boolean);
var
  Cmd: Byte;
begin
  FIsAutoIncX := DoIncX;
  FIsAutoIncY := DoIncY;

  Cmd := CMD_ADRMODE;
  if (True = DoIncX) then Cmd := Cmd or BITS_AUTOINC_X;
  if (True = DoIncY) then Cmd := Cmd or BITS_AUTOINC_Y;
  VFDWriteCommand(Cmd);
end;

end.
