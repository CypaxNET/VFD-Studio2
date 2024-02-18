unit Glyphs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type TGlyphs = class(TDataModule)
  private
  public
    class function Adapt2Charmap(s: String): String;
  end;

const

  GLYPH_W = 6;
  GLYPH_H = 8;
  GLYPH_GAP = 0;

  // 8x6 character font
  charMap8x6: array[0..255, 0..5] of Byte =
    (
    ($00, $00, $00, $00, $00, $00),  // 00
    ($00, $00, $00, $00, $00, $00),  // 01
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),  // 0A
    ($00, $00, $00, $00, $00, $00),  // 0B
    ($00, $00, $00, $00, $00, $00),  // 0C
    ($00, $00, $00, $00, $00, $00),  // 0D
    ($00, $00, $00, $00, $00, $00),  // 0E
    ($00, $00, $00, $00, $00, $00),  // 0F
    ($00, $00, $00, $00, $00, $00),  // 10
    ($00, $00, $00, $00, $00, $00),  // 11
    ($00, $00, $00, $00, $00, $00),  // 12
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),  // 1A
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),
    ($00, $00, $00, $00, $00, $00),  // 1F
    ($00, $00, $00, $00, $00, $00),  // 20 (Space)
    ($00, $00, $5E, $00, $00, $00),  // 21 !
    ($00, $0E, $00, $0E, $00, $00),  // 22 "
    ($14, $7F, $14, $7F, $14, $00),  // 23 #
    ($24, $2A, $7F, $2A, $12, $00),  // 24 $
    ($23, $13, $08, $64, $62, $00),  // 25 %
    ($36, $49, $55, $22, $50, $00),  // 26 &
    ($00, $0A, $06, $00, $00, $00),  // 27 '
    ($00, $1C, $22, $41, $00, $00),  // 28 (
    ($00, $41, $22, $1C, $00, $00),  // 29 )
    ($14, $08, $3E, $08, $14, $00),  // 2A *
    ($08, $08, $3E, $08, $08, $00),  // 2B +
    ($00, $00, $50, $30, $00, $00),  // 2C ,
    ($08, $08, $08, $08, $08, $00),  // 2D -
    ($00, $60, $60, $00, $00, $00),  // 2E .
    ($20, $10, $08, $04, $02, $01),  // 2F /
    ($3E, $51, $49, $45, $3E, $00),  // 30 0
    ($00, $42, $7F, $40, $00, $00),  // 31 1
    ($42, $61, $51, $49, $46, $00),  // 32 2
    ($21, $41, $45, $4B, $31, $00),  // 33 3
    ($18, $14, $12, $7F, $10, $00),  // 34 4
    ($27, $45, $45, $45, $39, $00),  // 35 5
    ($3C, $4A, $49, $49, $30, $00),  // 36 6
    ($01, $71, $09, $05, $03, $00),  // 37 7
    ($36, $49, $49, $49, $36, $00),  // 38 8
    ($06, $49, $49, $29, $1E, $00),  // 39 9
    ($00, $36, $36, $00, $00, $00),  // 3A :
    ($00, $56, $36, $00, $00, $00),  // 3B ;
    ($08, $14, $22, $41, $00, $00),  // 3C <
    ($14, $14, $14, $14, $14, $00),  // 3D =
    ($00, $41, $22, $14, $08, $00),  // 3E >
    ($02, $01, $51, $09, $06, $00),  // 3F ?
    ($32, $49, $79, $41, $3E, $00),  // 40 @
    ($7E, $11, $11, $11, $7E, $00),  // 41 A
    ($7F, $49, $49, $49, $36, $00),  // 42 B
    ($3E, $41, $41, $41, $22, $00),  // 43 C
    ($7F, $41, $41, $22, $1C, $00),  // 44 D
    ($7F, $49, $49, $49, $41, $00),  // 45 E
    ($7F, $09, $09, $09, $01, $00),  // 46 F
    ($3E, $41, $49, $49, $7A, $00),  // 47 G
    ($7F, $08, $08, $08, $7F, $00),  // 48 H
    ($00, $41, $7F, $41, $00, $00),  // 49 I
    ($20, $40, $41, $3F, $01, $00),  // 4A J
    ($7F, $08, $14, $22, $41, $00),  // 4B K
    ($7F, $40, $40, $40, $40, $00),  // 4C L
    ($7F, $02, $0C, $02, $7F, $00),  // 4D M
    ($7F, $04, $08, $10, $7F, $00),  // 4E N
    ($3E, $41, $41, $41, $3E, $00),  // 4F O
    ($7F, $09, $09, $09, $06, $00),  // 50 P
    ($3E, $41, $51, $21, $5E, $00),  // 51 Q
    ($7F, $09, $19, $29, $46, $00),  // 52 R
    ($46, $49, $49, $49, $31, $00),  // 53 S
    ($01, $01, $7F, $01, $01, $00),  // 54 T
    ($3F, $40, $40, $40, $3F, $00),  // 55 U
    ($1F, $20, $40, $20, $1F, $00),  // 56 V
    ($3F, $40, $30, $40, $3F, $00),  // 57 W
    ($63, $14, $08, $14, $63, $00),  // 58 X
    ($07, $08, $70, $08, $07, $00),  // 59 Y
    ($61, $51, $49, $45, $43, $00),  // 5A Z
    ($00, $7F, $41, $41, $00, $00),  // 5B [
    ($02, $04, $08, $10, $20, $00),  // 5C \
    ($00, $41, $41, $7F, $00, $00),  // 5D ]
    ($04, $02, $01, $02, $04, $00),  // 5E ^
    ($40, $40, $40, $40, $40, $00),  // 5F _
    ($00, $01, $02, $04, $00, $00),  // 60 `
    ($20, $54, $54, $54, $78, $00),  // 61 a
    ($7F, $48, $48, $48, $30, $00),  // 62 b
    ($38, $44, $44, $44, $44, $00),  // 63 c
    ($30, $48, $48, $48, $7F, $00),  // 64 d
    ($38, $54, $54, $54, $58, $00),  // 65 e
    ($00, $08, $7E, $09, $02, $00),  // 66 f
    ($48, $54, $54, $54, $3C, $00),  // 67 g
    ($7F, $08, $08, $08, $70, $00),  // 68 h
    ($00, $00, $7A, $00, $00, $00),  // 69 i
    ($20, $40, $40, $3D, $00, $00),  // 6A j
    ($00, $7F, $10, $28, $44, $00),  // 6B k
    ($00, $00, $41, $7F, $40, $00),  // 6C l
    ($78, $04, $38, $04, $78, $00),  // 6D m
    ($7C, $08, $04, $04, $78, $00),  // 6E n
    ($38, $44, $44, $44, $38, $00),  // 6F o
    ($7C, $14, $14, $14, $08, $00),  // 70 p
    ($08, $14, $14, $14, $7C, $00),  // 71 q
    ($7C, $08, $04, $04, $08, $00),  // 72 r
    ($48, $54, $54, $54, $24, $00),  // 73 s
    ($04, $04, $3F, $44, $24, $00),  // 74 t
    ($3C, $40, $40, $40, $3C, $00),  // 75 u
    ($1C, $20, $40, $20, $1C, $00),  // 76 v
    ($3C, $40, $30, $40, $3C, $00),  // 77 w
    ($44, $28, $10, $28, $44, $00),  // 78 x
    ($9C, $A0, $A0, $A0, $7C, $00),  // 79 y
    ($44, $64, $54, $4C, $44, $00),  // 7A z
    ($08, $36, $41, $41, $00, $00),  // 7B {
    ($00, $00, $77, $00, $00, $00),  // 7C |
    ($00, $41, $41, $36, $08, $00),  // 7D }
    ($08, $04, $04, $04, $02, $00),  // 7E ~
    ($00, $00, $00, $00, $00, $00),  // 7F (blank)
    ($80, $80, $80, $80, $80, $80),  // 80 (1x6 px)
    ($C0, $C0, $C0, $C0, $C0, $C0),  // 81 (2x6 px)
    ($E0, $E0, $E0, $E0, $E0, $E0),  // 82 (3x6 px)
    ($F0, $F0, $F0, $F0, $F0, $F0),  // 83 (4x6 px)
    ($F8, $F8, $F8, $F8, $F8, $F8),  // 84 (5x6 px)
    ($FC, $FC, $FC, $FC, $FC, $FC),  // 85 (6x6 px)
    ($FE, $FE, $FE, $FE, $FE, $FE),  // 86 (7x6 px)
    ($FF, $FF, $FF, $FF, $FF, $FF),  // 87 (8x6 px)
    ($FF, $00, $00, $00, $00, $00),  // 88 (1x8 px)
    ($FF, $FF, $00, $00, $00, $00),  // 89 (2x8 px)
    ($FF, $FF, $FF, $00, $00, $00),  // 8A (3x8 px)
    ($FF, $FF, $FF, $FF, $00, $00),  // 8B (4x8 px)
    ($FF, $FF, $FF, $FF, $FF, $00),  // 8C (5x8 px)
    ($55, $AA, $55, $AA, $55, $AA),  // 8D (50% pattern)
    ($AA, $55, $AA, $55, $AA, $55),  // 8E (50% pattern, inverse)
    ($10, $10, $10, $FF, $10, $10),  // 8F (+ lines)
    ($10, $10, $10, $1F, $10, $10),  // 90 (upside down T lines)
    ($10, $10, $10, $F8, $10, $10),  // 91 (T lines)
    ($10, $10, $10, $FF, $00, $00),  // 92 (-| lines)
    ($00, $00, $00, $FF, $10, $10),  // 93 (|- lines)
    ($01, $01, $01, $01, $01, $01),  // 94 ( upper lines)
    ($10, $10, $10, $10, $10, $10),  // 95 (middle lines -)
    ($00, $00, $00, $FF, $00, $00),  // 96 (middle lines |)
    ($00, $00, $00, $F0, $10, $10),  // 97 (right lines |)
    ($10, $10, $10, $F0, $00, $00),  // 98 (up-left corner lines)
    ($00, $00, $00, $1F, $10, $10),  // 99 (up-right corner lines)
    ($10, $10, $10, $1F, $00, $00),  // 9A (bottom-left corner lines)
    ($00, $00, $00, $E0, $10, $10),  // 9B (bottom-right corner lines)
    ($10, $10, $20, $C0, $00, $00),  // 9C (rounded up-left corner lines)
    ($00, $00, $00, $0F, $10, $10),  // 9D (rounded up-right corner lines)
    ($20, $20, $10, $0F, $00, $00),  // 9E (rounded bottom-left corner lines)
    ($78, $15, $14, $15, $78, $00),  // 9F (rounded bottom-right corner lines)
    ($78, $15, $14, $15, $78, $00),  // A0 Ä
    ($78, $14, $16, $15, $78, $00),  // A1
    ($0E, $51, $31, $11, $11, $00),  // A2 Á
    ($78, $16, $15, $16, $78, $00),  // A3
    ($7E, $09, $7E, $4A, $4A, $00),  // A4 Â
    ($7C, $54, $56, $55, $44, $00),  // A5
    ($7C, $55, $56, $54, $44, $00),  // A6 É
    ($7C, $56, $55, $56, $44, $00),  // A7 È
    ($00, $45, $7C, $45, $00, $00),  // A8 Ê
    ($00, $44, $7E, $45, $00, $00),  // A9
    ($00, $45, $7E, $44, $00, $00),  // AA Í
    ($00, $46, $7D, $46, $00, $00),  // AB Ì
    ($7A, $09, $13, $22, $79, $00),  // AC Î
    ($38, $45, $44, $45, $38, $00),  // AD Ö
    ($3E, $51, $49, $45, $3E, $00),  // AE
    ($38, $44, $46, $45, $38, $00),  // AF (O with /)
    ($38, $45, $46, $44, $38, $00),  // B0 Ó
    ($78, $15, $16, $14, $78, $00),  // B1 Ò
    ($7C, $55, $54, $55, $44, $00),  // B2 À
    ($FF, $01, $01, $49, $36, $00),  // B3 ß
    ($3C, $41, $40, $41, $3C, $00),  // B4
    ($3C, $41, $40, $41, $3C, $00),  // B5 Ü
    ($3C, $40, $42, $41, $3C, $00),  // B6 Ú
    ($3C, $41, $42, $40, $3C, $00),  // B7 Ù
    ($3C, $42, $41, $42, $3C, $00),  // B8 Û
    ($00, $02, $05, $02, $00, $00),  // B9 °
    ($00, $00, $00, $00, $00, $00),  // BA (PlusMinus)
    ($00, $00, $00, $00, $00, $00),  // BB (Division)
    ($08, $1C, $2A, $2A, $2A, $00),  // BC €
    ($4A, $55, $55, $29, $00, $00),  // BD §
    ($40, $3E, $20, $20, $1E, $00),  // BE µ
    ($00, $00, $00, $00, $00, $00),  // BF ô
    ($20, $55, $54, $55, $78, $00),  // C0 ä
    ($00, $00, $00, $00, $00, $00),  // C1
    ($00, $00, $00, $00, $00, $00),  // C2 á
    ($00, $00, $00, $00, $00, $00),  // C3
    ($00, $00, $00, $00, $00, $00),  // C4 â
    ($00, $00, $00, $00, $00, $00),  // C5
    ($00, $00, $00, $00, $00, $00),  // C6 é
    ($00, $00, $00, $00, $00, $00),  // C7 è
    ($00, $00, $00, $00, $00, $00),  // C8 ê
    ($00, $00, $00, $00, $00, $00),  // C9
    ($00, $00, $00, $00, $00, $00),  // CA í
    ($00, $00, $00, $00, $00, $00),  // CB ì
    ($00, $00, $00, $00, $00, $00),  // CC î
    ($00, $7A, $48, $7A, $00, $00),  // CD ö
    ($00, $00, $00, $00, $00, $00),  // CE
    ($00, $00, $00, $00, $00, $00),  // CF (o with /)
    ($00, $00, $00, $00, $00, $00),  // D0 ó
    ($00, $00, $00, $00, $00, $00),  // D1 ò
    ($00, $00, $00, $00, $00, $00),  // D2 à
    ($00, $00, $00, $00, $00, $00),  // D3
    ($00, $00, $00, $00, $00, $00),  // D4
    ($00, $7A, $40, $7A, $00, $00),  // D5 ü
    ($00, $00, $00, $00, $00, $00),  // D6 ú
    ($00, $00, $00, $00, $00, $00),  // D7 ù
    ($00, $00, $00, $00, $00, $00),  // D8 û
    ($00, $00, $00, $00, $00, $00),  // D9 (British Pund)
    ($00, $00, $00, $00, $00, $00),  // DA (alpha)
    ($00, $00, $00, $00, $00, $00),  // DB (beta)
    ($00, $00, $00, $00, $00, $00),  // DC (gamma)
    ($00, $00, $00, $00, $00, $00),  // DD (epsilon)
    ($00, $00, $00, $00, $00, $00),  // DE
    ($00, $00, $00, $00, $00, $00),  // DF ô
    ($00, $00, $00, $00, $00, $00),  // E0 (= lines)
    ($00, $00, $00, $00, $00, $00),  // E1 (|= lines)
    ($00, $00, $00, $00, $00, $00),  // E2 (= lines with middle |)
    ($00, $00, $00, $00, $00, $00),  // E3 (=| lines)
    ($00, $00, $00, $00, $00, $00),  // E4 (bottom-right triangle)
    ($00, $00, $00, $00, $00, $00),  // E5 (bottom-left triangle)
    ($00, $00, $00, $00, $00, $00),  // E6 (upper-right triangle)
    ($00, $00, $00, $00, $00, $00),  // E7 (upper-left triangle)
    ($00, $00, $00, $00, $00, $00),  // E8 (filled diamond)
    ($00, $00, $00, $00, $00, $00),  // E9 (diamond frame)
    ($00, $00, $00, $00, $00, $00),  // EA (6x6 box)
    ($00, $00, $00, $00, $00, $00),  // EB (6x6 frame)
    ($00, $00, $00, $00, $00, $00),  // EC (6x6 filled circle)
    ($00, $00, $00, $00, $00, $00),  // ED (6x6 circle)
    ($00, $00, $00, $00, $00, $00),  // EE (/ lines)
    ($00, $00, $00, $00, $00, $00),  // EF (\ lines)
    ($00, $00, $00, $00, $00, $00),  // F0 (X lines)
    ($00, $00, $00, $00, $00, $00),  // F1 (less-equal)
    ($00, $00, $00, $00, $00, $00),  // F2 (greater-equal)
    ($00, $00, $00, $00, $00, $00),  // F3 (not-equal)
    ($00, $00, $00, $00, $00, $00),  // F4 (up arrow)
    ($00, $00, $00, $00, $00, $00),  // F5 (down arrow)
    ($00, $00, $00, $00, $00, $00),  // F6 (left arrow)
    ($00, $00, $00, $00, $00, $00),  // F7 (right arrow)
    ($00, $00, $00, $00, $00, $00),  // F8 (bottom-left L lines)
    ($00, $00, $00, $00, $00, $00),  // F9 (upper-left L lines)
    ($00, $00, $00, $00, $00, $00),  // FA (bottom-right L lines)
    ($00, $00, $00, $00, $00, $00),  // FB (upper-right L lines)
    ($00, $00, $00, $00, $00, $00),  // FC
    ($00, $00, $00, $00, $00, $00),  // FD
    ($00, $00, $00, $00, $00, $00),  // FE
    ($00, $00, $00, $00, $00, $00)   // FF
    );


implementation

class function TGlyphs.Adapt2Charmap(s: String): String;
var
  R: String;
begin
  R := S;

  R := R.Replace('Ä', Chr($A0), [rfReplaceAll]);
  R := R.Replace('ä', Chr($C0), [rfReplaceAll]);
  R := R.Replace('Ö', Chr($AD), [rfReplaceAll]);
  R := R.Replace('ö', Chr($CD), [rfReplaceAll]);
  R := R.Replace('Ü', Chr($B5), [rfReplaceAll]);
  R := R.Replace('ü', Chr($D5), [rfReplaceAll]);

  R := R.Replace('ß', Chr($B3), [rfReplaceAll]);

  R := R.Replace('€', Chr($BC), [rfReplaceAll]);

  R := R.Replace('µ', Chr($BE), [rfReplaceAll]);
  R := R.Replace('°', Chr($B9), [rfReplaceAll]);
  R := R.Replace('§', Chr($BD), [rfReplaceAll]);

  Result := R;
end;

end.
