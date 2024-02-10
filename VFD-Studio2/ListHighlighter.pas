unit ListHighlighter;
(*
  This is an example how to implement your own highlighter.

  This example extends the Simple and Context HL:
  - The token -(- and -)- (must be surrounded by space or line-begin/end to be
    a token of their own) will add foldable sections

    Multply -(- and -)- can be nested.

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter,
  SynEditHighlighterFoldBase, StudioCommon;
type

  (*   This is an EXACT COPY of SynEditHighlighter

       ONLY the base class is changed to add support for folding

       The new code follows below
  *)

  TListHighlighter = class(TSynCustomFoldHighlighter)
  private
  protected
    // accesible for the other examples
    FCommandsList: TStringList;
    FKeywordsList: TStringList;
    FTokenPos, FTokenEnd: Integer;
    FLineText: String;
    FCommentAttri: TSynHighlighterAttributes;
    FCommandAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    procedure SetCommentAttri(AValue: TSynHighlighterAttributes);
    procedure SetTextAttri(AValue: TSynHighlighterAttributes);
    procedure SetCommandAttri(AValue: TSynHighlighterAttributes);
    procedure SetNumberAttri(AValue: TSynHighlighterAttributes);
    procedure SetKeywordAttri(AValue: TSynHighlighterAttributes);
    procedure SetStringAttri(AValue: TSynHighlighterAttributes); public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override; public
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: Integer; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Define attributes, for the different highlights. *)
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write SetCommentAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write SetKeywordAttri;
    property CommandAttri: TSynHighlighterAttributes read FCommandAttri write SetCommandAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri write SetTextAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write SetNumberAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write SetStringAttri;
  end;

  (*   This is a COPY of SynEditHighlighter

       ONLY the base class is changed to add support for folding

       The new code follows below
  *)

  TListFoldHighlighter = class(TListHighlighter)
  protected
    FCurRange: Integer;
  public
    procedure Next; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override; public
  (* The below needed to be changed and are in TSynDemoHlFold
     TListFoldHighlighter uses Ranges itself.
     The Range needed here is therefore stored in a diff location
  *)
    //procedure SetRange(Value: Pointer); override;
    //procedure ResetRange; override;
    //function GetRange: Pointer; override;
  end;

  { TSynDemoHlContext }

  (* You can base this on either
     TListHighlighter or TListFoldHighlighter

     Using ranges is NOT a condition for fold.
     (If changing, remove Range related code)

     Note that ranges to change.
  *)

  //TSynDemoHlFold = class(TListHighlighter)
  TSynDemoHlFold = class(TListFoldHighlighter)
  public
    procedure Next; override; public
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
  end;

implementation

{ TSynDemoHlFold }

procedure TSynDemoHlFold.Next;
begin
  inherited Next;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'NEWSCREEN') then
    StartCodeFoldBlock(nil);
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'ENDSCREEN') then
    EndCodeFoldBlock;
end;

procedure TSynDemoHlFold.SetRange(Value: Pointer);
begin
  // must call the SetRange in TSynCustomFoldHighlighter
  inherited SetRange(Value);
  FCurRange := PtrInt(CodeFoldRange.RangeType);
end;

procedure TSynDemoHlFold.ResetRange;
begin
  inherited ResetRange;
  FCurRange := 0;
end;

function TSynDemoHlFold.GetRange: Pointer;
begin
  // Store the range first
  CodeFoldRange.RangeType := Pointer(PtrInt(FCurRange));
  Result := inherited GetRange;
end;


(*   This is an EXACT COPY of SynEditHighlighter

     ONLY the base class is changed to add support for folding
*)

destructor TListHighlighter.Destroy;
begin
  FCommandsList.Free;
  FKeywordsList.Free;
  inherited;
end;

constructor TListHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommandsList := TStringList.Create;
  FKeywordsList := TStringList.Create;

  FCommandsList.Delimiter := ' ';
  FCommandsList.DelimitedText := STUDIO_COMMANDS;
  FKeywordsList.Delimiter := ' ';
  FKeywordsList.DelimitedText := STUDIO_KEYWORDS_STATIC + ' ' + STUDIO_KEYWORDS_VARIABLE;

  (* Create and initialize the attributes *)
  FCommentAttri := TSynHighlighterAttributes.Create('comment', 'comment');
  AddAttribute(FCommentAttri);
  FCommentAttri.Foreground := clGray;

  FKeywordAttri := TSynHighlighterAttributes.Create('keyword', 'keyword');
  AddAttribute(FKeywordAttri);
  FKeywordAttri.Foreground := clGreen;
  //FKeywordAttri.Style := [fsItalic];

  FCommandAttri := TSynHighlighterAttributes.Create('command', 'command');
  AddAttribute(FCommandAttri);
  FCommandAttri.Style := [fsBold];

  FTextAttri := TSynHighlighterAttributes.Create('text', 'text');
  AddAttribute(FTextAttri);

  FNumberAttri := TSynHighlighterAttributes.Create('number', 'number');
  AddAttribute(FNumberAttri);
  FNumberAttri.Foreground := clBlue;

  FStringAttri := TSynHighlighterAttributes.Create('string', 'string');
  AddAttribute(FStringAttri);
  FStringAttri.Foreground := clBlue;

  // Ensure the HL reacts to changes in the attributes. Do this once, if all attributes are created
  SetAttributesOnChange(@DefHighlightChange);
end;

(* Setters for attributes / This allows using in Object inspector*)
procedure TListHighlighter.SetCommentAttri(AValue: TSynHighlighterAttributes);
begin
  FCommentAttri.Assign(AValue);
end;

procedure TListHighlighter.SetTextAttri(AValue: TSynHighlighterAttributes);
begin
  FTextAttri.Assign(AValue);
end;

procedure TListHighlighter.SetCommandAttri(AValue: TSynHighlighterAttributes);
begin
  FCommandAttri.Assign(AValue);
end;

procedure TListHighlighter.SetNumberAttri(AValue: TSynHighlighterAttributes);
begin
  FNumberAttri.Assign(AValue);
end;

procedure TListHighlighter.SetKeywordAttri(AValue: TSynHighlighterAttributes);
begin
  FKeywordAttri.Assign(AValue);
end;

procedure TListHighlighter.SetStringAttri(AValue: TSynHighlighterAttributes);
begin
  FStringAttri.Assign(AValue);
end;

procedure TListHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TListHighlighter.Next;
var
  l: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  l := length(FLineText);
  if FTokenPos > l then
    // At line end
    Exit
  else


  if FLineText[FTokenEnd] = '$' then
  begin
    // At start of keyword? Find end of keyword
    repeat
      Inc(FTokenEnd);
    until (FTokenEnd > l) or (FLineText[FTokenEnd] = '$'); //not(FLineText[FTokenEnd] in [#32..#35, #37..#255])
    if (FLineText[FTokenEnd] = '$') then
      Inc(FTokenEnd);
  end
  else
  if FLineText[FTokenEnd] in [#9, ' '] then
    // At Space? Find end of spaces
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in [#0..#32]) do
    begin
      Inc(FTokenEnd);
    end
  else
    // At something else? Find end of something else
    while (FTokenEnd <= l) and not (FLineText[FTokenEnd] in [#9, ' ', '$']) do
    begin
      Inc(FTokenEnd);
    end;
end;

function TListHighlighter.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure TListHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TListHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  TokenWord: String;
  S: String;
  SearchWord: String;
  Dummy: Integer;
begin

  // Match the text, specified by FTokenPos and FTokenEnd
  TokenWord := Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);

  Result := TextAttri; // default result

  if Trim(FLineText).StartsWith(';') then
    Result := CommentAttri
  else
  if (TokenWord.StartsWith('$')) and (TokenWord.EndsWith('$')) then
  begin
    S := UpperCase(TokenWord).Replace('$', '', [rfReplaceAll]);
    for SearchWord in FKeywordsList do
    begin
      if S.StartsWith(SearchWord) then
      begin
        Result := KeywordAttri;
        Break;
      end;
    end;
  end
  else
  if TryStrToInt(TokenWord, Dummy) then
    Result := NumberAttri
  else
  begin
    if (Trim(FLineText).StartsWith(Trim(TokenWord))) then begin
      S := UpperCase(TokenWord);
      for SearchWord in FCommandsList do
      begin
        if S.StartsWith(SearchWord) then
        begin
          Result := CommandAttri;
          Break;
        end;
      end;
    end;
  end;

end;

function TListHighlighter.GetToken: String;
begin
  Result := Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TListHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TListHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FCommandAttri;
    SYN_ATTR_WHITESPACE: Result := FTextAttri;
    else
      Result := nil;
  end;
end;

function TListHighlighter.GetTokenKind: Integer;
var
  a: TSynHighlighterAttributes;
begin
  // Map Attribute into a unique number
  a := GetTokenAttribute;
  Result := 0;
  if a = FNumberAttri then Result := 1;
  if a = FKeywordAttri then Result := 2;
  if a = FTextAttri then Result := 3;
  if a = FCommandAttri then Result := 4;
  if a = FCommentAttri then Result := 5;
  if a = FStringAttri then Result := 6;
end;


(*   This is an EXACT COPY of SynEditHighlighter

     ONLY the base class is changed to add support for folding
*)

procedure TListFoldHighlighter.Next;
var
  TokenWord: String;
begin
  inherited Next;
  TokenWord := UpperCase(Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos));
  if (TokenWord = 'NEWSCREEN') then
    Inc(FCurRange);
  if (TokenWord = 'ENDSCREEN') and (FCurRange > 0) then
    Dec(FCurRange);
end;

function TListFoldHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  (*
  if (Result = CommandAttri) and (FCurRange > 0) then
    Result:= FTextAttri;
  *)
end;


end.
