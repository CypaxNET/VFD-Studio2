unit ListEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, StudioCommon, Math, PreviewDisplay, SysInfo, uSMBIOS, WinampControl,
  lclintf, Buttons, Spin, Menus, ComCtrls, SynEdit, SynCompletion,
  ListHighlighter, SynEditMarkupSpecialLine, SynEditTypes, LConvEncoding,
  RegExpr;
const
  VERSION_STR = '2.0.0.0';
  APP_TITLE = 'List Editor 2';

type

  { TMainForm }

  TMainForm = class(TForm)
    HelpTextMemo: TMemo;
    Editor: TSynEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    RedoMenuItem: TMenuItem;
    ZoomInButton: TSpeedButton;
    ZoomOutButton: TSpeedButton;
    UndoMenuItem: TMenuItem;
    RedoButton: TSpeedButton;
    UndoButton: TSpeedButton;
    ZoomOutMenuItem: TMenuItem;
    ZoomInMenuItem: TMenuItem;
    NewFileMenuItem: TMenuItem;
    PreviewImage: TImage;
    SaveMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    NewFileButton: TSpeedButton;
    OpenButton: TSpeedButton;
    SaveButton: TSpeedButton;
    StatusBar1: TStatusBar;
    SynCompletion1: TSynCompletion;
    ToolBar1: TToolBar;
    ToolListBox: TListBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PreviewGroupBox: TGroupBox;
    ToolGroupBox: TGroupBox;
    ImageList1: TImageList;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure MenuItem2Click(Sender: TObject);
    procedure NewFileButtonClick(Sender: TObject);
    procedure NewFileMenuItemClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PreviewGroupBoxClick(Sender: TObject);
    procedure RedoButtonClick(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SynCompletion1Execute(Sender: TObject);
    procedure SynCompletion1SearchPosition(var APosition: Integer);
    procedure LoadConfig(const AFilePath: String);
    procedure ToolListBoxSelectionChange(Sender: TObject; User: Boolean);

    procedure LoadListFromFile(FileName: TFileName);

    procedure ShowHelpText(Cmd: String);

    procedure AddAutocompleteWords(Sender: TObject);
    procedure UndoButtonClick(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomInMenuItemClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure ZoomOutMenuItemClick(Sender: TObject);

    function CheckLine(Line: String): String;

  private

    // settings
    FStudioConfig: TStudioConfig;

    FPreviewDisplay: TPreviewDisplay; // preview display object

    // objects to gather system information from
    FSysInfo: TSysInfo;
    FSMBios: TSMBios;
    FWinampControl: TWinampControl;

    FSynHighlighter: TSynDemoHlFold;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

resourcestring

  RsCharacters = 'Characters';


  RsHelpCOMMENT = 'Inserts a comment text into the list.' + #13#10 +
    'Comments will not processed and are for your convenience; for example to describe individual commands or blocks of commands.';

  RsHelpNEWSCREEN = 'The representations on the display are divided into individual sections, called "Screen".' + #13#10 +
    'This command starts a new Screen. Typically, this command stops all animations, effects, updateable information, etc.' + #13#10 +
    'You can specify whether the new Screen should continue those settings, whether the screen should only be displayed once, ' +
    'or if it is subject to a certain pre-condition to be displayed at all.';

  RsHelpBITMAP = 'Draws an image from a file to the display at given coordinates.';


  { TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFilePath: String;
begin
  Randomize;

  FSynHighlighter := TSynDemoHlFold.Create(Self);
  Editor.Highlighter := FSynHighlighter;

  //FSMBios := TSMBios.Create;
  //  FSysInfo:= TSysInfo.Create(Self);
  FWinampControl := TWinampControl.Create(Self);
  FPreviewDisplay := TPreviewDisplay.Create(Self);

  FPreviewDisplay.LayerMode := lmXOR;

  IniFilePath := ExtractFilePath(application.ExeName) + 'vfdstudio.ini';
  LoadConfig(IniFilePath);

  ToolListBox.Items.Clear;
  ToolListBox.Items.Delimiter := ' ';
  ToolListBox.Items.DelimitedText := STUDIO_COMMANDS;
  ToolListBox.Items.Insert(0, 'COMMENT');
  ToolListBox.Selected[0]:= True;

  SynCompletion1.ItemList.Delimiter := ' ';
  SynCompletion1.ItemList.DelimitedText := STUDIO_COMMANDS + ' ' + STUDIO_KEYWORDS_STATIC + ' ' + STUDIO_KEYWORDS_VARIABLE;

  PreviewImage.Width := FStudioConfig.DisplayConfig.ResX;
  PreviewImage.Height := FStudioConfig.DisplayConfig.ResY;
  PreviewImage.Picture.Bitmap.Width := FStudioConfig.DisplayConfig.ResX;
  PreviewImage.Picture.Bitmap.Height := FStudioConfig.DisplayConfig.ResY;
  FPreviewDisplay.SetSize(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);

  LoadListFromFile(ExtractFilePath(Application.ExeName) + 'Lists\' + FStudioConfig.ListConfig.ListName);

  Caption := APP_TITLE + ' - ' + FStudioConfig.ListConfig.ListName;

end;

procedure TMainForm.ToolListBoxSelectionChange(Sender: TObject; User: Boolean);
var
  ItemString: String;
begin
  ItemString := ToolListBox.GetSelectedText;
  ShowHelpText(ItemString);
end;

procedure TMainForm.ShowHelpText(Cmd: String);
begin
  HelpTextMemo.Lines.Clear;
  if (Cmd.StartsWith('NEWSCREEN')) then
  begin
    HelpTextMemo.Lines.Add(RsHelpNEWSCREEN);
  end
  else
  if (Cmd.StartsWith('COMMENT')) then
  begin
    HelpTextMemo.Lines.Add(RsHelpCOMMENT);
  end
  else
  if (Cmd.StartsWith('PIXEL')) then
  begin
    //HelpTextMemo.Lines.Add(RsHelpPIXEL);
  end
  else
  if (Cmd.StartsWith('Bitmap')) then
  begin
    HelpTextMemo.Lines.Add(RsHelpBITMAP);
  end;

end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar1.Panels.Items[0].Text:= '[' + Inttostr(Editor.CaretY)+':'+Inttostr(Editor.CaretX)+']' + '    Sel: ' + InttoStr(Length(Editor.SelText));
  StatusBar1.Panels.Items[1].Text:= InttoStr(Length(Editor.Text)) + ' ' + RsCharacters;
  if (Editor.InsertMode) then
    StatusBar1.Panels.Items[3].Text:= 'INS'
  else
    StatusBar1.Panels.Items[3].Text:= 'OVR';

  if (Editor.CaretY > 0) then
    StatusBar1.Panels[4].Text:= CheckLine(Editor.Lines[Editor.CaretY - 1]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSMBios.Free;
  //FSysInfo.Free;
  FWinampControl.Free;
  FPreviewDisplay.Free;
  FSynHighlighter.Free;
end;

procedure TMainForm.EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if (Line > 0) and (CheckLine(Editor.Lines[Line - 1]) <> 'ok') then begin
    Special := True;
    BG := $C1C1FF;
  end else if (Line = Editor.BlockBegin.Y) then
  begin
    Special := True;
    BG := Editor.LineHighlightColor.Background;
  end;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  OpenButtonClick(Sender);
end;

procedure TMainForm.NewFileButtonClick(Sender: TObject);
begin
  Editor.Lines.Clear;
end;

procedure TMainForm.NewFileMenuItemClick(Sender: TObject);
begin
  NewFileButtonClick(Sender);
end;

procedure TMainForm.LoadListFromFile(FileName: TFileName);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    SL.Text := ConvertEncoding(SL.Text, GuessEncoding(SL.Text), EncodingUTF8);
    Editor.Lines.Text := SL.Text;
    Caption := APP_TITLE + ' - ' + ExtractFileName(FileName);
  finally
    SL.Free;
  end;
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  if (OpenDialog.Execute) then
  begin
    LoadListFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.PreviewGroupBoxClick(Sender: TObject);
begin

end;

procedure TMainForm.RedoButtonClick(Sender: TObject);
begin
  if Editor.CanRedo then
    Editor.Redo;
end;

procedure TMainForm.RedoMenuItemClick(Sender: TObject);
begin
  RedoButtonClick(Sender);
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if (SaveDialog.Execute) then
  begin
    Editor.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
  SaveButtonClick(Sender);
end;

procedure TMainForm.SynCompletion1Execute(Sender: TObject);
begin
  SynCompletion1.ItemList.Clear;
  AddAutocompleteWords(Self);
end;

procedure TMainForm.SynCompletion1SearchPosition(var APosition: Integer);
begin
  SynCompletion1.ItemList.Clear;
  AddAutocompleteWords(Self);
  if SynCompletion1.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TMainForm.AddAutocompleteWords(Sender: TObject);
var
  StringList: TStringList;
  AString: String;

  procedure Add(S: String);
  begin
    if Pos(LowerCase(SynCompletion1.CurrentString), LowerCase(S)) = 1 then
      SynCompletion1.ItemList.Add(S);
  end;
begin
  StringList := TStringList.Create;
  StringList.Delimiter := ' ';
  StringList.DelimitedText := STUDIO_COMMANDS + ' ' +
    STUDIO_KEYWORDS_STATIC + ' ' + STUDIO_KEYWORDS_VARIABLE;
  StringList.Sort;

  for AString in StringList do
  begin
    Add(AString);
  end;

  StringList.Free;
end;

procedure TMainForm.UndoButtonClick(Sender: TObject);
begin
  if Editor.CanUndo then
    Editor.Undo;
end;

procedure TMainForm.UndoMenuItemClick(Sender: TObject);
begin
  UndoButtonClick(Sender);
end;

procedure TMainForm.ZoomInButtonClick(Sender: TObject);
begin
  Editor.Font.Size:= Editor.Font.Size + 1;
end;

procedure TMainForm.ZoomInMenuItemClick(Sender: TObject);
begin
  ZoomInButtonClick(Sender);
end;

procedure TMainForm.ZoomOutButtonClick(Sender: TObject);
begin
  if (Editor.Font.Size > 8) then
    Editor.Font.Size:= Editor.Font.Size - 1;
end;

procedure TMainForm.ZoomOutMenuItemClick(Sender: TObject);
begin
  ZoomOutButtonClick(Sender);
end;

procedure TMainForm.LoadConfig(const AFilePath: String);
var
  IniFile: TIniFile;
  ColorString: String;
begin
  try
    IniFile := TIniFile.Create(AFilePath);

    { application section }
    FStudioConfig.ApplicationConfig.Language := IniFile.ReadString('APPLICATION', 'Language', 'en');
    ColorString := IniFile.ReadString('APPLICATION', 'DspColor', '$FFFFFF');
    ColorString := ColorString.Replace('#', '$');
    FStudioConfig.ApplicationConfig.PreviewDisplayColor := StringToColor(ColorString);
    FStudioConfig.ApplicationConfig.DoStartMinimized := IniFile.ReadBool('APPLICATION', 'StartMinimized', False);

    { display section }
    FStudioConfig.DisplayConfig.DisplayType := IniFile.ReadString('DISPLAY', 'Type', '');
    FStudioConfig.DisplayConfig.ResX := IniFile.ReadInteger('DISPLAY', 'ResX', 128);
    FStudioConfig.DisplayConfig.ResY := IniFile.ReadInteger('DISPLAY', 'ResY', 64);
    FStudioConfig.DisplayConfig.IntName := IniFile.ReadString('DISPLAY', 'Interface', 'COM1');
    FStudioConfig.DisplayConfig.Baudrate := IniFile.ReadInteger('DISPLAY', 'Baud', 115200);
    FStudioConfig.DisplayConfig.DoClearOnExit := IniFile.ReadBool('DISPLAY', 'ClearOnExit', False);
    FStudioConfig.DisplayConfig.IsBrightnessControlledByList := IniFile.ReadBool('DISPLAY', 'BrightnessByList', True);
    FStudioConfig.DisplayConfig.DisplayBrightness := Min(100, IniFile.ReadInteger('DISPLAY', 'Brightness', 100));

    { list section }
    FStudioConfig.ListConfig.ListName := IniFile.ReadString('LIST', 'Listname', 'Default.vfdlst');

    { animations section }
    FStudioConfig.AnimationConfig.PlayOnlyOnIdle := IniFile.ReadBool('ANIMATIONS', 'PlayOnlyOnIdle', True);
    FStudioConfig.AnimationConfig.IdlePercent := Min(100, IniFile.ReadInteger('ANIMATIONS', 'IdleLevel', 50));

  finally
    IniFile.Free;
  end;
end;

function TMainForm.CheckLine(Line: String): String;
var
  S: String;
  Res: String;
  SL: TStringList;
  Cmd: String;
  I: Integer;
  Valid: Boolean;
  RegEx: TRegExpr;
  Match: String;
  Value: Integer;
begin
  Res:= 'not ok';
  S:= Trim(Line);
  SL:= TStringList.Create;
  SL.Delimiter:= ' ';
  SL.QuoteChar:= '''';
  SL.DelimitedText:= S;

  RegEx:= TRegExpr.Create;

  if (S.IsEmpty) or (S.StartsWith(';')) then begin
    Res:= 'ok';
  end else begin
    // check if it is a known command
    Cmd:= UpperCase(SL[0]);
    Valid:= False;
    for I:= 0 to ToolListBox.Items.Count - 1 do begin
      if (Cmd = ToolListBox.Items[I]) then begin
        Valid:= True;
        Break;
      end;
    end;
    if (not Valid) then begin
      Res:= 'unknown command "' + Cmd + '"';
    end else begin
      // plausibility check of commands

      if ('NEWSCREEN' = CMD) then begin
        // should have max. one parameter ONCE, CONTINUE REQUIRE*
        RegEx.Expression:= '^NEWSCREEN(\s+.+)?$';
        if (RegEx.Exec(S)) then begin
          if (RegEx.SubExprMatchCount = 1) then begin
            Match:= Trim(RegEx.Match[1]);
            if (Match = '') or (Match = 'ONCE') or (Match = 'CONTINUE') or (Match = 'REQUIREWINAMP') then begin
              Res:= 'ok'
            end else begin
              Res:= 'possible NEWSCREEN parameters are ONCE, CONTINUE, REQUIREWINAMP';
            end;
          end else begin
            Res:= 'NEWSCREEN should have max. one parameter';
          end;
        end else begin
          Res:= 'invalid NEWSCREEN syntax';
        end;
      end

      else if ('SCREENEND' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^SCREENEND$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'SCREENEND should have no parameters';
        end;
      end

      else if ('CLEARSCREEN' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^CLEARSCREEN$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'CLEARSCREEN should have no parameters';
        end;
      end

      else if ('STOP' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^STOP$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'STOP should have no parameters';
        end;
      end

      else if ('ORMODE' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^ORMODE$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'ORMODE should have no parameters';
        end;
      end

      else if ('XORMODE' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^XORMODE';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'XORMODE should have no parameters';
        end;
      end

      else if ('ANDMODE' = CMD) then begin
        // should have no parameters
        RegEx.Expression:= '^ANDMODE';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok';
        end else begin
          Res:= 'ANDMODE should have no parameters';
        end;
      end

      else if ('LIGHT' = CMD) then begin
        // should have one number parameter
        RegEx.Expression:= '^LIGHT(\s+\d+)$';
        if (RegEx.Exec(S)) then begin
          Match:= RegEx.Match[1];
          Value:= StrToInt(Match);
          if (Value >=0) and (Value <= 100) then
            Res:= 'ok'
          else
            Res:= 'LIGHT parameter must be 0..100';
        end else begin
          Res:= 'LIGHT should have one number parameter 0..100';
        end;
      end

      else if ('SCREENTIME' = CMD) then begin
        // should have one number parameter
        RegEx.Expression:= '^SCREENTIME(\s+\d+)$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'SCREENTIME should have one number parameter';
        end;
      end

      else if ('THEMATRIX' = CMD) then begin
        // should have one number parameter
        RegEx.Expression:= '^THEMATRIX(\s+\d+)$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'THEMATRIX should have one number parameter';
        end;
      end

      else if ('NOISE' = CMD) then begin
        // should have one number parameter
        RegEx.Expression:= '^NOISE(\s+\d+)$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'NOISE should have one number parameter';
        end;
      end

      else if ('CPUMONITOR' = CMD) then begin
        // should have two number parameters
        RegEx.Expression:= '^CPUMONITOR(\s+\d+){2}$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'CPUMONITOR should have two number parameters';
        end;
      end

      else if ('RAMMONITOR' = CMD) then begin
        // should have two number parameters
        RegEx.Expression:= '^RAMMONITOR(\s+\d+){2}$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'RAMMONITOR should have two number parameters';
        end;
      end


      else if ('PIXEL' = CMD) then begin
        // should have two number parameters and an optional inverted marker
        RegEx.Expression:= '^PIXEL(\s+[+-]?\d+){2}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then begin
          Res:= 'ok'
        end else begin
          Res:= 'PIXEL should have two number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else if ('LINE' = CMD) then begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression:= '^LINE(\s+[+-]?\d+){4}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then begin
          Res:= 'ok'
        end else begin
          Res:= 'LINE should have four number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else if ('FRAME' = CMD) then begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression:= '^FRAME(\s+[+-]?\d+){4}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then begin
          Res:= 'ok'
        end else begin
          Res:= 'FRAME should have four number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else if ('CLOCK' = CMD) then begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression:= '^CLOCK(\s+[+-]?\d+){6}?$';
        if (RegEx.Exec(UpperCase(S))) then begin
          Res:= 'ok'
        end else begin
          Res:= 'CLOCK should have six number parameters';
        end;
      end

      else if ('PLAINTEXT' = CMD) then begin
        // should have a text parameter followed by two number parameters
        RegEx.Expression:= '^PLAINTEXT\s+\''(.*)\''(\s+\d+){2}$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'PLAINTEXT should have a text parameter followed by two number parameters';
        end;
      end

      else if ('TEXTOUT' = CMD) then begin
        // should have a text parameter followed by three number parameters and another text parameter for the font name
        RegEx.Expression:= '^TEXTOUT\s+\''(.*)\''(\s+\d+){3}\s+\''(.*)\''$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'TEXTOUT should have a text parameter followed by three number parameters and another text parameter';
        end;
      end

      else if ('ANIMATE' = CMD) then begin
        // should have a text parameter followed by four number parameters
        RegEx.Expression:= '^ANIMATE\s+\''(.*)\''(\s+\d+)(\s+[+-]?\d+){2}(\s+\d+)$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'ANIMATE should have a text parameter followed by four number parameters';
        end;
      end

      else if ('BITMAP' = CMD) then begin
        // should have a text parameter followed by two number parameters
        RegEx.Expression:= '^BITMAP\s+\''(.*)\''(\s+[+-]?\d+){2}$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'BITMAP should have a text parameter followed by two number parameters';
        end;
      end

      else if ('DRIVEUSAGE' = CMD) then begin
        // should have a char parameter followed by three number parameters
        RegEx.Expression:= '^DRIVEUSAGE\s+\''(\w)\''(\s+\d+){3}$';
        if (RegEx.Exec(S)) then begin
          Res:= 'ok'
        end else begin
          Res:= 'DRIVEUSAGE should have a single character text parameter followed by three number parameters';
        end;
      end


    end; // is known command
  end; // is not empty or comment

  RegEx.Free;
  SL.Free;
  Result:= Res;
end;

end.
