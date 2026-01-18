unit ListEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, StudioCommon, Math, PreviewDisplay, uSMBIOS, SysInfo, WinampControl,
  lclintf, Buttons, Menus, ComCtrls, SynEdit, SynCompletion, Glyphs,
  ListHighlighter, SynEditTypes, LConvEncoding, MultiMon,
  RegExpr, DateUtils, LCLTranslator, LCLType, ExtDlgs, DisplayManager;
const
  APP_TITLE = 'List Editor 2';

  DOUBLESCALE_OFFSET = 16; // some minimum width spacing around the preview image when rescaling


type
  TExpression = record
    Expr: String;
    Help: String;
    Example: String;
  end;

type

  { TMainForm }

  TMainForm = class(TForm)
    KeywordListBox: TListBox;
    Editor: TSynEdit;
    ExampleEdit: TSynEdit;
    ExampleLabel: TLabel;
    MainMenu1: TMainMenu;
    HelpTextMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    CommentLinesMenuItem: TMenuItem;
    InsertPlaintextMenuItem: TMenuItem;
    InsertTextMenuItem: TMenuItem;
    InsertBitmapMenuItem: TMenuItem;
    InsertAnimationMenuItem: TMenuItem;
    Separator1: TMenuItem;
    NewScreenMenuItem: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    UnCommentMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    CloseButton: TSpeedButton;
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
    CommandListBox: TListBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PreviewGroupBox: TGroupBox;
    ToolGroupBox: TGroupBox;
    ImageList1: TImageList;
    procedure CloseButtonClick(Sender: TObject);
    procedure CommentLinesMenuItemClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure FormResize(Sender: TObject);
    procedure InsertAnimationMenuItemClick(Sender: TObject);
    procedure InsertBitmapMenuItemClick(Sender: TObject);
    procedure InsertPlaintextMenuItemClick(Sender: TObject);
    procedure InsertTextMenuItemClick(Sender: TObject);
    procedure KeywordListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure MenuItem2Click(Sender: TObject);
    procedure NewFileButtonClick(Sender: TObject);
    procedure NewFileMenuItemClick(Sender: TObject);
    procedure NewScreenMenuItemClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure RedoButtonClick(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SynCompletion1BeforeExecute(ASender: TSynBaseCompletion;
      var ACurrentString: String; var APosition: Integer; var AnX,
      AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    procedure SynCompletion1Execute(Sender: TObject);
    procedure SynCompletion1SearchPosition(var APosition: Integer);
    procedure LoadConfig(const AFilePath: String);
    procedure CommandListBoxSelectionChange(Sender: TObject; User: Boolean);

    procedure LoadListFromFile(FileName: TFileName);

    procedure ShowCommandExplanation(Cmd: String);
    procedure ShowKeywordExplanation(Cmd: String);

    procedure AddAutocompleteWords(Sender: TObject);
    procedure UnCommentMenuItemClick(Sender: TObject);
    procedure UndoButtonClick(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomInMenuItemClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure ZoomOutMenuItemClick(Sender: TObject);

    procedure ChangeMemoHeightToFitLines(Memo: TMemo);
    function AutoSizeMemoY(Memo: TMemo): Word;


    procedure CommentCurrentLine;
    procedure UncommentCurrentLine;

    function CheckLine(Line: String): String;

    procedure PreviewLine(S: String);

    procedure DrawDriveUsage(DriveLetter: Char; Col, Row, BarWidth: Integer);

    function FindCommandIndex(const Expr: String): Integer;
    function FindKeywordIndex(const Expr: String): Integer;

    procedure SaveWindowState;
    procedure RestoreWindowState;

    // methods related to the preview image
    procedure HandlePreviewImageUpdate(NewImage: TBitmap);

  private

    // settings
    FStudioConfig: TStudioConfig;

    // display stuff
    FDisplayMgr: TDisplayManager;

    // objects to gather system information from
    FSysInfo: TSysInfo;
    FWinampControl: TWinampControl;

    FSynHighlighter: TSynDemoHlFold;
    FExampleHighlighter: TSynDemoHlFold;


    FLastLine: String;

    FFileName: String;

    IsCpuMonitorDisplayed: Boolean; // is the CPU usage monitor currently displayed?
    IsMemMonitorDisplayed: Boolean; // is the RAM usage monitor currently displayed?



  public

  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}


resourcestring
  RsCharacters = 'Characters';

  RsDiscardTitle = 'You have unsaved changes.';
  RsDiscardMessage = 'Discard changes?';

  { General dialog options }
  RsYes = 'Yes';
  RsNo = 'No';


  {$include Expressions.pas}


  { TMainForm }




procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFilePath: String;
  TextWidth: Integer;
  AFileName: TFileName;
begin
  Randomize;

  // current directory shall always be where the exe is
  ChDir(ExtractFilePath(Application.ExeName));

  FSynHighlighter := TSynDemoHlFold.Create(Self);
  FExampleHighlighter := TSynDemoHlFold.Create(Self);
  Editor.Highlighter := FSynHighlighter;
  //ExampleEdit.Highlighter := FSynHighlighter;


  FDisplayMgr:= TDisplayManager.Create(Self);
  FDisplayMgr.OnPreviewChanged:= @HandlePreviewImageUpdate;

  FSysInfo := TSysInfo.Create(Self);
  FWinampControl := TWinampControl.Create(Self);

  IniFilePath := ExtractFilePath(application.ExeName) + STUDIO_INIFILE;
  LoadConfig(IniFilePath);

  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);


  CommandListBox.Items.Clear;
  CommandListBox.Items.Delimiter := ' ';
  CommandListBox.Items.DelimitedText := STUDIO_COMMANDS;
  CommandListBox.Items.Insert(0, 'COMMENT');
  CommandListBox.Selected[0] := True;

  KeywordListBox.Items.Clear;
  KeywordListBox.Items.Delimiter := ' ';
  KeywordListBox.Items.DelimitedText := STUDIO_KEYWORDS_STATIC + ' ' + STUDIO_KEYWORDS_VARIABLE;
  KeywordListBox.Sorted := True;
  //KeywordListBox.Selected[0]:= True;

  SynCompletion1.ItemList.Delimiter := ' ';
  SynCompletion1.ItemList.DelimitedText := STUDIO_COMMANDS + ' ' + STUDIO_KEYWORDS_STATIC + ' ' + STUDIO_KEYWORDS_VARIABLE;

  RestoreWindowState;

  if (PreviewGroupBox.Width >= (FStudioConfig.DisplayConfig.ResX * 2 + DOUBLESCALE_OFFSET)) then begin
    PreviewImage.Width := FStudioConfig.DisplayConfig.ResX * 2;
    PreviewImage.Height := FStudioConfig.DisplayConfig.ResY * 2;
  end else begin
    PreviewImage.Width := FStudioConfig.DisplayConfig.ResX;
    PreviewImage.Height := FStudioConfig.DisplayConfig.ResY;
  end;

  PreviewImage.Picture.Bitmap.Width := FStudioConfig.DisplayConfig.ResX;
  PreviewImage.Picture.Bitmap.Height := FStudioConfig.DisplayConfig.ResY;

  FDisplayMgr.PreviewColor:= FStudioConfig.ApplicationConfig.PreviewDisplayColor;
  FDisplayMgr.PreviewBackgroundColor:= FStudioConfig.ApplicationConfig.PreviewDisplayBackgroundColor;

  FDisplayMgr.AddDisplay('PREVIEW', FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY, '', 0);
  FDisplayMgr.ForcePreviewUpdate; // this instruction is needed to show the correct preview display background color on startup


  AFileName := FStudioConfig.ListConfig.ListName;

  // load passed file (otherwise just load last used file)
  if (Paramcount > 0) then begin
    // load passed list file
    if FileExists(ParamStr(1)) then
      AFileName := ParamStr(1);
  end;

  if (FileExists(AFileName)) then begin
    // load file
    LoadListFromFile(AFileName); // this will also change the caption of the form
  end;

  TextWidth := FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP);

  (*removeme
  // fill CPU usage with random data
  while (Length(FCpuUsageData.UsageHistory) < TextWidth) do
  begin
    SetLength(FCpuUsageData.UsageHistory, Length(FCpuUsageData.UsageHistory) + 1);
    FCpuUsageData.UsageHistory[High(FCpuUsageData.UsageHistory)] := Random(100);
  end;
  FCpuUsageData.AverageCpuUsage := Random(100);
  FCpuUsageData.CurrentCpuUsage := Random(100);

  // fill RAM usage with random data
  while (Length(FMemUsageData.UsageHistory) < TextWidth) do
  begin
    SetLength(FMemUsageData.UsageHistory, Length(FMemUsageData.UsageHistory) + 1);
    FMemUsageData.UsageHistory[High(FMemUsageData.UsageHistory)] := Random(100);
  end;
  FMemUsageData.AverageMemUsage := Random(100);
  FMemUsageData.CurrentMemUsage := Random(100);
  FMemUsageData.FreeMemory := Random(8 * 1014) * 1024 * 1024;
  FMemUsageData.PhysicalMemory := Random(16 * 1014) * 1024 * 1024;
  *)

end;



procedure TMainForm.SaveWindowState;
var
  IniFile: TIniFile;
  IsMaximized: Boolean;
begin
  IniFile := TIniFile.Create(EDITOR_INIFILE);
  try
    IsMaximized:= (wsMaximized = Application.MainForm.WindowState);
    IniFile.WriteBool('WindowState', 'IsMaximized', IsMaximized);
    IniFile.WriteInteger('WindowState', 'Left', Application.MainForm.Left);
    IniFile.WriteInteger('WindowState', 'Top', Application.MainForm.Top);
    if (not IsMaximized) then begin
      IniFile.WriteInteger('WindowState', 'Width', Application.MainForm.Width);
      IniFile.WriteInteger('WindowState', 'Height', Application.MainForm.Height);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.RestoreWindowState;
var
  IniFile: TIniFile;
  IsMaximixed: Boolean;
  AMonitor: TMonitor;
begin
  IniFile := TIniFile.Create(EDITOR_INIFILE);
  try

    // Read window state from config file
    Application.MainForm.Left := IniFile.ReadInteger('WindowState', 'Left', 0);
    Application.MainForm.Top := IniFile.ReadInteger('WindowState', 'Top', 0);
    Application.MainForm.Width := IniFile.ReadInteger('WindowState', 'Width', MainForm.Constraints.MinWidth);
    Application.MainForm.Height := IniFile.ReadInteger('WindowState', 'Height', MainForm.Constraints.MinHeight);
    IsMaximixed := IniFile.ReadBool('WindowState', 'IsMaximized', False);
    if (IsMaximixed) then
      MainForm.WindowState:= wsMaximized;

    // Find the monitor where the window should be shown
    AMonitor := Screen.MonitorFromPoint(Point(Application.MainForm.Left, Application.MainForm.Top));

    if not Assigned(Monitor) then
      AMonitor := Screen.PrimaryMonitor;

    // Ensure that the window is completely visible on the screen
    if (not IsMaximixed) then begin
      if Application.MainForm.Left + Application.MainForm.Width > AMonitor.BoundsRect.Right then
        Application.MainForm.Left := AMonitor.BoundsRect.Right - Application.MainForm.Width;
      if Application.MainForm.Top + Application.MainForm.Height > AMonitor.BoundsRect.Bottom then
        Application.MainForm.Top := AMonitor.BoundsRect.Bottom - Application.MainForm.Height;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.HandlePreviewImageUpdate(NewImage: TBitmap);
begin
  PreviewImage.Picture.Bitmap.Canvas.Draw(0, 0, NewImage);
end;

procedure TMainForm.CommandListBoxSelectionChange(Sender: TObject; User: Boolean);
var
  ItemString: String;
begin
  if ('' <> CommandListBox.GetSelectedText) then
  begin
    ItemString := CommandListBox.GetSelectedText;
    ShowCommandExplanation(ItemString);
  end;
end;

procedure TMainForm.KeywordListBoxSelectionChange(Sender: TObject; User: Boolean);
var
  ItemString: String;
begin
  if ('' <> KeywordListBox.GetSelectedText) then
  begin
    ItemString := KeywordListBox.GetSelectedText;
    ShowKeywordExplanation(ItemString);
  end;
end;


procedure TMainForm.ChangeMemoHeightToFitLines(Memo: TMemo);
var
  TestMemo: TMemo;
begin
  TestMemo := TMemo.Create(nil);
  try
    TestMemo.Parent := Memo.Parent;
    TestMemo.Width := Memo.Width;
    TestMemo.Font.Assign(Memo.Font);
    TestMemo.Text := Memo.Text;
    TestMemo.AdjustSize;
    Memo.Height := TestMemo.Height;
  finally
    TestMemo.Free;
  end;
end;

function TMainForm.AutoSizeMemoY(Memo: TMemo): Word;
begin
  if (assigned(Memo)) then begin
    Canvas.Font := Memo.Font;
    Result := Canvas.TextExtent(Memo.Lines.Strings[0]).cy * (Memo.Lines.Count + 1) +
      Canvas.TextExtent(Memo.Lines.Strings[0]).cy;
  end;
end;

procedure TMainForm.ShowCommandExplanation(Cmd: String);
var
  I: Integer;
begin
  HelpTextMemo.Lines.Clear;
  ExampleEdit.Lines.Clear;

  ExampleEdit.Highlighter := FExampleHighlighter;

  I := FindCommandIndex(Cmd);

  if (I <> -1) then
  begin
    HelpTextMemo.Text := Commands[I].Help;
    ExampleEdit.Text := Commands[I].Example;
  end;

  // adjust HelpTextMemo to new text size
  HelpTextMemo.Anchors := HelpTextMemo.Anchors - [akBottom];
  HelpTextMemo.Height := AutoSizeMemoY(HelpTextMemo);
  HelpTextMemo.Anchors := HelpTextMemo.Anchors + [akBottom];
end;


procedure TMainForm.ShowKeywordExplanation(Cmd: String);
var
  I: Integer;
begin
  HelpTextMemo.Lines.Clear;
  ExampleEdit.Lines.Clear;

  ExampleEdit.Highlighter := FExampleHighlighter;

  I := FindKeywordIndex(Cmd);

  if (I <> -1) then
  begin
    HelpTextMemo.Text := Keywords[I].Help;
    ExampleEdit.Text := Keywords[I].Example;
  end;

  // adjust HelpTextMemo to new text size
  HelpTextMemo.Anchors := HelpTextMemo.Anchors - [akBottom];
  HelpTextMemo.Height := AutoSizeMemoY(HelpTextMemo);
  HelpTextMemo.Anchors := HelpTextMemo.Anchors + [akBottom];

end;


procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveWindowState;
  if (Editor.Modified) then begin
    if QuestionDlg(RsDiscardTitle, RsDiscardMessage, mtConfirmation, [mrYes, RsYes, mrNo, RsNo, 'IsDefault', 'IsCancel'], 0) = mrYes then
      CanClose:= True
    else
      CanClose:= False;
  end else
    CanClose:= True;
end;

procedure TMainForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Line: string;
begin
  // print cursor location in status bar
  StatusBar1.Panels.Items[0].Text := '[' + IntToStr(Editor.CaretY) + ':' + IntToStr(Editor.CaretX) + ']' + '    Sel: ' + IntToStr(Length(Editor.SelText));
  // print total length in status bar
  StatusBar1.Panels.Items[1].Text := IntToStr(Length(Editor.Text)) + ' ' + RsCharacters;
  // print insert/overwrite mode
  if (Editor.InsertMode) then
    StatusBar1.Panels.Items[3].Text := 'INS'
  else
    StatusBar1.Panels.Items[3].Text := 'OVR';

  // check line and print result
  if (Editor.CaretY > 0) then
  begin
    Line:= Editor.Lines[Editor.CaretY - 1];
    if (FLastLine <> Line) then // only when the line has changed
    begin
      StatusBar1.Panels[4].Text := CheckLine(Line);
      FLastLine := Line;
      if ('ok' = StatusBar1.Panels[4].Text) then
        // is result was ok, then show a preview ...
        PreviewLine(Line)
      else
      begin
        // ... otherwise clear the preview display
        if (assigned(FDisplayMgr)) then begin
          FDisplayMgr.ClearScreen;
        end;
      end;
    end;

  end;

end;

procedure TMainForm.EditorChange(Sender: TObject);
var
  Line: String;
begin
  if (Editor.CaretY > 0) then
  begin
    Line:= Editor.Lines[Editor.CaretY - 1];
    if (FLastLine <> Line) then begin // only when the line has changed
      StatusBar1.Panels[4].Text := CheckLine(Line);
      if ('ok' = StatusBar1.Panels[4].Text) then
        if (assigned(FDisplayMgr)) then
          PreviewLine(Editor.Lines[Editor.CaretY - 1])
      else
      begin
        if (assigned(FDisplayMgr)) then begin
          FDisplayMgr.ClearScreen;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Line: string;
  Expr: string;
  I: Integer;
  P1, P2: Integer;
begin
  if (VK_F1 = Key) then begin
    Line:= Editor.Lines[Editor.CaretY - 1];

    // try to get the current expression
    if (not Line.IsEmpty) then begin
      Expr:= '';
      I:= Editor.CaretX;
      P1:= I;
      P2:= I;
      while (I >= 1) do begin
        if (Line[I] <> ' ') and (Line[I] <> '$') then begin
          P1:= I;
          Dec(I);
        end
        else begin
          Break;
        end;
      end;
      I:= Editor.CaretX;
      while I <= Length(Line) do begin
        if (Line[I] <> ' ') and (Line[I] <> '$') then begin
          Inc(I);
        end
        else begin
          Break;
        end;
      end;
      P2:= I;

      if (P1 >= 1) and (P2 > P1) then begin
        Expr:= Trim(Line.Substring(P1-1, P2-P1).Replace(';', ''));
        if (-1 <> FindCommandIndex(Expr)) then
          ShowCommandExplanation(Expr)
        else if (-1 <> FindKeywordIndex(Expr)) then
          ShowKeywordExplanation(Expr)
        else
          MessageBeep(MB_ICONERROR);
      end;
    end;
  end;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.Close;
end;


{
 Add a ';' at the begin of each selected line
}
procedure TMainForm.CommentLinesMenuItemClick(Sender: TObject);
var
  I: Integer;
  PreviousCaretXY: TPoint;
  PrevBlockBegin, PrevBlockEnd: TPoint;
begin
  PreviousCaretXY := Editor.LogicalCaretXY;
  PrevBlockBegin := Editor.BlockBegin;
  PrevBlockEnd := Editor.BlockEnd;

  Editor.BeginUpdate(True);
  Editor.BeginUndoBlock;

  // check if at least one line is selected
  if Editor.SelAvail then
  begin
    for I := Editor.BlockBegin.Y to Editor.BlockEnd.Y do
    begin
      Editor.CaretY:= I;
      CommentCurrentLine;
    end;
  end else begin
    // handle only current line
    CommentCurrentLine;
  end;

  Editor.EndUndoBlock;
  Editor.EndUpdate;

  Editor.BlockBegin := PrevBlockBegin;
  Editor.BlockEnd := PrevBlockEnd;
  Editor.LogicalCaretXY := PreviousCaretXY;
end;

{
 Remove a ';' at the begin of each selected line
}
procedure TMainForm.UnCommentMenuItemClick(Sender: TObject);
var
  I: Integer;
  PreviousCaretXY: TPoint;
  PrevBlockBegin, PrevBlockEnd: TPoint;
begin
  PreviousCaretXY := Editor.LogicalCaretXY;
  PrevBlockBegin := Editor.BlockBegin;
  PrevBlockEnd := Editor.BlockEnd;

  Editor.BeginUpdate(True);
  Editor.BeginUndoBlock;

  // check if at least one line is selected
  if Editor.SelAvail then
  begin
    for I := Editor.BlockBegin.Y to Editor.BlockEnd.Y do
    begin
      if (Editor.Lines[I-1].StartsWith(';')) then begin
        Editor.CaretY:= I;
        UncommentCurrentLine;
      end;
    end;
  end else begin
    UncommentCurrentLine;
  end;

  Editor.EndUndoBlock;
  Editor.EndUpdate;

  Editor.BlockBegin := PrevBlockBegin;
  Editor.BlockEnd := PrevBlockEnd;
  Editor.LogicalCaretXY := PreviousCaretXY;
end;

procedure TMainForm.CommentCurrentLine;
begin
  Editor.CaretX := 1;
  Editor.InsertTextAtCaret(';');
end;

procedure TMainForm.UncommentCurrentLine;
var
  P1, P2: TPoint;
begin
  if (Editor.Lines[Editor.CaretXY.Y - 1].StartsWith(';')) then begin
    P1 := Editor.LogicalCaretXY;
    P1.X:= 1;
    P2 := P1;
    // Calculate the byte pos of the next char
    P2.X := P2.X + 1;
    // P1 points to the first byte of char to be replaced
    // P2 points to the first byte of the char after the last replaceable char
    Editor.TextBetweenPoints[P1, P2] := '';
  end;
end;


procedure TMainForm.NewScreenMenuItemClick(Sender: TObject);
begin
  Editor.CaretX:= 1;
  Editor.InsertTextAtCaret(';add a description here' + LineEnding +
                           'NEWSCREEN' + LineEnding +
                           'CLEARSCREEN' + LineEnding +
                            LineEnding +
                            'SCREENTIME 60' + LineEnding +
                           'ENDSCREEN' + LineEnding);

  // place cursor at end of the SCREENTIME instruction
  Editor.CaretY:= Editor.CaretY - 2;
  Editor.CaretX:= 14;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSysInfo.Free;
  FWinampControl.Free;
  FDisplayMgr.Free;
  FSynHighlighter.Free;
end;

procedure TMainForm.EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if (Line > 0) and (CheckLine(Editor.Lines[Line - 1]) <> 'ok') then
  begin
    Special := True;
    BG := $C1C1FF;
  end
  else
  if (Line = Editor.BlockBegin.Y) then
  begin
    Special := True;
    BG := Editor.LineHighlightColor.Background;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if (PreviewGroupBox.Width >= (FStudioConfig.DisplayConfig.ResX * 2 + DOUBLESCALE_OFFSET)) then begin
    PreviewImage.Width := FStudioConfig.DisplayConfig.ResX * 2;
    PreviewImage.Height := FStudioConfig.DisplayConfig.ResY * 2;
  end else begin
    PreviewImage.Width := FStudioConfig.DisplayConfig.ResX;
    PreviewImage.Height := FStudioConfig.DisplayConfig.ResY;
  end;
end;

procedure TMainForm.InsertAnimationMenuItemClick(Sender: TObject);
var
  AFilename: String;
  RegEx: TRegExpr;
  TmpBitmap: TBitmap;
  NumFrames: Integer;
  FrameWidth: Integer;
  Match1: String;
begin
  if OpenPictureDialog1.Execute then begin
    AFilename:= OpenPictureDialog1.FileName;
    if (AFilename.StartsWith(ExtractFilePath(Application.ExeName))) then
      AFilename:= AFilename.Replace(ExtractFilePath(Application.ExeName), '');

    NumFrames:= 2;

    // if the filename ends with something like "_30.bmp" then we extract the number of frames from that
    RegEx:= TRegExpr.Create();
    RegEx.Expression := '_(\d+)\.bmp$';
    if (RegEx.Exec(AFilename)) then
    begin
      Match1 := RegEx.Match[1];
      NumFrames:= Max(2, StrToIntDef(Match1, NumFrames)); // must be at least two frames
    end;
    RegEx.Free;

    TmpBitmap:= TBitmap.Create();
    TmpBitmap.LoadFromFile(AFilename);
    FrameWidth:= TmpBitmap.Width div NumFrames;
    TmpBitmap.Free;

    Editor.CaretX:= 1;
    Editor.InsertTextAtCaret('ANIMATE '''+AFilename+''' 1000 00 00 ' + IntToStr(FrameWidth) + LineEnding);

    Editor.CaretY:= Editor.CaretY - 1;
    Editor.CaretX:= 12;
  end;
end;

procedure TMainForm.InsertBitmapMenuItemClick(Sender: TObject);
var
  AFilename: String;
begin
  if OpenPictureDialog1.Execute then begin
    AFilename:= OpenPictureDialog1.FileName;
    if (AFilename.StartsWith(ExtractFilePath(Application.ExeName))) then
      AFilename:= AFilename.Replace(ExtractFilePath(Application.ExeName), '');
    Editor.CaretX:= 1;
    Editor.InsertTextAtCaret('BITMAP '''+AFilename+''' 00 00' + LineEnding);

    Editor.CaretY:= Editor.CaretY - 1;
    Editor.CaretX:= 12;
  end;
end;

procedure TMainForm.InsertPlaintextMenuItemClick(Sender: TObject);
begin
  Editor.CaretX:= 1;
  Editor.InsertTextAtCaret('PLAINTEXT '''' 00 00' + LineEnding);

  // place cursor at the text position
  Editor.CaretY:= Editor.CaretY - 1;
  Editor.CaretX:= 12;
end;

procedure TMainForm.InsertTextMenuItemClick(Sender: TObject);
begin
  Editor.CaretX:= 1;
  Editor.InsertTextAtCaret('TEXTOUT '''' 00 00 12 ''Arial''' + LineEnding);

  // place cursor at the text position
  Editor.CaretY:= Editor.CaretY - 1;
  Editor.CaretX:= 10;
end;


procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  OpenButtonClick(Sender);
end;

procedure TMainForm.NewFileMenuItemClick(Sender: TObject);
begin
  NewFileButtonClick(Sender);
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

procedure TMainForm.NewFileButtonClick(Sender: TObject);
begin
  if (Editor.Modified) then begin
    if QuestionDlg(RsDiscardTitle, RsDiscardMessage, mtConfirmation, [mrYes, RsYes, mrNo, RsNo, 'IsDefault', 'IsCancel'], 0) = mrYes then
    begin
      Editor.Lines.Clear;
      Editor.Modified:= False;
      FFileName:= '';
    end;
  end else begin
    Editor.Lines.Clear;
    Editor.Modified:= False;
    FFileName:= '';
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if ('' = FFileName) then
    if (SaveDialog.Execute) then
      FFileName:= SaveDialog.FileName;

  if ('' <> FFileName) then
  begin
    Editor.Lines.SaveToFile(FFileName);
    Caption := APP_TITLE + ' - ' + ExtractFileName(SaveDialog.FileName);
    Editor.Modified:= False;
  end;
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
begin
  if (SaveDialog.Execute) then begin
    Editor.Lines.SaveToFile(SaveDialog.FileName);
    Caption := APP_TITLE + ' - ' + ExtractFileName(SaveDialog.FileName);
    FFileName:= SaveDialog.FileName;
    Editor.Modified:= False;
  end;
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
    FFileName:= FileName;
    Editor.Modified:= False;
  finally
    SL.Free;
  end;
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  if (Editor.Modified) then
    if QuestionDlg(RsDiscardTitle, RsDiscardMessage, mtConfirmation, [mrYes, RsYes, mrNo, RsNo, 'IsDefault', 'IsCancel'], 0) = mrNo then
      Exit;

  if (OpenDialog.Execute) then
  begin
    LoadListFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
  SaveButtonClick(Sender);
end;

procedure TMainForm.SynCompletion1BeforeExecute(ASender: TSynBaseCompletion;
  var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer;
  var AnResult: TOnBeforeExeucteFlags);
begin

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
  Editor.Font.Size := Editor.Font.Size + 1;
end;

procedure TMainForm.ZoomInMenuItemClick(Sender: TObject);
begin
  ZoomInButtonClick(Sender);
end;

procedure TMainForm.ZoomOutButtonClick(Sender: TObject);
begin
  if (Editor.Font.Size > 8) then
    Editor.Font.Size := Editor.Font.Size - 1;
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
    ColorString := IniFile.ReadString('APPLICATION', 'DspBackColor', '$000000');
    ColorString := ColorString.Replace('#', '$');
    FStudioConfig.ApplicationConfig.PreviewDisplayBackgroundColor := StringToColor(ColorString);
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
  Res := 'not ok';
  S := Trim(Line);
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.QuoteChar := '''';
  SL.DelimitedText := S;

  RegEx := TRegExpr.Create;

  if (S.IsEmpty) or (S.StartsWith(';')) then
  begin
    Res := 'ok';
  end
  else
  begin
    // check if it is a known command
    Cmd := UpperCase(SL[0]);
    Valid := False;
    for I := 0 to CommandListBox.Items.Count - 1 do
    begin
      if (Cmd = CommandListBox.Items[I]) then
      begin
        Valid := True;
        (* TODO
        CommandListBox.ItemIndex:= I;
        CommandListBox.Selected[I]:= True;
        *)
        Break;
      end;
    end;
    if (not Valid) then
    begin
      Res := 'unknown command "' + Cmd + '"';
    end
    else
    begin
      // plausibility check of commands

      if ('NEWSCREEN' = CMD) then
      begin
        // should have max. one parameter ONCE, CONTINUE REQUIRE*
        RegEx.Expression := '^NEWSCREEN(\s+.+)?$';
        if (RegEx.Exec(S)) then
        begin
          if (RegEx.SubExprMatchCount = 1) then
          begin
            Match := Trim(RegEx.Match[1]);
            if (Match = '') or (Match = 'ONCE') or (Match = 'CONTINUE') or (Match = 'REQUIREWINAMP') then
            begin
              Res := 'ok';
            end
            else
            begin
              Res := 'possible NEWSCREEN parameters are ONCE, CONTINUE, REQUIREWINAMP';
            end;
          end
          else
          begin
            Res := 'NEWSCREEN should have max. one parameter';
          end;
        end
        else
        begin
          Res := 'invalid NEWSCREEN syntax';
        end;
      end

      else
      if ('ENDSCREEN' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^ENDSCREEN$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'ENDSCREEN should have no parameters';
        end;
      end

      else
      if ('INCLUDE' = CMD) then
      begin
        // should have one filename parameter
        RegEx.Expression := '^INCLUDE\s+(.+)$';
        if (RegEx.Exec(S)) then
        begin
          Match := RegEx.Match[1];
          if FileExists(Match) then
            Res := 'ok'
          else
            Res := 'File "' + Match + '" not found.';
        end
        else
        begin
          Res := 'INCLUDE should have one file name parameter';
        end;
      end

      else
      if ('CLEARSCREEN' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^CLEARSCREEN$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'CLEARSCREEN should have no parameters';
        end;
      end

      else
      if ('STOP' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^STOP$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'STOP should have no parameters';
        end;
      end

      else
      if ('ORMODE' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^ORMODE$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'ORMODE should have no parameters';
        end;
      end

      else
      if ('XORMODE' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^XORMODE';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'XORMODE should have no parameters';
        end;
      end

      else
      if ('ANDMODE' = CMD) then
      begin
        // should have no parameters
        RegEx.Expression := '^ANDMODE';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'ANDMODE should have no parameters';
        end;
      end

      else
      if ('LIGHT' = CMD) then
      begin
        // should have one number parameter
        RegEx.Expression := '^LIGHT(\s+\d+)$';
        if (RegEx.Exec(S)) then
        begin
          Match := RegEx.Match[1];
          Value := StrToIntDef(Match, 100);
          if (Value >= 0) and (Value <= 100) then
            Res := 'ok'
          else
            Res := 'LIGHT parameter must be 0..100';
        end
        else
        begin
          Res := 'LIGHT should have one number parameter 0..100';
        end;
      end

      else
      if ('SCREENTIME' = CMD) then
      begin
        // should have one number parameter
        RegEx.Expression := '^SCREENTIME(\s+\d+)$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'SCREENTIME should have one number parameter';
        end;
      end

      else
      if ('MATRIX' = CMD) then
      begin
        // should have one number parameter
        RegEx.Expression := '^MATRIX(\s+\d+)$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'MATRIX should have one number parameter';
        end;
      end

      else
      if ('NOISE' = CMD) then
      begin
        // should have one number parameter
        RegEx.Expression := '^NOISE(\s+\d+)$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'NOISE should have one number parameter';
        end;
      end

      else
      if ('CPUMONITOR' = CMD) then
      begin
        // should have two number parameters
        RegEx.Expression := '^CPUMONITOR(\s+\d+){2}$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'CPUMONITOR should have two number parameters';
        end;
      end

      else
      if ('RAMMONITOR' = CMD) then
      begin
        // should have two number parameters
        RegEx.Expression := '^RAMMONITOR(\s+\d+){2}$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'RAMMONITOR should have two number parameters';
        end;
      end


      else
      if ('PIXEL' = CMD) then
      begin
        // should have two number parameters and an optional inverted marker
        RegEx.Expression := '^PIXEL(\s+[+-]?\d+){2}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'PIXEL should have two number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else
      if ('LINE' = CMD) then
      begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression := '^LINE(\s+[+-]?\d+){4}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'LINE should have four number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else
      if ('FRAME' = CMD) then
      begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression := '^FRAME(\s+[+-]?\d+){4}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(UpperCase(S))) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'FRAME should have four number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else
      if ('CLOCK' = CMD) then
      begin
        // should have four number parameters and an optional inverted marker
        RegEx.Expression := '^CLOCK(\s+[+-]?\d+){6}?$';
        if (RegEx.Exec(UpperCase(S))) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'CLOCK should have six number parameters';
        end;
      end

      else
      if ('PLAINTEXT' = CMD) then
      begin
        // should have a text parameter followed by two number parameters
        RegEx.Expression := '^PLAINTEXT\s+\''(\''{2}|[^\''])+?\''(\s+\d+){2}$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'PLAINTEXT should have a non-empty text parameter followed by two number parameters';
        end;
      end

      else
      if ('TEXTOUT' = CMD) then
      begin
        // should have a text parameter followed by three number parameters and another text parameter for the font name
        RegEx.Expression := '^TEXTOUT\s+\''(\''{2}|[^\''])+?\''(\s+\d+){3}\s+\''(.*)\''$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'TEXTOUT should have a text parameter followed by three number parameters and another text parameter';
        end;
      end

      else
      if ('ANIMATE' = CMD) then
      begin
        // should have a text parameter followed by four number parameters
        RegEx.Expression := '^ANIMATE\s+\''(.+?)\''(\s+\d+)(\s+[+-]?\d+){2}(\s+\d+)(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(S)) then
        begin
          Match := RegEx.Match[1];
          if (FileExists(Match)) then
            Res := 'ok'
          else
            Res:= 'Bitmap file not found';
        end
        else
        begin
          Res := 'ANIMATE should have a text parameter followed by four number parameters, followed by an optional 0 or FALSE';
        end;
      end

      else
      if ('BITMAP' = CMD) then
      begin
        // should have a text parameter followed by two number parameters
        RegEx.Expression := '^BITMAP\s+\''(.+?)\''(\s+[+-]?\d+){2}$';
        if (RegEx.Exec(S)) then
        begin
          Match := RegEx.Match[1];
          if (FileExists(Match)) then
            Res := 'ok'
          else
            Res:= 'Bitmap file not found';
        end
        else
        begin
          Res := 'BITMAP should have a text parameter followed by two number parameters';
        end;
      end

      else
      if ('WAPLAYBAR' = CMD) then
      begin
        // should have three number parameters
        RegEx.Expression := '^WAPLAYBAR(\s+[+-]?\d+){3}(\s+[01]|\s+TRUE|\s+FALSE)?$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'WAPLAYBAR should have three number parameters, followed by an optional 0/1 or FALSE/TRUE';
        end;
      end

      else
      if ('DRIVEUSAGE' = CMD) then
      begin
        // should have a char parameter followed by three number parameters
        RegEx.Expression := '^DRIVEUSAGE\s+\''(\w)\''(\s+\d+){3}$';
        if (RegEx.Exec(S)) then
        begin
          Res := 'ok';
        end
        else
        begin
          Res := 'DRIVEUSAGE should have a single character text parameter followed by three number parameters';
        end;
      end;

    end; // is known command
  end; // is not empty or comment


  RegEx.Free;
  SL.Free;
  Result := Res;
end;

procedure TMainForm.PreviewLine(S: String);
var
  CmdParts: TStringList;
  Cmd: String;
  ITmp: Integer;
  X, Y: Integer;
  P1, P2, P3, P4, P5: Integer;
  TmpBitmap: TBitmap;
  IsInverted: Boolean;
begin
  if (not assigned(FDisplayMgr)) then
    Exit;

  FDisplayMgr.ClearScreen;
  FDisplayMgr.ClearInfoStrings;

  CmdParts := TStringList.Create;
  try
    CmdParts.Delimiter := ' ';
    CmdParts.QuoteChar := '''';
    CmdParts.DelimitedText := S;

    if (CmdParts.Count > 0) then
    begin
      Cmd := CmdParts[0];

      if ('NEWSCREEN' = Cmd) then
      begin
      end
      else
      if ('ENDSCREEN' = Cmd) then
      begin
      end
      else
      if ('DSPINIT' = Cmd) then
      begin
      end
      else
      if ('ORMODE' = Cmd) then
      begin
        // wouldn't make sense since we show only one command at a time on the preview display
      end
      else
      if ('XORMODE' = Cmd) then
      begin
        // wouldn't make sense since we show only one command at a time on the preview display
      end
      else
      if ('ANDMODE' = Cmd) then
      begin
        // wouldn't make sense since we show only one command at a time on the preview display
      end
      else
      if ('STOP' = Cmd) then
      begin
      end
      else
      if ('CLEARSCREEN' = Cmd) then
      begin
        FDisplayMgr.ClearScreen;
      end
      else
      if ('SCREENTIME' = Cmd) then
      begin
      end
      else
      if ('LIGHT' = Cmd) then
      begin
      end
      else
      if ('NOISE' = Cmd) then
      begin
        // p1 = amount of pixels, p2 = inverted or not [optional]
        if (CmdParts.Count >= 2) then
        begin
          P1 := StrToIntDef(CmdParts[1], 100);
          if ((CmdParts.Count >= 3) and ((CmdParts[2].ToUpper = 'TRUE') or (CmdParts[2] = '1'))) then
          begin
            IsInverted:= True;
          end
          else
          begin
            IsInverted:= False;
          end;
          Randomize;
          for ITmp := 1 to P1 do
          begin
            X := Random(FStudioConfig.DisplayConfig.ResX);
            Y := Random(FStudioConfig.DisplayConfig.ResY);
            FDisplayMgr.PaintPixel(X, Y, IsInverted);
          end;
        end;
      end
      else
      if ('PIXEL' = Cmd) then
      begin
        // p1 = x, p2 = y, p3 = inverted or not [optional]
        if (CmdParts.Count >= 3) then
        begin
          P1 := StrToIntDef(CmdParts[1], 0);
          P2 := StrToIntDef(CmdParts[2], 0);
          if ((CmdParts.Count >= 4) and ((CmdParts[3].ToUpper = 'TRUE') or (CmdParts[3] = '1'))) then
          begin
            IsInverted:= True;
          end
          else
          begin
            IsInverted:= False;
          end;
          FDisplayMgr.PaintPixel(P1, P2, IsInverted);
        end;
      end
      else
      if ('LINE' = Cmd) then
      begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then
        begin
          P1 := StrToIntDef(CmdParts[1], 0);
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 32);
          P4 := StrToIntDef(CmdParts[4], 32);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then
          begin
            IsInverted:= True;
          end
          else
          begin
            IsInverted:= False;
          end;
          FDisplayMgr.PaintLine(P1, P2, P3, P4, IsInverted);
        end;
      end
      else
      if ('FRAME' = Cmd) then
      begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then
        begin
          P1 := StrToIntDef(CmdParts[1], 0);
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 32);
          P4 := StrToIntDef(CmdParts[4], 32);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then
          begin
            IsInverted:= True;
          end
          else
          begin
            IsInverted:= False;
          end;
          FDisplayMgr.PaintFrame(P1, P2, P3, P4, IsInverted);
        end;
      end
      else
      if ('PLAINTEXT' = Cmd) then
      begin
        // p1 = text, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then
        begin
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 0);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$')) then
          begin
            FDisplayMgr.HandleTextOutput(CmdParts[1], P2, P3, '', 0);
            FDisplayMgr.ForcePreviewUpdate;
          end
          else
          begin
            FDisplayMgr.PaintString(CmdParts[1], P2, P3);
            FDisplayMgr.ForcePreviewUpdate;
          end;
        end;
      end
      else
      if ('TEXTOUT' = Cmd) then
      begin
        // p1 = text, p2 = x, p3 = y, p4 = font size, p5 = font name
        if (CmdParts.Count >= 6) then
        begin
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 0);
          P4 := StrToIntDef(CmdParts[4], 12);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$')) then
          begin
            FDisplayMgr.HandleTextOutput(CmdParts[1], P2, P3, CmdParts[5], P4);
            FDisplayMgr.ForcePreviewUpdate;
          end
          else
          begin
            FDisplayMgr.DrawFontedText(CmdParts[1], P2, P3, CmdParts[5], P4, 1, 1);
            FDisplayMgr.ForcePreviewUpdate;
          end;
        end;
      end
      else
      if ('ANIMATE' = Cmd) then
      begin
        // p1 = file name, p2 = animation speed, p3 = x, p4 = y, p5 = frame width
        if (CmdParts.Count >= 6) then
        begin
          P2 := StrToIntDef(CmdParts[2], 500);
          P3 := StrToIntDef(CmdParts[3], 0);
          P4 := StrToIntDef(CmdParts[4], 0);
          P5 := StrToIntDef(CmdParts[5], 1);
          TmpBitmap := TBitmap.Create;
          try
            TmpBitmap.LoadFromFile(CmdParts[1]);
            TmpBitmap.Width := P5; // clip to first frame
            // clip bitmap to display if needed
            if (TmpBitmap.Height + P4 >= FStudioConfig.DisplayConfig.ResY) then
              TmpBitmap.Height := TmpBitmap.Height - (TmpBitmap.Height + P4 - FStudioConfig.DisplayConfig.ResY);
            if (TmpBitmap.Width + P3 >= FStudioConfig.DisplayConfig.ResX) then
              TmpBitmap.Width := TmpBitmap.Width - (TmpBitmap.Width + P3 - FStudioConfig.DisplayConfig.ResX);

            FDisplayMgr.PaintBitmap(TmpBitmap, P3, P4);

          finally
            TmpBitmap.Free;
          end;

        end;

      end
      else
      if ('BITMAP' = Cmd) then
      begin
        // p1 = file path, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then
        begin
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 0);
          FDisplayMgr.PaintBitmapFromFile(CmdParts[1], P2, P3);
        end;
      end
      else
      if ('DRIVEUSAGE' = Cmd) then
      begin
        // p1 = drive letter, P2 = x, P3 = y, P4 = width (in characters)
        if (CmdParts.Count >= 5) then
        begin
          P2 := StrToIntDef(CmdParts[2], 0);
          P3 := StrToIntDef(CmdParts[3], 0);
          P4 := StrToIntDef(CmdParts[4], 10);
          DrawDriveUsage(CmdParts[1][1], P2, P3, P4);
        end;
      end
      else
      if ('CLOCK' = Cmd) then
      begin
        (* TODO
        // p1 = offset in minutes, p2 = x, p3 = y, p4 = hour hand length, p5 = minute hand length, p6 = seconds hand length
        if (CmdParts.Count >= 7) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          P5:= StrToInt(CmdParts[5]);
          P6:= StrToInt(CmdParts[6]);
          AddClock(P1, P2, P3, P4, P5, P6);
        end;
        *)
      end
      else
      if ('MATRIX' = Cmd) then
      begin
        (*  TODO
        // p1 = speed
        if (CmdParts.Count >= 2) then begin
          P1:= StrToInt(CmdParts[1]);
          // Setting up the drops
          InitMATRIX;
          ExtraTimer.Interval:= P1;
          ExtraTimer.Enabled:= True;
        end;
        *)
      end
      else
      if ('CPUMONITOR' = Cmd) then
      begin
        // p1 = number of rows to use, p2 = bottom-most row
        if (CmdParts.Count >= 3) then
        begin
          P1 := StrToIntDef(CmdParts[1], 1);
          P2 := StrToIntDef(CmdParts[2], 7);
          FDisplayMgr.ConfigureCpuMonitor(P1, P2);
          IsCpuMonitorDisplayed := True;
          FDisplayMgr.UpdateUsageMonitors(IsCpuMonitorDisplayed, IsMemMonitorDisplayed);
        end;
      end
      else
      if ('RAMMONITOR' = Cmd) then
      begin
        // p1 = number of rows to use, p2 = bottom-most row
        if (CmdParts.Count >= 3) then
        begin
          P1 := StrToIntDef(CmdParts[1], 1);
          P2 := StrToIntDef(CmdParts[2], 7);
          FDisplayMgr.ConfigureMemMonitor(P1, P2);
          IsMemMonitorDisplayed := True;
          FDisplayMgr.UpdateUsageMonitors(IsCpuMonitorDisplayed, IsMemMonitorDisplayed);
        end;
      end;

    end; // endif (CmdParts.Count > 0)

  finally
    CmdParts.Free;
  end;

end;



procedure TMainForm.DrawDriveUsage(DriveLetter: Char; Col, Row, BarWidth: Integer);
var
  TotalMem: QWord;
  FreeMem: QWord;
  PercentFree: Double;
  X: Integer;
  C: Char;
  UsedNum: Integer;
begin

  // get drive information
  TotalMem := FSysInfo.GetDiskSpace(DriveLetter);
  FreeMem := FSysInfo.GetFreeDiskSpace(DriveLetter);

  // calculate percentage of free bytes
  PercentFree := FreeMem / TotalMem;

  // calculate number of characters in bar which are used
  UsedNum := Round(BarWidth * (1.0 - PercentFree));

  for X := 0 to (BarWidth - 1) do
  begin
    if (X < UsedNum) then
      C := Chr($87)
    else
      C := Chr($8E);
    FDisplayMgr.PaintString(C, X + Col, Row);
  end;
end;


function TMainForm.FindCommandIndex(const Expr: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Commands) to High(Commands) do
  begin
    if Expr.StartsWith(Commands[I].Expr) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TMainForm.FindKeywordIndex(const Expr: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Keywords) to High(Keywords) do
  begin
    if Expr.StartsWith(Keywords[I].Expr) then
    begin
      Result := I;
      Break;
    end;
  end;
end;



end.
