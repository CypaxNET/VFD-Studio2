unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  VFDisplay, IniFiles, Menus, ExtCtrls, InfoUnit, uSMBIOS,
  SysInfo, WinampControl, LCLTranslator, ComCtrls, DateUtils, Math,
  StudioCommon, Glyphs, lclintf, RegExpr, Process, DisplayManager, SettingsForm;
type

  TSearchDirection = (sdForward, sdBackward);

  TApplicationConfig = record
    Language: String;
    PreviewDisplayColor: TColor; // color of the pixels in the preview display
    DoStartMinimized: Boolean;   // start VFD-Studio2 minimized in system tray?
    IsBrightnessControlledByList: Boolean; // display brightness is controlled by list commands (otherwise by following setting)
    DisplayBrightness: Byte; // display brightness in percent, unless brightness is controlled by list commands
    DoClearOnExit: Boolean; // clear the display when closing the application (otherwise it is left as it is)
    IconIndex: Integer; // which icon shall be shown in system tray?
  end;

  TListConfig = record
    ListName: String;
  end;

  TAnimationConfig = record
    PlayOnlyOnIdle: Boolean; // play animations only then the CPU is idle?
    IdlePercent: Byte;       // which percentage of CPU usage still counts as idle?
  end;

  TStudioConfig = record
    ApplicationConfig: TApplicationConfig;
    DisplayConfig: TDisplayConfig;
    ListConfig: TListConfig;
    AnimationConfig: TAnimationConfig;
  end;

  TVariableInfo = record
    Text: String;
    SubsText: String;    // substituted content of the string; used to determine if it needs to be re-drawn
    X, Y: Byte;
    FontName: String;
    FontSize: Integer;
    PrevWidth: Integer;  // width of the string before we updated it
    PrevHeight: Integer; // height of the string before we updated it; this is only relevant when handling text with a font
  end;

  TClockData = record
    IsActive: Boolean;
    Offset: Integer;
    X, Y: Word;
    HourHandLength, MinuteHandLength, SecondsHandLength: Word;
    HourPoint, MinutePoint, SecondsPoint: TPoint;
  end;

  // structure of one column of text dropping down the display
  TMatrixDrop = record
    Text: String;          // goes bottom up: first char will be in lowest column
    MaxTextLen: Byte;      // maximum length of this drop
    Row: Integer;          // current row (where the first character is located); can be out of display area
    SlownessFactor: Byte;  // controls the speed of the drop; the higher, the slower
  end;

  TMatrixData = record
    CycleCounter: Int64;
    Drops: array of TMatrixDrop;
  end;

  TWinampPlayBarData = record
    Col, Row, Width: Integer;
    IsActive: Boolean;
    IsGraphical: Boolean;
  end;

  TAnimationData = record
    IsAnimationDisplayed: Boolean; // is an animation currently displayed?
    IsAnimationPaused: Boolean;    // is the animation paused?
    AnimationBitmap: TBitmap;      // Bitmap object holding all the frames
    FrameWidth: Byte;              // frame width in pixels
    FrameIndex: Word;              // index of current frame
    FrameCount: Word;              // total number of frames
    XPos: Word;                    // x position in display
    YPos: Word;                    // y position in display
  end;



  { TMainForm }
  TMainForm = class(TForm)
    CfgButton: TBitBtn;
    ListEditorButton: TBitBtn;
    ClearLogButton: TBitBtn;
    ExpertViewButton: TBitBtn;
    ExtListBox: TListBox;
    LogfileGroupBox: TGroupBox;
    ListBox: TListBox;
    ListGroupBox: TGroupBox;
    ImageList1: TImageList;
    HyperlinkLabel: TLabel;
    DevelopmentYearsLabel: TLabel;
    byLabel: TLabel;
    LogListView: TListView;
    MenuItemReload: TMenuItem;
    SpacerPanel: TPanel;
    SaveDialog: TSaveDialog;
    SaveLogButton: TBitBtn;
    VersionLabel: TLabel;
    BottomButtonsPanel: TPanel;
    ExitButton: TBitBtn;
    InfoButton: TBitBtn;
    ListTestButton: TBitBtn;
    ColorDialog: TColorDialog;
    PreviewImage: TImage;
    NextButton: TBitBtn;
    OKButton: TBitBtn;
    LoadListPanel: TPanel;
    MenuItemExit: TMenuItem;
    ExtraTimer: TTimer;
    LoadListTimer: TTimer;
    InfoTimer: TTimer;
    MenuItemLoadList: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    MenuItemNext: TMenuItem;
    OpenDialog: TOpenDialog;
    OkButtonPanel: TPanel;
    PopupMenu1: TPopupMenu;
    MenuItemStopGo: TMenuItem;
    ReloadButton: TBitBtn;
    ScreenTimeLabel: TLabel;
    MenuItemMainWindow: TMenuItem;
    AnimateTimer: TTimer;
    StopButton: TBitBtn;
    TrayIcon1: TTrayIcon;
    UsageTimer: TTimer;
    WaitTimer: TTimer;
    procedure AnimateTimerTimer(Sender: TObject);
    procedure CfgButtonClick(Sender: TObject);
    procedure ListEditorButtonClick(Sender: TObject);
    procedure ExpertViewButtonClick(Sender: TObject);
    procedure ClearLogButtonClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure ExtraTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure LoadListTimerTimer(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure InfoTimerTimer(Sender: TObject);
    procedure HyperlinkLabelClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure MenuItemLoadListClick(Sender: TObject);
    procedure OpenListButtonClick(Sender: TObject);
    procedure MenuItemReloadClick(Sender: TObject);
    procedure MenuItemNextClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure MenuItemStopGoClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure SaveLogButtonClick(Sender: TObject);
    procedure MenuItemMainWindowClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure UsageTimerTimer(Sender: TObject);
    procedure WaitTimerStartTimer(Sender: TObject);
    procedure WaitTimerStopTimer(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);

    function FindWindowByTitle(WindowTitle: String): Hwnd;

    // Icon handling
    procedure SetApplicationIcon;

    // methods related to the preview image
    procedure HandlePreviewImageUpdate(NewImage: TBitmap);

    // methods related to processing lists
    function LoadListFromFile(const ListFileName: TFileName): Boolean;
    procedure LoadList(const ListFileName: TFileName);

    procedure CreateScreen;
    function InterpreteListCommand(S: String): Integer;
    procedure StopProcessing;
    function SeekTo(StartIndex: Integer; SearchDirection: TSearchDirection; Cmd: String): Integer;
    procedure UpdateTimeLabel;

    // Bitmaps and animations
    procedure Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, FrameWidth: Word);
    procedure DrawAnimationFrame(ABitmap: TBitmap; X, Y, Frame, FrameWidth: Word);
    procedure PauseAnimation;
    procedure StopAnimation;

    // special display feature stuff
    procedure InitMatrix;
    procedure AddMatrixDrop(const AText: String; MaxTextLen, Row: Integer; SlownessFactor: Byte);
    procedure UpdateMatrixDrops;
    procedure AddClock(Offset: Integer; X, Y, HourHandLength, MinuteHandLength, SecondsHandLength: Word);
    procedure DisableClocks;
    procedure RefreshClocks;
    procedure DrawDriveUsage(DriveLetter: Char; Col, Row, BarWidth: Integer);
    procedure DrawWinampPlayBar;
    procedure DrawProgressBar(Col, Row, BarWidth: Integer; Percentage: Double);

    // settings
    procedure LoadConfig(const AFilePath: String);
    procedure SaveConfig(const AFilePath: String);
    procedure SettingsOkPressed;
    procedure SettingsAbortPressed;
    procedure SettingsColorChange(AColor: TColor);

    // logging
    procedure LogEvent(const LogLevel: TLogLevel; const AText: String; const Timestamp: TDateTime);


  private
  protected

    // settings
    FStudioConfig: TStudioConfig;

    // display stuff
    FDisplayMgr: TDisplayManager;

    // objects to gather system information from
    FSysInfo: TSysInfo;
    FWinampControl: TWinampControl;

    FListIndex: Integer;           // position in ListBox
    FRemainingSeconds: Integer;    // remaining number of seconds the current screen is shown
    FIsCurrentScreenShownOnce: Boolean; // is the currently displayed screen to be shown only once?

    FClocks: array[0..(MAX_CLOCKS - 1)] of TClockData; // holds data of analog clocks
    FAnimationData: TAnimationData; // holds animation data
    FMatrix: TMatrixData; // holds the string drops used to display a Matrix like effect
    FWinampPlayBar: TWinampPlayBarData; // holds data related to the Winamp play bar

    IsCpuMonitorDisplayed: Boolean; // is the CPU usage monitor currently displayed?
    IsMemMonitorDisplayed: Boolean; // is the RAM usage monitor currently displayed?

  public
  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}

resourcestring
  { stop / play button }
  RsBtnStop = 'Stop';
  RsBtnGo = 'Go';

  { Next screen announcement }
  RsNextScreenText = 'Next screen in';

  { Showing the list name }
  RsCurrentlyDisplayed = 'Currently displayed';

  { TMainForm }




procedure TMainForm.CreateScreen;
var
  S: String;
  I: Integer;
  StartIndex: Integer;
  LastIndex: Integer;
begin

  // seek forward to next 'NEWSCREEN'
  I := SeekTo(FListIndex, sdForward, 'NEWSCREEN');

  if (-1 = I) then
  begin
    LogEvent(lvERROR, 'Could not find a NEWSCREEN command in the list.', Now);
  end
  else
  begin

    StartIndex := I;
    FListIndex := I;
    S := '';

    // interprete the commands until ENDSCREEN is reached (or the end of the list, which should be a ENDSCREEN command anyway)
    while (not S.StartsWith('ENDSCREEN')) do
    begin
      LastIndex := FListIndex;
      S := ListBox.items[FListIndex];
      I := InterpreteListCommand(S);
      if ((-1 <> I) and (I >= 0) and (I < ListBox.Items.Count)) then
      begin
        // InterpreteListCommand requested to jump to another position in ListBox.Items
        FListIndex := I;
        StartIndex := I;
      end
      else
      begin
        Inc(FListIndex);

        // check if the end of the list is reached
        if (FListIndex >= ListBox.Items.Count) then
        begin
          if (not S.StartsWith('ENDSCREEN')) then
          begin
            // this should not happen since the last command should have been a 'ENDSCREEN'
            LogEvent(lvERROR, 'List does not end with a ENDSCREEN command.', Now);
          end;
          FListIndex := 0;
          Break;
        end;

      end;
    end; // end: while

    // mark processed lines
    ListBox.MultiSelect := False;
    ListBox.MultiSelect := True;
    ListBox.ClearSelection;
    for I := StartIndex to LastIndex do
      ListBox.selected[I] := True;

  end; // NEWSCREEN command found

end;


procedure TMainForm.WaitTimerStartTimer(Sender: TObject);
begin
  FRemainingSeconds := WaitTimer.Interval div 1000;
  UpdateTimeLabel;
end;

procedure TMainForm.WaitTimerStopTimer(Sender: TObject);
begin
  UpdateTimeLabel;
end;


procedure TMainForm.WaitTimerTimer(Sender: TObject);
begin
  WaitTimer.Enabled := False;

  CreateScreen;
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
    FStudioConfig.ApplicationConfig.IconIndex := IniFile.ReadInteger('APPLICATION', 'IconIndex', 0);

    FStudioConfig.ApplicationConfig.DoClearOnExit := IniFile.ReadBool('APPLICATION', 'DspClearOnExit', False);
    FStudioConfig.ApplicationConfig.IsBrightnessControlledByList := IniFile.ReadBool('APPLICATION', 'DspBrightnessByList', True);
    FStudioConfig.ApplicationConfig.DisplayBrightness := Min(100, IniFile.ReadInteger('APPLICATION', 'DspBrightness', 100));

    { display section }
    FStudioConfig.DisplayConfig.DisplayType := IniFile.ReadString('DISPLAY', 'Type', '');
    FStudioConfig.DisplayConfig.ResX := IniFile.ReadInteger('DISPLAY', 'ResX', 128);
    FStudioConfig.DisplayConfig.ResY := IniFile.ReadInteger('DISPLAY', 'ResY', 64);
    FStudioConfig.DisplayConfig.IntName := IniFile.ReadString('DISPLAY', 'Interface', 'COM1');
    FStudioConfig.DisplayConfig.Baudrate := IniFile.ReadInteger('DISPLAY', 'Baud', 115200);

    { list section }
    FStudioConfig.ListConfig.ListName := IniFile.ReadString('LIST', 'Listname', 'Default.vfdlst');

    { animations section }
    FStudioConfig.AnimationConfig.PlayOnlyOnIdle := IniFile.ReadBool('ANIMATIONS', 'PlayOnlyOnIdle', True);
    FStudioConfig.AnimationConfig.IdlePercent := Min(100, IniFile.ReadInteger('ANIMATIONS', 'IdleLevel', 50));

  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.SaveConfig(const AFilePath: String);
var
  IniFile: TIniFile;
  ColorString: String;
begin
  try
    IniFile := TIniFile.Create(AFilePath);

    { application section }
    IniFile.WriteString('APPLICATION', 'Language', FStudioConfig.ApplicationConfig.Language);
    ColorString := Format('$%.6x', [FStudioConfig.ApplicationConfig.PreviewDisplayColor]);
    IniFile.WriteBool('APPLICATION', 'StartMinimized', FStudioConfig.ApplicationConfig.DoStartMinimized);
    IniFile.WriteInteger('APPLICATION', 'IconIndex', FStudioConfig.ApplicationConfig.IconIndex);
    IniFile.WriteString('APPLICATION', 'DspColor', ColorString);
    IniFile.WriteBool('APPLICATION', 'DspClearOnExit', FStudioConfig.ApplicationConfig.DoClearOnExit);
    IniFile.WriteBool('APPLICATION', 'DspBrightnessByList', FStudioConfig.ApplicationConfig.IsBrightnessControlledByList);
    IniFile.WriteInteger('APPLICATION', 'DspBrightness', FStudioConfig.ApplicationConfig.DisplayBrightness);

    { display section }
    IniFile.WriteString('DISPLAY', 'Type', FStudioConfig.DisplayConfig.DisplayType);
    IniFile.WriteInteger('DISPLAY', 'ResX', FStudioConfig.DisplayConfig.ResX);
    IniFile.WriteInteger('DISPLAY', 'ResY', FStudioConfig.DisplayConfig.ResY);
    IniFile.WriteString('DISPLAY', 'Interface', FStudioConfig.DisplayConfig.IntName);
    IniFile.WriteInteger('DISPLAY', 'Baud', FStudioConfig.DisplayConfig.Baudrate);

    { list section }
    IniFile.WriteString('LIST', 'Listname', FStudioConfig.ListConfig.ListName);

    { animations section }
    IniFile.WriteBool('ANIMATIONS', 'PlayOnlyOnIdle', FStudioConfig.AnimationConfig.PlayOnlyOnIdle);
    IniFile.WriteInteger('ANIMATIONS', 'IdleLevel', FStudioConfig.AnimationConfig.IdlePercent);

  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFilePath: String;
begin
  Randomize;

  LogEvent(lvINFO, 'Application started. Version ' + FSysInfo.ResourceVersionInfo, Now);

  FDisplayMgr := TDisplayManager.Create(Self);
  FDisplayMgr.OnDbgMessage := @LogEvent;
  FDisplayMgr.OnPreviewChanged := @HandlePreviewImageUpdate;


  FWinampControl := TWinampControl.Create(Self);

  IniFilePath := ExtractFilePath(application.ExeName) + STUDIO_INIFILE;
  LoadConfig(IniFilePath);

  // set controls accordingly to loaded settings
  if (FStudioConfig.ApplicationConfig.DoStartMinimized) then
    Hide;

  SetApplicationIcon;

  FDisplayMgr.PreviewColor := FStudioConfig.ApplicationConfig.PreviewDisplayColor;
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);

  FDisplayMgr.AddDisplay('PREVIEW', FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY, '', 0);

  FDisplayMgr.AddDisplay(FStudioConfig.DisplayConfig);

  PreviewImage.Width := FStudioConfig.DisplayConfig.ResX * 2;
  PreviewImage.Height := FStudioConfig.DisplayConfig.ResY * 2;
  PreviewImage.Picture.Bitmap.Width := FStudioConfig.DisplayConfig.ResX;
  PreviewImage.Picture.Bitmap.Height := FStudioConfig.DisplayConfig.ResY;

  FSysInfo := TSysInfo.Create(Self);

  FAnimationData.AnimationBitmap := TBitmap.Create;

  TrayIcon1.Hint := 'VFD-Studio 2: ' + FStudioConfig.DisplayConfig.DisplayType + '@' + FStudioConfig.DisplayConfig.IntName;

  VersionLabel.Caption := 'v' + FSysInfo.ResourceVersionInfo;

  // load passed file (otherwise just load last used file)
  if (Paramcount > 0) then begin
    // load passed list file
    if FileExists(ParamStr(1)) then
      FStudioConfig.ListConfig.ListName := ParamStr(1);
  end;


  // Starting the application might require quite some (CPU)time, so it's a good
  // idea to give the system some time. That's why loading the list is moved
  // to a timer which will add a small delay to this and which will disable
  // itself afterwards.
  LoadListTimer.Enabled := True;

end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AStringList: TStringList;
  I: Integer;
begin

  if (FStudioConfig.ApplicationConfig.DoClearOnExit) then
  begin
    if (nil <> FDisplayMgr) then
      FDisplayMgr.ClearScreen;
  end;

  LogEvent(lvINFO, 'Application closed.', Now);

  SaveConfig(ExtractFilePath(Application.ExeName) + STUDIO_INIFILE);

  AStringList := TStringList.Create;
  try
    AStringList.Add('#;Message;Level;Time');
    for I := 0 to (LogListView.Items.Count - 1) do
      AStringList.Add(LogListView.Items[I].Caption + ',' + // #
        '"' + LogListView.Items[i].SubItems[0] + '";' +    // Message
        LogListView.Items[i].SubItems[1] + ';' +           // Level
        LogListView.Items[i].SubItems[2]);                 // Time
    try
      AStringList.SaveToFile(ExtractFilePath(application.ExeName) + 'vfdstudio.log');
    finally
    end;
  finally
    AStringList.Free;
  end;

  CanClose := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSysInfo.Free;
  FWinampControl.Free;
  FAnimationData.AnimationBitmap.Free;
  SetLength(FMatrix.Drops, 0);
  FDisplayMgr.Free;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (wsMinimized = WindowState) then
    Hide;
end;

{
   Checks if the file exists, sets FStudioConfig.ListConfig.ListName accordingly and calls LoadList().
   Does NOT check if it is an actually readable list file!
   Returns False if the file could not be found.
}
function TMainForm.LoadListFromFile(const ListFileName: TFileName): Boolean;
var
  AFilename: TFileName;
begin
  Result:= False;
  AFilename := ListFileName;

  // if the file cannot be found, try the Lists-subdirectory
  if (not FileExists(AFileName)) then
    AFilename := ExtractFilePath(Application.ExeName) + 'Lists\' +  ExtractFileName(AFilename);

  if (FileExists(AFileName)) then
  begin
    FStudioConfig.ListConfig.ListName := AFileName;
    LoadList(AFileName);
    Result:= True;
  end
  else
    LogEvent(lvERROR, 'File "' + ListFileName + '" not found.', Now);
end;


{
  Loads a list from file and calls CreateScreen().
  Does NOT check in advance if the file actually exists! Use LoadListFromFile() to do so.
}
procedure TMainForm.LoadList(const ListFileName: TFileName);
var
  I: Integer;
  S: String;
  FileName: String; // file name of other list
  J: Integer;
begin
  StopProcessing;
  FListIndex := 0;

  ListGroupBox.Caption := RsCurrentlyDisplayed + ': ' + ExtractFileName(ListFileName);

  try
    //load list

    LogEvent(lvINFO, 'Loading file "' + ListFileName + '"', Now);
    ListBox.Items.LoadFromFile(ListFileName);

    //insert external lists
    I := 0;
    while (I < ListBox.Items.Count) do
    begin
      S := ListBox.items[I];
      if (Pos('LOADSCREEN', S) = 1) then
      begin
        FileName := Copy(S, 12, Length(S) - 11);
        ExtListBox.Items.LoadFromFile(FileName);
        ListBox.Items.Delete(I);
        // insert content from other list at position I
        for J := (ExtListBox.Items.Count - 1) downto 0 do
        begin
          ListBox.Items.Insert(I, ExtListBox.Items[J]);
        end;
      end;
      Inc(I);
    end;

    // remove all comment lines and empty lines
    I := 0;
    while (I <= ListBox.items.Count - 1) do
    begin
      S := ListBox.items[I];
      if (S = '') or (S[1] = ';') then
      begin
        ListBox.items.Delete(I);
        Dec(I);
      end;
      Inc(I);
    end; //while

    CreateScreen;
  except
    on E: Exception do
    begin
      LogEvent(lvError, E.Message, Now);
    end;
  end;
end;

{
  OnTimer event handling procedure of LoadListTimer.
}
procedure TMainForm.LoadListTimerTimer(Sender: TObject);
begin
  LoadListTimer.Enabled := False; // self-disabling

  LoadListFromFile(FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.InfoButtonClick(Sender: TObject);
begin
  InfoForm.Show;
end;

procedure TMainForm.UpdateTimeLabel;
var
  H, M, S: Integer;
begin
  if (WaitTimer.Enabled) then
  begin
    H := Trunc(FRemainingSeconds / 3600);
    M := Trunc((FRemainingSeconds mod 3600) / 60);
    S := FRemainingSeconds mod 60;
    ScreenTimeLabel.Caption := RsNextScreenText + Format(' %d:%.02d:%.02d', [H, M, S]);
  end
  else
  begin
    ScreenTimeLabel.Caption := RsNextScreenText + ' -: - -: - -';
  end;
end;

procedure TMainForm.InfoTimerTimer(Sender: TObject);
begin

  UpdateTimeLabel;
  if (FRemainingSeconds > 0) then
    Dec(FRemainingSeconds);

  // check if there are any varaiable infos
  if (FDisplayMgr.GetVariableInfoCount > 0) then
    FDisplayMgr.RefreshTextOutputs;

  RefreshClocks;

end;

procedure TMainForm.HyperlinkLabelClick(Sender: TObject);
begin
  OpenURL('http://cypax.net');
end;

// User whishes to jump to a specific line
procedure TMainForm.ListBoxDblClick(Sender: TObject);
var
  I: Integer;
begin
  // seek to the corresponding NEWSCREEN instruction
  I := SeekTo(ListBox.ItemIndex, sdBackward, 'NEWSCREEN');
  if (-1 <> I) then
  begin
    StopProcessing;
    FListIndex := I;
    CreateScreen;
  end;
end;

procedure TMainForm.MenuItemLoadListClick(Sender: TObject);
begin
  OpenListButtonClick(Self);
end;

// Halts all timers related to display output and clears all animation,
// information and clock data
procedure TMainForm.StopProcessing;
begin
  WaitTimer.Enabled := False;
  ExtraTimer.Enabled := False;
  StopAnimation; // this also disables the animation timer
  DisableClocks;
  SetLength(FMatrix.Drops, 0);
  FWinampPlayBar.IsActive:= False;
  FDisplayMgr.ClearInfoStrings;
  IsCpuMonitorDisplayed := False;
  IsMemMonitorDisplayed := False;
  StopButton.Caption := RsBtnStop;
  MenuItemStopGo.Caption := RsBtnStop;
end;

procedure TMainForm.OpenListButtonClick(Sender: TObject);
begin
  OpenDialog.InitialDir := extractFileDir(FStudioConfig.ListConfig.ListName);
  if (OpenDialog.Execute) then
  begin
    if (LoadListFromFile(OpenDialog.FileName)) then begin
      FStudioConfig.ListConfig.ListName := OpenDialog.FileName;
      WaitTimer.Enabled := True;
    end;
  end;
end;

procedure TMainForm.MenuItemReloadClick(Sender: TObject);
begin
  ReloadButtonClick(Self);
end;

procedure TMainForm.MenuItemNextClick(Sender: TObject);
begin
  NextButtonClick(Sender);
end;

procedure TMainForm.NextButtonClick(Sender: TObject);
var
  LI: Integer;
begin
  StopProcessing;

  LI := SeekTo(FListIndex, sdForward, 'NEWSCREEN');

  if (-1 = LI) then
    LogEvent(lvERROR, 'Could not find next NEWSCREEN command.', Now)
  else
  begin
    FListIndex := LI;
    CreateScreen;
  end;
end;

procedure TMainForm.OKButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.MenuItemStopGoClick(Sender: TObject);
begin
  StopButtonClick(Sender);
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  Mainform.LoadListFromFile(FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.SaveLogButtonClick(Sender: TObject);
var
  AStringList: TStringList;
  I: Integer;
begin
  if (SaveDialog.Execute) then
  begin

    AStringList := TStringList.Create;
    try
      AStringList.Add('#;Message;Level;Time');
      for I := 0 to (LogListView.Items.Count - 1) do
        AStringList.Add(LogListView.Items[I].Caption + ',' + // #
          '"' + LogListView.Items[i].SubItems[0] + '";' +    // Message
          LogListView.Items[i].SubItems[1] + ';' +           // Level
          LogListView.Items[i].SubItems[2]);                 // Time
      try
        AStringList.SaveToFile(SaveDialog.FileName);
      except
        on E: Exception do
          LogEvent(lvERROR, 'Could not save log file. Error: ' + E.Message, Now);
      end;
    finally
      AStringList.Free;
    end;

  end;
end;

procedure TMainForm.MenuItemMainWindowClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
end;

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  WaitTimer.Enabled := not WaitTimer.Enabled;
  if (True = Waittimer.Enabled) then
  begin
    StopButton.Caption := RsBtnStop;
    StopButton.ImageIndex := 0;
    MenuItemStopGo.Caption := RsBtnStop;
    MenuItemStopGo.ImageIndex := 0;
  end
  else
  begin
    StopButton.Caption := RsBtnGo;
    StopButton.ImageIndex := 1;
    MenuItemStopGo.Caption := RsBtnGo;
    MenuItemStopGo.ImageIndex := 1;
  end;
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure TMainForm.UsageTimerTimer(Sender: TObject);
begin
  FDisplayMgr.UpdateUsageMonitors(IsCpuMonitorDisplayed, IsMemMonitorDisplayed);
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.ExtraTimerTimer(Sender: TObject);
begin
  if (Length(FMatrix.Drops) > 0) then
  begin
    UpdateMatrixDrops;
    Inc(FMatrix.CycleCounter);
  end;

  if (FWinampPlayBar.IsActive) then
  begin
    DrawWinampPlayBar;
  end;

end;



procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  MainForm.Close;
end;



procedure TMainForm.Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, FrameWidth: Word);
begin
  try
    AnimateTimer.Enabled := False;

    FAnimationData.AnimationBitmap.LoadFromFile(FileName);

    FAnimationData.FrameIndex := 0;
    FAnimationData.FrameCount := (FAnimationData.AnimationBitmap.Width div FrameWidth);
    FAnimationData.FrameWidth := FrameWidth;
    FAnimationData.XPos := XPosition;
    FAnimationData.YPos := YPosition;
    AnimateTimer.Interval := AnimationSpeed;
    AnimateTimer.Enabled := True;
  except
    on E: Exception do
    begin
      AnimateTimer.Enabled := False;
      LogEvent(lvERROR, 'Could not load animation from file ' + FileName, Now);
    end;
  end;

end;

procedure TMainForm.PauseAnimation;
begin
  if (nil <> FAnimationData.AnimationBitmap) then
    AnimateTimer.Enabled := not AnimateTimer.Enabled;    //nil in FAnimationBitmap würde bei bei Animation zu Exception führen
end;

procedure TMainForm.StopAnimation;
begin
  AnimateTimer.Enabled := False;
  FAnimationData.FrameIndex := 0;
  FAnimationData.IsAnimationDisplayed := False;
end;

procedure TMainForm.AnimateTimerTimer(Sender: TObject);
begin
  if (not FStudioConfig.AnimationConfig.PlayOnlyOnIdle) or (FStudioConfig.AnimationConfig.IdlePercent > FDisplayMgr.GetAverageCpuLoad) then
  begin

    DrawAnimationFrame(FAnimationData.AnimationBitmap, FAnimationData.XPos, FAnimationData.YPos, FAnimationData.FrameIndex, FAnimationData.FrameWidth);

    Inc(FAnimationData.FrameIndex);
    if (FAnimationData.FrameIndex >= FAnimationData.FrameCount) then
      FAnimationData.FrameIndex := 0;

  end;
end;

procedure TMainForm.CfgButtonClick(Sender: TObject);
begin
  ConfigForm.ColorButton.ButtonColor := FStudioConfig.ApplicationConfig.PreviewDisplayColor;
  ConfigForm.StartMinimizedCheckBox.Checked := FStudioConfig.ApplicationConfig.DoStartMinimized;
  ConfigForm.BrightListRadioButton.Checked := FStudioConfig.ApplicationConfig.IsBrightnessControlledByList;
  ConfigForm.BrightSettingsRadioButton.Checked := not FStudioConfig.ApplicationConfig.IsBrightnessControlledByList;
  ConfigForm.BrightnessTrackBar.Position := FStudioConfig.ApplicationConfig.DisplayBrightness;
  ConfigForm.BrightnessPercentLabel.Caption := IntToStr(FStudioConfig.ApplicationConfig.DisplayBrightness) + '%';
  ConfigForm.OnlyOnIdleBox.Checked := FStudioConfig.AnimationConfig.PlayOnlyOnIdle;
  ConfigForm.IdleTrackBar.Position := FStudioConfig.AnimationConfig.IdlePercent;
  ConfigForm.IdlePercentageLabel.Caption := IntToStr(FStudioConfig.AnimationConfig.IdlePercent) + '%';
  ConfigForm.ClearOnCloseCheckBox.Checked := FStudioConfig.ApplicationConfig.DoClearOnExit;
  ConfigForm.Language := FStudioConfig.ApplicationConfig.Language;

  ConfigForm.InterfaceCombo.Caption := FStudioConfig.DisplayConfig.IntName;
  ConfigForm.ResXSpinEdit.Value := FStudioConfig.DisplayConfig.ResX;
  ConfigForm.ResYSpinEdit.Value := FStudioConfig.DisplayConfig.ResY;
  ConfigForm.IfCfgCombo.Caption := IntToStr(FStudioConfig.DisplayConfig.Baudrate);

  if (FStudioConfig.DisplayConfig.DisplayType = 'NTK800') then
    ConfigForm.DspTypeCombo.ItemIndex := 1
  else
  if (FStudioConfig.DisplayConfig.DisplayType = 'NTK300') then
    ConfigForm.DspTypeCombo.ItemIndex := 2
  else
    if (FStudioConfig.DisplayConfig.DisplayType = 'U8G2') then
    ConfigForm.DspTypeCombo.ItemIndex := 3
  else
    ConfigForm.DspTypeCombo.ItemIndex := 0;

  ConfigForm.OnAbortPressed := @SettingsAbortPressed;
  ConfigForm.OnOkPressed := @SettingsOkPressed;
  ConfigForm.OnColorChange := @SettingsColorChange;

  SetApplicationIcon;
  ConfigForm.IconIndex := FStudioConfig.ApplicationConfig.IconIndex;

  ConfigForm.Show;
end;

procedure TMainForm.HandlePreviewImageUpdate(NewImage: TBitmap);
begin
  PreviewImage.Picture.Bitmap.Canvas.Draw(0, 0, NewImage);
end;

function TMainForm.FindWindowByTitle(WindowTitle: String): Hwnd;
var
  NextHandle: Hwnd;
  NextTitle: array[0..260] of Char;
begin
  // Get the first window
  NextHandle := GetWindow(Application.Handle, GW_HWNDFIRST);
  while NextHandle > 0 do
  begin
    // retrieve its text
    GetWindowText(NextHandle, NextTitle, 255);
    if Pos(WindowTitle, StrPas(NextTitle)) <> 0 then
    begin
      Result := NextHandle;
      Exit;
    end
    else
      // Get the next window
      NextHandle := GetWindow(NextHandle, GW_HWNDNEXT);
  end;
  Result := 0;
end;

procedure TMainForm.ListEditorButtonClick(Sender: TObject);
var
  Process: TProcess;
  H: HWND;
begin
  SaveConfig(ExtractFilePath(Application.ExeName) + STUDIO_INIFILE);

  H := FindWindowByTitle('List Editor 2');
  if H <> 0 then // if we found notepad
    // bring to foreground
    SetForegroundWindow(H)
  else
  begin
    // start List Editor
    Process := TProcess.Create(nil);
    try
      Process.Executable := 'ListEditor.exe';
      Process.Options := [poNoConsole];
      Process.Execute;
    finally
      Process.Free;
    end;
  end;
end;

procedure TMainForm.ExpertViewButtonClick(Sender: TObject);
begin
  LogfileGroupBox.Visible := not LogfileGroupBox.Visible;
  ListGroupBox.Visible := LogfileGroupBox.Visible;

  (*
  if (LogfileGroupBox.Visible) then begin
    ExpertViewButton.ImageIndex:= 2;
  end else begin
    ExpertViewButton.ImageIndex:= 3;
  end;
  *)

end;

procedure TMainForm.ClearLogButtonClick(Sender: TObject);
begin
  LogListView.Clear;
end;


procedure TMainForm.DrawAnimationFrame(ABitmap: TBitmap; X, Y, Frame, FrameWidth: Word);
var
  TmpBitmap: TBitmap;
  sRect, dRect: TRect;
begin
  try
    TmpBitmap := TBitmap.Create;
    TmpBitmap.Height := ABitmap.Height;
    TmpBitmap.Width := FrameWidth;
    sRect := Rect(Frame * FrameWidth, 0, Frame * FrameWidth + FrameWidth, ABitmap.Height);
    dRect := Rect(0, 0, FrameWidth, ABitmap.Height);
    TmpBitmap.Canvas.CopyRect(dRect, ABitmap.Canvas, sRect);
    FDisplayMgr.PaintBitmap(TmpBitmap, X, Y);
    TmpBitmap.Canvas.Pixels[0, 0] := TmpBitmap.Canvas.Pixels[0, 0]; // this seems like nonsense but is required to actually load the bitmap in memory
  finally
    TmpBitmap.Free;
  end;
end;

procedure TMainForm.LogEvent(const LogLevel: TLogLevel; const AText: String; const Timestamp: TDateTime);
var
  Item: TListItem;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(Timestamp, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);

  Item := LogListView.Items.Add;
  Item.Caption := IntToStr(LogListView.Items.Count);
  Item.Subitems.Add(AText);
  case LogLevel of
    lvINFO: Item.Subitems.Add('INFO');
    lvWARNING: Item.Subitems.Add('WARNING');
    lvERROR: Item.Subitems.Add('ERROR');
    else
      Item.Subitems.Add('CRITICAL');
  end;
  Item.Subitems.Add(Format('%.04d-%.02d-%.02d %.02d:%.02d:%.02d.%.03d', [AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond]));
end;

// Starting at StartIndex this function seeks forward/backward to the next line in ListBox which starts with the questioned string.
// String comparison is not cases-sensitive.
// Will continue on start/end of list.
// Returns the index of the line or -1 if it could not be found.
// Returns -1 if StartIndex is out of bounds or the requested string is empty.
function TMainForm.SeekTo(StartIndex: Integer; SearchDirection: TSearchDirection; Cmd: String): Integer;
var
  I: Integer;
  S: String;
  Res: Integer;
begin

  if (('' = Cmd) or (StartIndex >= ListBox.Items.Count) or (StartIndex < 0)) then
  begin
    Result := -1;
  end
  else
  begin
    I := StartIndex;
    Res := -1;

    repeat
      S := ListBox.Items[I];
      if (S.ToUpper.StartsWith(Cmd.ToUpper)) then
      begin
        Res := I;
        Break;
      end;

      if (sdForward = SearchDirection) then
      begin
        Inc(I);
        if (I >= ListBox.Items.Count) then
          I := 0;
      end
      else
      begin
        Dec(I);
        if (I < 0) then
          I := ListBox.Items.Count - 1;
      end;

      // have we reached the point where we started from?
      if (StartIndex = I) then
        Break;
    until (-1 <> Res);

    Result := Res;
  end;
end;

procedure TMainForm.AddClock(Offset: Integer; X, Y, HourHandLength, MinuteHandLength, SecondsHandLength: Word);
var
  I: Integer;
  IsFreeSlotFound: Boolean;
begin
  IsFreeSlotFound := False;

  // add clock to next free slot
  for I := 0 to (MAX_CLOCKS - 1) do
  begin
    if (not FClocks[I].IsActive) then
    begin
      FClocks[I].Offset := Offset;
      FClocks[I].X := X;
      FClocks[I].Y := Y;
      FClocks[I].HourHandLength := HourHandLength;
      FClocks[I].MinuteHandLength := MinuteHandLength;
      FClocks[I].SecondsHandLength := SecondsHandLength;
      FClocks[I].IsActive := True;
      IsFreeSlotFound := True;
      Break;
    end;
  end;

  if (not IsFreeSlotFound) then
  begin
    LogEvent(lvERROR, 'Not more than ' + IntToStr(MAX_CLOCKS) + ' clocks can be shown at the same time.', Now);
  end;
end;

procedure TMainForm.DisableClocks;
var
  I: Integer;
begin
  for I := 0 to (MAX_CLOCKS - 1) do
  begin
    FClocks[I].IsActive := False;
    FClocks[I].HourPoint := Point(-1, -1);
    FClocks[I].MinutePoint := Point(-1, -1);
    FClocks[I].SecondsPoint := Point(-1, -1);
  end;
end;

procedure TMainForm.RefreshClocks;
var
  I: Integer;
  CurrentDateTime: TDateTime;
  ClockDateTime: TDateTime;
  HH, MM, SS, MS: Word;
  X0, Y0, X1, Y1: Integer;
  Angle: Double;
  Distance: Integer;
const
  MIN_DISTANCE = 5;
begin
  CurrentDateTime := Now;

  for I := 0 to (MAX_CLOCKS - 1) do
  begin
    if (FClocks[I].IsActive) then
    begin
      ClockDateTime := IncMinute(CurrentDateTime, FClocks[I].Offset);
      DecodeTime(ClockDateTime, HH, MM, SS, MS);

      X0 := FClocks[I].X;
      Y0 := FClocks[I].Y;

      // draw seconds hand
      if (FClocks[I].SecondsHandLength > 0) then
      begin

        // calculate X1, Y1
        // convert seconds to angle (in radians)
        Angle := DegToRad((SS mod 60) * 6); // 6 * 60 = 360°

        // calculate X1, Y1 coordinates
        X1 := Round(X0 + FClocks[I].SecondsHandLength * Sin(Angle));
        Y1 := Round(Y0 - FClocks[I].SecondsHandLength * Cos(Angle));


        // do something in case we have new coordinates
        if ((X1 <> FClocks[I].SecondsPoint.X) or (Y1 <> FClocks[I].SecondsPoint.Y)) then
        begin
          // clear previous hand
          if (FClocks[I].SecondsPoint.X <> -1) then
          begin
            FDisplayMgr.PaintLine(X0, Y0, FClocks[I].SecondsPoint.X, FClocks[I].SecondsPoint.Y, True);
          end;
          // draw new hand
          FDisplayMgr.PaintLine(X0, Y0, X1, Y1, False);
          // remember coordinates
          FClocks[I].SecondsPoint.X := X1;
          FClocks[I].SecondsPoint.Y := Y1;
        end;

      end; // if SecondsHandLength > 0


      // draw minutes hand
      if (FClocks[I].MinuteHandLength > 0) then
      begin

        // calculate X1, Y1
        // convert minute to angle (in radians)
        Angle := DegToRad((MM mod 60) * 6); // 6 * 60 = 360°

        // calculate X1, Y1 coordinates
        X1 := Round(X0 + FClocks[I].MinuteHandLength * Sin(Angle));
        Y1 := Round(Y0 - FClocks[I].MinuteHandLength * Cos(Angle));

        // check if the seconfs hand is near the minute hand
        if (FClocks[I].SecondsHandLength > 0) then
        begin
          Distance := Abs(MM - SS);
          if (Distance > 30) then
            Distance := Abs(60 - Distance);
        end
        else
          Distance := MIN_DISTANCE;

        // do something in case we have new coordinates or if SS is near MM
        if ((X1 <> FClocks[I].MinutePoint.X) or (Y1 <> FClocks[I].MinutePoint.Y) or (Distance < MIN_DISTANCE)) then
        begin
          // clear previous hand
          if (FClocks[I].MinutePoint.X <> -1) then
          begin
            FDisplayMgr.PaintLine(X0, Y0, FClocks[I].MinutePoint.X, FClocks[I].MinutePoint.Y, True);
          end;
          // draw new hand
          FDisplayMgr.PaintLine(X0, Y0, X1, Y1, False);
          // remember coordinates
          FClocks[I].MinutePoint.X := X1;
          FClocks[I].MinutePoint.Y := Y1;
        end;

      end; // if MinuteHandLength > 0


      // draw hour hand
      if (FClocks[I].HourHandLength > 0) then
      begin

        // calculate X1, Y1
        // convert hour to angle (in radians)
        Angle := DegToRad(((HH mod 12) * 30) + (MM * 30 div 60)); // 12 * 30 = 360°

        // calculate X1, Y1 coordinates
        X1 := Round(X0 + FClocks[I].HourHandLength * Sin(Angle));
        Y1 := Round(Y0 - FClocks[I].HourHandLength * Cos(Angle));

        // check if the seconds hand is near the hour hand
        if (FClocks[I].SecondsHandLength > 0) then
        begin
          Distance := Abs((HH * 5 + (MM * 5 div 60)) - SS);
          if (Distance > 30) then
            Distance := Abs(60 - Distance);
        end
        else
          Distance := MIN_DISTANCE;

        // now, if Distance in not already below MIN_DISTANCE, then check if the minute hand is near the hour hand
        if (Distance >= MIN_DISTANCE) then
        begin
          Distance := Abs((HH * 5) - MM);
          if (Distance > 30) then
            Distance := Abs(60 - Distance);
        end;

        // do something in case we have new coordinates or if SS or MM is near HH
        if ((X1 <> FClocks[I].HourPoint.X) or (Y1 <> FClocks[I].HourPoint.Y) or (Distance < MIN_DISTANCE)) then
        begin
          // clear previous hand
          if (FClocks[I].HourPoint.X <> -1) then
          begin
            FDisplayMgr.PaintLine(X0, Y0, FClocks[I].HourPoint.X, FClocks[I].HourPoint.Y, True);
          end;
          // draw new hand
          FDisplayMgr.PaintLine(X0, Y0, X1, Y1, False);
          // remember coordinates
          FClocks[I].HourPoint.X := X1;
          FClocks[I].HourPoint.Y := Y1;
        end;

      end; // if HandHandLength > 0

    end;
  end;
end;


procedure TMainForm.AddMatrixDrop(const AText: String; MaxTextLen, Row: Integer; SlownessFactor: Byte);
var
  Drop: TMatrixDrop;
begin
  SetLength(FMatrix.Drops, Length(FMatrix.Drops) + 1);

  Drop.Text := AText;
  Drop.MaxTextLen := MaxTextLen;
  Drop.Row := Row;
  Drop.SlownessFactor := SlownessFactor;

  FMatrix.Drops[High(FMatrix.Drops)] := Drop;
end;

procedure TMainForm.UpdateMatrixDrops;
var
  I, N: Integer;
  PDrop: ^TMatrixDrop;
  Pos: Integer;
  C: Char;
  DspRowCount: Integer;
  BottomRow, TopRow: Integer;
  ARow: Integer;
  DoRepaintFirstChar: Boolean;
  DoRepaintAll: Boolean;
const
  MATRIXLETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789^!"§$%&/()=?`°²{[]}\*+~#µ<>|@';
begin
  DspRowCount := (FStudioConfig.DisplayConfig.ResY div GLYPH_H);

  for I := 0 to High(FMatrix.Drops) do
  begin
    PDrop := @FMatrix.Drops[I];

    DoRepaintFirstChar := False;
    DoRepaintAll := False;


    // randomly select a new character
    Pos := Random(Length(MATRIXLETTERS)) + 1;
    C := MATRIXLETTERS[Pos];

    if (Length(PDrop^.Text) < PDrop^.MaxTextLen) then
    begin
      // insert new character at begin if max length is not yet reached
      PDrop^.Text := C + PDrop^.Text;
      DoRepaintAll := True;
      DoRepaintFirstChar := True;
    end
    else
    begin
      // just change first character
      PDrop^.Text[1] := C;
      DoRepaintFirstChar := True;
    end;

    // check if this drop is to be moved at all
    if ((FMatrix.CycleCounter mod PDrop^.SlownessFactor) = 0) then
    begin
      // inc position and restart at begin if needed
      Inc(PDrop^.Row);
      DoRepaintAll := True;
      DoRepaintFirstChar := True;

      if (PDrop^.Row > (DspRowCount + Length(PDrop^.Text) + 1)) then
      begin
        // restart drop at random position above the visible display area
        PDrop^.Row := (Length(PDrop^.Text) + Random(20)) * -1;
        PDrop^.SlownessFactor := Random(3) + 1;
        DoRepaintAll := False;
        DoRepaintFirstChar := False;
        // replace drop characters with random content
        for Pos := 1 to (PDrop^.MaxTextLen - 1) do
        begin
          C := MATRIXLETTERS[Random(Length(MATRIXLETTERS)) + 1];
          PDrop^.Text[Pos] := C;
        end;
      end;
    end;

    BottomRow := PDrop^.Row;
    TopRow := PDrop^.Row - Length(PDrop^.Text);

    // check if the drop is (at least partially) within the visible display area
    if ((BottomRow >= 0) and (TopRow < (DspRowCount - 1))) then
    begin

      if (DoRepaintAll) then
        N := Length(PDrop^.Text)
      else
        N := 1;

      // check if the drop actually needs to be repainted
      if (DoRepaintAll or DoRepaintFirstChar) then
      begin

        for Pos := 1 to N do
        begin
          ARow := BottomRow - Pos + 1;
          if (ARow < 0) then
            Break;
          if (ARow >= DspRowCount) then
            Continue;

          FDisplayMgr.PaintString(PDrop^.Text[Pos], I, ARow);

        end;
      end;
    end;

  end;
end;


procedure TMainForm.InitMatrix;
var
  I: Integer;
  AText: String;
  MaxTextLen, Row: Integer; SlownessFactor: Byte;
begin

  SetLength(FMatrix.Drops, 0);
  for I := 1 to (FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP)) do
  begin
    MaxTextLen := Random(4) + 4;
    SlownessFactor := Random(3) + 1;
    AText := ' ';
    Row := (Random(10) + 1) * -1;
    AddMatrixDrop(AText, MaxTextLen, Row, SlownessFactor);
  end;
end;

procedure TMainForm.DrawDriveUsage(DriveLetter: Char; Col, Row, BarWidth: Integer);
var
  TotalMem: QWord;
  FreeMem: QWord;
  PercentFree: Double;
begin
  // get drive information
  TotalMem := FSysInfo.GetDiskSpace(DriveLetter);
  FreeMem := FSysInfo.GetFreeDiskSpace(DriveLetter);

  // calculate percentage of free bytes
  PercentFree := FreeMem / TotalMem;

  DrawProgressBar(Col, Row, BarWidth, 1.0 - PercentFree);
end;


procedure TMainForm.DrawWinampPlayBar;
var
  TrackPos, TrackLen: Integer;
  PercentPlayed: Double;
  X, Y: Integer;
begin
  // get drive information
  TrackPos := FWinampControl.GetOffset div 1000;
  TrackLen := FWinampControl.GetLength;

  // calculate percentage of free bytes
  if (TrackLen > 0) then
    PercentPlayed := TrackPos / TrackLen
  else
    PercentPlayed := 0.0;

  if (FWinampPlayBar.IsGraphical) then
  begin
    X := FWinampPlayBar.Col;
    Y := FWinampPlayBar.Row;
    X := X + Round(FWinampPlayBar.Width * PercentPlayed);
    FDisplayMgr.PaintLine(FWinampPlayBar.Col, Y-2, FWinampPlayBar.Col + FWinampPlayBar.Width, Y-2, True);
    FDisplayMgr.PaintLine(FWinampPlayBar.Col, Y-1, FWinampPlayBar.Col + FWinampPlayBar.Width, Y-1, True);
    FDisplayMgr.PaintLine(FWinampPlayBar.Col, Y+2, FWinampPlayBar.Col + FWinampPlayBar.Width, Y+2, True);
    FDisplayMgr.PaintLine(FWinampPlayBar.Col, Y+1, FWinampPlayBar.Col + FWinampPlayBar.Width, Y+1, True);
    FDisplayMgr.PaintLine(X, Y-2, X, Y+2, False);
    X := FWinampPlayBar.Col;
    FDisplayMgr.PaintLine(X, Y, X + FWinampPlayBar.Width, Y, False);
  end else
    DrawProgressBar(FWinampPlayBar.Col, FWinampPlayBar.Row, FWinampPlayBar.Width, PercentPlayed);
end;

procedure TMainForm.DrawProgressBar(Col, Row, BarWidth: Integer; Percentage: Double);
var
  X: Integer;
  C: Char;
  FilledNum: Integer;
begin
  // calculate number of characters in bar which are used
  FilledNum := Round(BarWidth * Percentage);

  for X := 0 to (BarWidth - 1) do
  begin
    if (X < FilledNum) then
      C := Chr($87)
    else
      C := Chr($8E);
    FDisplayMgr.PaintString(C, X + Col, Row);
  end;
end;



// Interpretes a command (from ListBox), given and parameter S.
// If the command requires jumping formward/backwards the list, the desired new
// list index is returned; otherwise -1.
// Reads FListIndex; does not modify FListIndex.
function TMainForm.InterpreteListCommand(S: String): Integer;
var
  CmdParts: TStringList;
  Cmd: String;
  n, ITmp: Integer;
  X, Y: Integer;
  P1, P2, P3, P4, P5, P6: Integer;
  IsInverted: Boolean;
  IsRequirementMet: Boolean;
  Res: Integer;
  DoContinueScreen: Boolean;
begin
  Res := -1;

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

        FIsCurrentScreenShownOnce := False; // by default this screen will be shown again then the list loops
        DoContinueScreen := False;          // by default, NEWSCREEN will stop animations, clear/reset variable information, etc.

        if (CmdParts.Count >= 2) then
        begin
          // p1 = screen limitations
          if ('ONCE' = CmdParts[1]) then
          begin
            // display this screen only once
            FIsCurrentScreenShownOnce := True;
          end
          else
          if ('CONTINUE' = CmdParts[1]) then
          begin
            // do not stop animations, do not clear/reset variable infomration, etc.
            DoContinueScreen := True;
          end
          else
          if (CmdParts[1].StartsWith('REQUIRE')) then
          begin
            // this screen shall only be shown if a requirement is met
            IsRequirementMet := False; // False by default, must be set to True in code below

            if ('REQUIREWINAMP' = CmdParts[1]) then
            begin
              // check if Winamp is running
              if (FWinampControl.IsWinampRunning) then
                IsRequirementMet := True;
            end;

            if (not IsRequirementMet) then
            begin
              //LogEvent(lvINFO, 'Requirment ' + CmdParts[1] + ' is not met -> skip to next screen', Now);

              // seek next 'NEWSCREEN' line; starting with next line
              ITmp := FListIndex + 1;
              if (ITmp >= ListBox.Items.Count) then
                ITmp := 0;
              Res := SeekTo(ITmp, sdForward, 'NEWSCREEN');

              if (-1 = Res) then
                LogEvent(lvERROR, 'Could not find the next screen in the list.', Now);

            end; // if requirement not met
          end; // if something required
        end; // if 2nd parameter with screen limitations given

        if (not DoContinueScreen) then
        begin
          StopProcessing;
        end;

        FDisplayMgr.ShowScreen(BOTH_LAYERS);
        FDisplayMgr.SetLayerMode(lmXOR);

        if (not FStudioConfig.ApplicationConfig.IsBrightnessControlledByList) then
        begin
          FDisplayMgr.SetBrightness(FStudioConfig.ApplicationConfig.DisplayBrightness);
        end;

      end
      else
      if ('ENDSCREEN' = Cmd) then
      begin
        if (True = FIsCurrentScreenShownOnce) then
        begin
          // remove this screen from the list

          // go back to last 'NEWSCREEN' line
          ITmp := SeekTo(FListIndex, sdBackward, 'NEWSCREEN');

          if ((-1 = ITmp) or (ITmp > FListIndex)) then
            LogEvent(lvERROR, 'Could not find start of current screen.', Now)
          else
          begin
            LogEvent(lvINFO, 'This screen was once. Clearing lines ' + IntToStr(ITmp) + '..' + IntToStr(FListIndex), Now);
            ListBox.MultiSelect := True;
            ListBox.ClearSelection;
            for n := ITmp to FListIndex do
            begin
              ListBox.selected[n] := True;
            end;
            ListBox.DeleteSelected;
            FIsCurrentScreenShownOnce := False;

            Res := ITmp - 1;
            if (Res < 0) then
              Res := ListBox.Items.Count - 1;
          end;
        end;

        (* obsolete
      end else if ('DSPINIT' = Cmd) then begin
        if (nil <> FDisplay) then
          FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);
          *)

      end
      else
      if ('ORMODE' = Cmd) then
      begin
        FDisplayMgr.SetLayerMode(lmOR);

      end
      else
      if ('XORMODE' = Cmd) then
      begin
        FDisplayMgr.SetLayerMode(lmXOR);

      end
      else
      if ('ANDMODE' = Cmd) then
      begin
        FDisplayMgr.SetLayerMode(lmAND);

      end
      else
      if ('STOP' = Cmd) then
      begin
        WaitTimer.Enabled := False;
        StopButton.Caption := RsBtnGo;
        MenuItemStopGo.Caption := RsBtnGo;
        StopButton.ImageIndex := 1;

      end
      else
      if ('CLEARSCREEN' = Cmd) then
      begin
        FDisplayMgr.ClearScreen;

      end
      else
      if ('SCREENTIME' = Cmd) then
      begin
        // p1 = screen time in seconds
        if (CmdParts.Count >= 2) then
        begin
          P1 := StrToInt(CmdParts[1]);
          WaitTimer.Interval := P1 * 1000;
          //ScreenTimeLabel.caption:= 'ScreenTime: ' + floattostr(WaitTimer.Interval/1000)+ 'S';
          WaitTimer.Enabled := True;
        end;

      end
      else
      if ('LIGHT' = Cmd) then
      begin
        // p1 = brightness level
        if (FStudioConfig.ApplicationConfig.IsBrightnessControlledByList) then
        begin
          if (CmdParts.Count >= 2) then
          begin
            P1 := StrToInt(CmdParts[1]);
            FDisplayMgr.SetBrightness(P1);
          end;
        end;

      end
      else
      if ('NOISE' = Cmd) then
      begin
        // p1 = amount of pixels, p2 = inverted or not [optional]
        if (CmdParts.Count >= 2) then
        begin
          P1 := StrToInt(CmdParts[1]);
          if ((CmdParts.Count >= 3) and ((CmdParts[2].ToUpper = 'TRUE') or (CmdParts[2] = '1'))) then
          begin
            IsInverted := True;
          end
          else
          begin
            IsInverted := False;
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
          P1 := StrToInt(CmdParts[1]);
          P2 := StrToInt(CmdParts[2]);
          if ((CmdParts.Count >= 4) and ((CmdParts[3].ToUpper = 'TRUE') or (CmdParts[3] = '1'))) then
          begin
            FDisplayMgr.PaintPixel(P1, P2, True);
          end
          else
          begin
            FDisplayMgr.PaintPixel(P1, P2, False);
          end;
        end;

      end
      else
      if ('LINE' = Cmd) then
      begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then
        begin
          P1 := StrToInt(CmdParts[1]);
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          P4 := StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then
          begin
            FDisplayMgr.PaintLine(P1, P2, P3, P4, True);
          end
          else
          begin
            FDisplayMgr.PaintLine(P1, P2, P3, P4, False);
          end;
        end;

      end
      else
      if ('FRAME' = Cmd) then
      begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then
        begin
          P1 := StrToInt(CmdParts[1]);
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          P4 := StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then
          begin
            FDisplayMgr.PaintFrame(P1, P2, P3, P4, True);
          end
          else
          begin
            FDisplayMgr.PaintFrame(P1, P2, P3, P4, False);
          end;
        end;

      end
      else
      if ('PLAINTEXT' = Cmd) then
      begin
        // p1 = text, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then
        begin
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$')) then
          begin
            FDisplayMgr.HandleTextOutput(CmdParts[1], P2, P3, '', 0);
          end
          else
          begin
            FDisplayMgr.PaintString(CmdParts[1], P2, P3);
          end;
        end;

      end
      else
      if ('TEXTOUT' = Cmd) then
      begin
        // p1 = text, p2 = x, p3 = y, p4 = font size, p5 = font name
        if (CmdParts.Count >= 6) then
        begin
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          P4 := StrToInt(CmdParts[4]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$')) then
          begin
            FDisplayMgr.HandleTextOutput(CmdParts[1], P2, P3, CmdParts[5], P4);
          end
          else
          begin
            FDisplayMgr.DrawFontedText(CmdParts[1], P2, P3, CmdParts[5], P4);
          end;
        end;

      end
      else
      if ('ANIMATE' = Cmd) then
      begin
        // p1 = file name, p2 = animation speed, p3 = x, p4 = y, p5 = frame width
        if (False = FAnimationData.IsAnimationDisplayed) then
        begin
          // only one animation per screen is supported
          if (CmdParts.Count >= 6) then
          begin
            P2 := StrToInt(CmdParts[2]);
            P3 := StrToInt(CmdParts[3]);
            P4 := StrToInt(CmdParts[4]);
            P5 := StrToInt(CmdParts[5]);
            Animate(CmdParts[1], P2, P3, P4, P5);
            FAnimationData.IsAnimationDisplayed := True;
          end;
        end;

      end
      else
      if ('BITMAP' = Cmd) then
      begin
        // p1 = file path, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then
        begin
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          FDisplayMgr.PaintBitmapFromFile(CmdParts[1], P2, P3);
        end;

      end
      else
      if ('WAPLAYBAR' = Cmd) then
      begin
        // P1 = x, P2 = y, P3 = width (in characters), P4 = graphical bar? [optional]
        if (CmdParts.Count >= 4) then
        begin
          P1 := StrToInt(CmdParts[1]);
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          if ((CmdParts.Count >= 5) and ((CmdParts[4].ToUpper = 'TRUE') or (CmdParts[4] = '1'))) then
            FWinampPlayBar.IsGraphical:= True
          else
            FWinampPlayBar.IsGraphical:= False;
          FWinampPlayBar.Col := P1;
          FWinampPlayBar.Row := P2;
          FWinampPlayBar.Width:= Max(1, P3);
          FWinampPlayBar.IsActive:= True;
          DrawWinampPlayBar;
          ExtraTimer.Interval := 500;
          ExtraTimer.Enabled := True;
        end;

      end
      else
      if ('DRIVEUSAGE' = Cmd) then
      begin
        // p1 = drive letter, P2 = x, P3 = y, P4 = width (in characters)
        if (CmdParts.Count >= 5) then
        begin
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          P4 := StrToInt(CmdParts[4]);
          DrawDriveUsage(CmdParts[1][1], P2, P3, P4);
        end;

      end
      else
      if ('CLOCK' = Cmd) then
      begin
        // p1 = offset in minutes, p2 = x, p3 = y, p4 = hour hand length, p5 = minute hand length, p6 = seconds hand length
        if (CmdParts.Count >= 7) then
        begin
          P1 := StrToInt(CmdParts[1]);
          P2 := StrToInt(CmdParts[2]);
          P3 := StrToInt(CmdParts[3]);
          P4 := StrToInt(CmdParts[4]);
          P5 := StrToInt(CmdParts[5]);
          P6 := StrToInt(CmdParts[6]);
          AddClock(P1, P2, P3, P4, P5, P6);
        end;

      end
      else
      if ('MATRIX' = Cmd) then
      begin
        // p1 = speed
        if (CmdParts.Count >= 2) then
        begin
          P1 := StrToInt(CmdParts[1]);
          // Setting up the drops
          InitMatrix;
          ExtraTimer.Interval := P1;
          ExtraTimer.Enabled := True;
        end;

      end
      else
      if ('CPUMONITOR' = Cmd) then
      begin
        // p1 = number of rows to use, p2 = bottom-most row
        if (CmdParts.Count >= 3) then
        begin
          if (False = IsCpuMonitorDisplayed) then
          begin
            P1 := StrToInt(CmdParts[1]);
            P2 := StrToInt(CmdParts[2]);
            FDisplayMgr.ConfigureCpuMonitor(P1, P2);
            IsCpuMonitorDisplayed := True;
          end;
        end;

      end
      else
      if ('RAMMONITOR' = Cmd) then
      begin
        // p1 = number of rows to use, p2 = bottom-most row
        if (CmdParts.Count >= 3) then
        begin
          if (False = IsMemMonitorDisplayed) then
          begin
            P1 := StrToInt(CmdParts[1]);
            P2 := StrToInt(CmdParts[2]);
            FDisplayMgr.ConfigureMemMonitor(P1, P2);
            IsMemMonitorDisplayed := True;
          end;
        end;

      end;

    end; // endif (CmdParts.Count > 0)

  finally
    CmdParts.Free;
  end;
  Result := Res;
end;

procedure TMainForm.SettingsOkPressed;
begin
  FStudioConfig.ApplicationConfig.PreviewDisplayColor := ConfigForm.ColorButton.ButtonColor;
  FDisplayMgr.PreviewColor := FStudioConfig.ApplicationConfig.PreviewDisplayColor;
  FStudioConfig.ApplicationConfig.DoStartMinimized := ConfigForm.StartMinimizedCheckBox.Checked;
  FStudioConfig.ApplicationConfig.IsBrightnessControlledByList := ConfigForm.BrightListRadioButton.Checked;
  FStudioConfig.ApplicationConfig.DisplayBrightness := ConfigForm.BrightnessTrackBar.Position;
  FStudioConfig.AnimationConfig.PlayOnlyOnIdle := ConfigForm.OnlyOnIdleBox.Checked;
  FStudioConfig.AnimationConfig.IdlePercent := ConfigForm.IdleTrackBar.Position;
  FStudioConfig.ApplicationConfig.DoClearOnExit := ConfigForm.ClearOnCloseCheckBox.Checked;
  FStudioConfig.ApplicationConfig.Language := ConfigForm.Language;

  if (1 = ConfigForm.DspTypeCombo.ItemIndex) then
    FStudioConfig.DisplayConfig.DisplayType := 'NTK800'
  else
  if (2 = ConfigForm.DspTypeCombo.ItemIndex) then
    FStudioConfig.DisplayConfig.DisplayType := 'NTK300'
  else
  if (3 = ConfigForm.DspTypeCombo.ItemIndex) then
    FStudioConfig.DisplayConfig.DisplayType := 'U8G2'
  else
    FStudioConfig.DisplayConfig.DisplayType := '';
  FStudioConfig.DisplayConfig.IntName := ConfigForm.InterfaceCombo.Caption;
  FStudioConfig.DisplayConfig.Baudrate := StrToIntDef(ConfigForm.IfCfgCombo.Caption, 115200);
  FStudioConfig.DisplayConfig.ResX := ConfigForm.ResXSpinEdit.Value;
  FStudioConfig.DisplayConfig.ResY := ConfigForm.ResYSpinEdit.Value;

  if (FStudioConfig.ApplicationConfig.IconIndex <> ConfigForm.IconIndex) then
  begin
    FStudioConfig.ApplicationConfig.IconIndex := ConfigForm.IconIndex;
    SetApplicationIcon;
  end;

  SaveConfig(ExtractFilePath(Application.ExeName) + STUDIO_INIFILE);

end;

procedure TMainForm.SettingsAbortPressed;
begin
  // restore settings which might have been changed in ConfigForm
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
  FDisplayMgr.PreviewColor := FStudioConfig.ApplicationConfig.PreviewDisplayColor;
end;

procedure TMainForm.SettingsColorChange(AColor: TColor);
begin
  FDisplayMgr.PreviewColor := AColor;
end;

procedure TMainForm.SetApplicationIcon;
var
  FileName: String;
begin

  if (0 = FStudioConfig.ApplicationConfig.IconIndex) then
    FileName := 'VFDStudio2.ico'
  else
    FileName := IntToStr(FStudioConfig.ApplicationConfig.IconIndex) + '.ico';

  if (FileExists(FileName)) then
  begin
    Application.Icon.LoadFromFile(FileName);
    TrayIcon1.Icon.LoadFromFile(FileName);
  end;

end;

end.
