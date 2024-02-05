unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  NTK300, NTK800, VFDisplay, IniFiles, Menus, ExtCtrls, InfoUnit, uSMBIOS,
  SysInfo, WinampControl, LCLTranslator, ComCtrls, DateUtils, Math,
  StudioCommon, Glyphs, lclintf, RegExpr, PreviewDisplay;

const MAX_VARIABLE_INFO = 10;
const MAX_CLOCKS = 4;
const VERSION_STR = '2.0.0.0';

type

  TSearchDirection = (sdForward, sdBackward);

  TApplicationConfig = record
    Language: string;
    PreviewDisplayColor: TColor; // color of the pixels in the preview display
    DoStartMinimized: Boolean;   // start VFD-Studio2 minimized in system tray?
  end;

  TDisplayConfig = record
    DisplayType: string; // e.g. 'NTK800'
    ResX: Word;          // display resolution in x direction
    ResY: Word;          // display resolution in y direction
    IntName: string;     // interface name (e.g. 'COM5')
    Baudrate: Cardinal;  // baudrate for serial display connection
    IsBrightnessControlledByList: Boolean; // display brightness is controlled by list commands (otherwise by following setting)
    DisplayBrightness: Byte; // display brightness in percent, unless brightness is controlled by list commands
    DoClearOnExit: Boolean; // clear the display when closing the application (otherwise it is left as it is)
  end;

  TListConfig = record
    ListName: string;
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
    Text: string;
    SubsText: string;    // substituted content of the string; used to determine if it needs to be re-drawn
    X, Y: Byte;
    FontName: string;
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
    Text: string;          // goes bottom up: first char will be in lowest column
    MaxTextLen: Byte;      // maximum length of this drop
    Row: Integer;          // current row (where the first character is located); can be out of display area
    SlownessFactor: Byte;  // controls the speed of the drop; the higher, the slower
  end;

  TMatrixData = record
    CycleCounter: Int64;
    Drops: array of TMatrixDrop;
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

  TCpuUsageData = record
    CurrentCpuUsage: Byte;       // current CPU usage in percent; basically the youngest entry in the history
    AverageCpuUsage: Byte;       // average CPU usage in percent
    UsageHistory: array of Byte; // history of CPU usage
    NumRows: Integer;            // number of rows the usage graph shall use
    BottomRow: Integer;          // bottom-most row the usage graph shall use
    IsUsageMonitorDisplayed: Boolean; // is the CPU usage monitor currently displayed?
  end;

  TMemUsageData = record
    CurrentMemUsage: Byte;       // current memory usage in percent; basically the youngest entry in the history
    AverageMemUsage: Byte;       // average memory usage in percent
    PhysicalMemory: QWord;       // total physical memory size in bytes
    FreeMemory: QWord;           // free physical memory size in bytes
    UsageHistory: array of Byte; // history of memory usage
    NumRows: Integer;            // number of rows the usage graph shall use
    BottomRow: Integer;          // bottom-most row the usage graph shall use
    IsUsageMonitorDisplayed: Boolean; // is the memory usage monitor currently displayed?
  end;


  { TMainForm }
  TMainForm = class(TForm)
    AnimationsGroupBox: TGroupBox;
    ApplicationGroupBox: TGroupBox;
    BrightListRadioButton: TRadioButton;
    BrightnessControlLabel: TLabel;
    BrightnessPercentLabel: TLabel;
    BrightnessTrackBar: TTrackBar;
    BrightSettingsRadioButton: TRadioButton;
    ClearLogButton: TBitBtn;
    ClearOnCloseCheckBox: TCheckBox;
    ColorButton: TColorButton;
    DisplayGroupBox: TGroupBox;
    ExpertViewButton: TBitBtn;
    ExtListBox: TListBox;
    IdleLevelLabel: TLabel;
    IdlePercentageLabel: TLabel;
    IdleTrackBar: TTrackBar;
    LangDeButton: TBitBtn;
    LangEnButton: TBitBtn;
    LangItButton: TBitBtn;
    LanguageGroupBox: TGroupBox;
    OnlyOnIdleBox: TCheckBox;
    PreviewColorLabel: TLabel;
    SettingsGroupBox: TGroupBox;
    LogfileGroupBox: TGroupBox;
    ListBox: TListBox;
    ListGroupBox: TGroupBox;
    ImageList1: TImageList;
    HyperlinkLabel: TLabel;
    DevelopmentYearsLabel: TLabel;
    byLabel: TLabel;
    LogListView: TListView;
    MenuItem1: TMenuItem;
    SettingsScrollBox: TScrollBox;
    SpacerPanel: TPanel;
    SaveDialog: TSaveDialog;
    SaveLogButton: TBitBtn;
    StartMinimizedCheckBox: TCheckBox;
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
    Exit1: TMenuItem;
    ExtraTimer: TTimer;
    LoadListTimer: TTimer;
    InfoTimer: TTimer;
    Listeladen1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    NchsterScreen1: TMenuItem;
    OpenDialog: TOpenDialog;
    OkButtonPanel: TPanel;
    PopupMenu1: TPopupMenu;
    PopupStopButton: TMenuItem;
    ReloadButton: TBitBtn;
    ScreenTimeLabel: TLabel;
    Show1: TMenuItem;
    AnimateTimer: TTimer;
    StopButton: TBitBtn;
    TrayIcon1: TTrayIcon;
    UsageTimer: TTimer;
    WaitTimer: TTimer;
    procedure AnimateTimerTimer(Sender: TObject);
    procedure BrightListRadioButtonChange(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure ClearOnCloseCheckBoxChange(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
    procedure ColorButtonColorChanged(Sender: TObject);
    procedure IdleTrackBarChange(Sender: TObject);
    procedure LangItButtonClick(Sender: TObject);
    procedure ExpertViewButtonClick(Sender: TObject);
    procedure ClearLogButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColorPanelClick(Sender: TObject);
    procedure ColorShapeChangeBounds(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure ExtraTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure LanguageGroupBoxResize(Sender: TObject);
    procedure LoadListTimerTimer(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure InfoTimerTimer(Sender: TObject);
    procedure HyperlinkLabelClick(Sender: TObject);
    procedure LangDeButtonClick(Sender: TObject);
    procedure LangEnButtonClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure Listeladen1Click(Sender: TObject);
    procedure ListTestButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure NchsterScreen1Click(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OnlyOnIdleBoxChange(Sender: TObject);
    procedure PopupStopButtonClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure SaveLogButtonClick(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure StartMinimizedCheckBoxChange(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure UsageTimerTimer(Sender: TObject);
    procedure LoadList(ListFileName: TFileName);
    procedure WaitTimerStartTimer(Sender: TObject);
    procedure WaitTimerStopTimer(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);
    
    
    // methods related to processing lists
    procedure CreateScreen;
    function InterpreteListCommand(S: string): Integer;
    procedure StopProcessing;
    function SeekTo(StartIndex: Integer; SearchDirection: TSearchDirection; Cmd: string): Integer;
    procedure UpdateTimeLabel;
    
    // methods related to text output
    procedure ClearInfoStrings;
    function SubstituteStaticInfo(AText: string): string;
    function SubstituteVariableInfo(AText: string): string;
    function AddVariableInfo(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): Boolean;
    procedure HandleTextOutput(AText: string; X, Y: Byte; FontName: string; FontSize: Integer);
    procedure RefreshTextOutputs;
    function DrawFontedText(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): TPoint;
    
    // Bitmaps and animations
    procedure BitmapToVFD(FileName: string; X, Y: Word);
    procedure TrimBitmap(var Bmp: TBitmap);
    procedure Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, FrameWidth: Word);
    procedure DrawAnimationFrame(ABitmap: TBitmap; X, Y, Frame, FrameWidth: Word);
    procedure PauseAnimation;
    procedure StopAnimation;
    
    // methods related to the preview display

    // special display feature stuff
    procedure InitTheMatrix;
    procedure AddMatrixDrop(const AText: string; MaxTextLen, Row: Integer; SlownessFactor: Byte);
    procedure UpdateMatrixDrops;
    procedure UpdateCpuMonitor;
    procedure UpdateMemMonitor;
    procedure AddClock(Offset: Integer; X, Y, HourHandLength, MinuteHandLength, SecondsHandLength: Word);
    procedure DisableClocks;
    procedure RefreshClocks;
    procedure DrawDriveUsage(DriveLetter: Char; Col, Row, BarWidth: Integer);
    
    // settings
    procedure LoadConfig(const AFilePath: string);
    procedure SaveConfig(const AFilePath: string);
    
    // logging
    procedure LogEvent(const LogLevel: TLogLevel; const AText: string; const Timestamp: TDateTime);


  private
  protected

    // settings
    FStudioConfig: TStudioConfig;

    // display stuff
    FDisplay: TVFDisplay;     // display object; might be nil
    FPreviewDisplay: TPreviewDisplay; // preview display object

    // objects to gather system information from
    FSysInfo: TSysInfo;
    FSMBios: TSMBios;
    FWinampControl: TWinampControl;

    FListIndex: Integer;           // position in ListBox
    FScrollStringIndex: Word;      // used for scolling texts
    FRemainingSeconds: Integer;    // remaining number of seconds the current screen is shown
    FIsCurrentScreenShownOnce: Boolean; // is the currently displayed screen to be shown only once?

    FVariableInfo: Array[0..(MAX_VARIABLE_INFO - 1)] of TVariableInfo;  // holds textual information to be refreshed periodically
    FClocks: Array[0..(MAX_CLOCKS - 1)] of TClockData; // holds data of analog clocks
    FAnimationData: TAnimationData; // holds animation data
    FCpuUsageData: TCpuUsageData;   // hold information about current and average CPU load and a history of recent CPU usage
    FMemUsageData: TMemUsageData;   // hold information about current and average RAM load and a history of recent RAM usage
    FTheMatrix: TMatrixData; // holds the string drops used to display a Matrix like effect


  public
  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}

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

  { stop / play button }
  RsBtnStop = 'Stop';
  RsBtnGo = 'Go';

  { expert / normal mode view button }
  RsModeViewExpert = 'Expert view';
  RsModeViewNormal = 'Normal view';

  { Next screen announcement }
  RsNextScreenText = 'Next screen in';

  { Showing the list name }
  RsCurrentlyDisplayed = 'Currently displayed';

{ TMainForm }


// clear info strings
procedure TMainForm.ClearInfoStrings;
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

function TMainForm.SubstituteStaticInfo(AText: string): string;
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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].GetCurrentUsage, False);
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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].GetSlotType, False);
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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].SlotDesignationStr, False);
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
    Insert(VERSION_STR, S, I);
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


function TMainForm.SubstituteVariableInfo(AText: string): string;
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


function TMainForm.AddVariableInfo(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): Boolean;
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
procedure TMainForm.HandleTextOutput(AText: string; X, Y: Byte; FontName: string; FontSize: Integer);
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
      LogEvent(lvERROR, 'No more free slots to handle variable information. Text will be displayed as static text.', Now);

      if (FontName = '') or (FontSize = 0) then begin
        if (nil <> FDisplay) then
          FDisplay.PaintString(S, X, Y);
        FPreviewDisplay.PaintString(S, X, Y);
        FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
      end else begin
        DrawFontedText(S, X, Y, FontName, FontSize);
      end;
    end;

  end else begin
    // no (more) '$' in the string -> its just some staic text
    if (FontName = '') or (FontSize = 0) then begin
      if (nil <> FDisplay ) then
        FDisplay.PaintString(S, X, Y);
      FPreviewDisplay.PaintString(S, X, Y);
      FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
    end else begin
      DrawFontedText(S, X, Y, FontName, FontSize);
    end;
  end;

  RefreshTextOutputs;

end;


//Infos, die sich ständig ändern
procedure TMainForm.RefreshTextOutputs;
var
  I: Integer;
  OldText: string;
  S: string;
  TextWidth: Integer;
  Overlength: Integer;
  Subs: string;
  p0: Integer;
  BmpDimensions: TPoint;
begin

  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    S:= FVariableInfo[I].Text;

    if ('' = S) then
      Continue;

    OldText:= FVariableInfo[I].SubsText;
    S:= SubstituteVariableInfo(S);
    if (nil <> FDisplay) then
      TextWidth:= FDisplay.TextWidth // get number of characters one line can show
    else
      TextWidth:= FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP);

    TextWidth:= TextWidth - FVariableInfo[I].X;
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
          if (nil <> FDisplay) then
            FDisplay.PaintString(Subs, FVariableInfo[I].X, FVariableInfo[I].Y);
          FPreviewDisplay.PaintString(Subs, FVariableInfo[I].X, FVariableInfo[I].Y);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end else begin
          while (FVariableInfo[I].PrevWidth > Length(S)) do begin
           S:= S + ' '; // if the text was longer previously, we need to add whitespaces to clear the remainings of the previous text
          end;
          FVariableInfo[I].PrevWidth:= Length(S);
          if (nil <> FDisplay) then
            FDisplay.PaintString(S, FVariableInfo[I].X, FVariableInfo[I].Y);
          FPreviewDisplay.PaintString(S, FVariableInfo[I].X, FVariableInfo[I].Y);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
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


procedure TMainForm.LoadList(ListFileName: TFileName);
var
  I: Integer;
  S: string;
  FileName: string; // file name of other list
  J: Integer;
begin
  StopProcessing;
  FListIndex:= 0;

  ListGroupBox.Caption:= RsCurrentlyDisplayed + ': ' + ExtractFileName(ListFileName);

  //load list
  ListBox.Items.LoadFromFile(ListFileName);

  //insert external lists
  I:= 0;
  while (I < ListBox.Items.Count) do begin
    S:= ListBox.items[I];
    if (Pos('LOADSCREEN', S) = 1) then begin
      FileName:= Copy(S, 12, Length(S) - 11);
      ExtListBox.Items.LoadFromFile(FileName);
      ListBox.Items.Delete(I);
      // insert content from other list at position I
      for J:= (ExtListBox.Items.Count - 1) downto 0 do begin
        ListBox.Items.Insert(I, ExtListBox.Items[J]);
      end;
    end;
    Inc(I);
  end;

  // remove all comment lines and empty lines
  I:= 0;
  while (I<= ListBox.items.count - 1 ) do begin
    S:= ListBox.items[I];
    if (S='') or (S[1]=';') then begin
      ListBox.items.Delete(I);
      Dec(I);
    end;
    Inc(I);
  end; //while

  CreateScreen;
end;


procedure TMainForm.CreateScreen;
var
   S: string;
   I: Integer;
   StartIndex: Integer;
   LastIndex: Integer;
begin

  // seek forward to next 'NEWSCREEN'
  I:= SeekTo(FListIndex, sdForward, 'NEWSCREEN');

  if (-1 = I) then begin
    LogEvent(lvERROR, 'Could not find a NEWSCREEN command in the list.', Now)
  end else begin

    StartIndex:= I;
    FListIndex:= I;
    S:= '';

    // interprete the commands until SCREENEND is reached (or the end of the list, which should be a SCREENEND command anyway)
    while (not S.StartsWith('SCREENEND')) do begin
      LastIndex:= FListIndex;
      S:= ListBox.items[FListIndex];
      I:= InterpreteListCommand(S);
      if ((-1 <> I) and (I >= 0) and (I < ListBox.Items.Count)) then begin
        // InterpreteListCommand requested to jump to another position in ListBox.Items
        FListIndex:= I;
        StartIndex:= I;
      end else begin
        Inc(FListIndex);

        // check if the end of the list is reached
        if (FListIndex >= ListBox.Items.Count) then begin
          if (not S.StartsWith('SCREENEND')) then begin
            // this should not happen since the last command should have been a 'SCREENEND'
            LogEvent(lvERROR, 'List does not end with a SCREENEND command.', Now);
          end;
          FListIndex:= 0;
          Break;
        end;

      end;
    end; // end: while

    // mark processed lines
    ListBox.MultiSelect:= False;
    ListBox.MultiSelect:= True;
    ListBox.ClearSelection;
    for I:= StartIndex to LastIndex do
      ListBox.selected[I]:= True;

  end; // NEWSCREEN command found

end;

// Interpretes a command (from ListBox), given and parameter S.
// If the command requires jumping formward/backwards the list, the desired new
// list index is returned; otherwise -1.
// Reads FListIndex; does not modify FListIndex.
function TMainForm.InterpreteListCommand(S: string): Integer;
var
  CmdParts: TStringList;
  Cmd: string;
  n, ITmp: Integer;
  X, Y: Integer;
  P1, P2, P3, P4, P5, P6: Integer;
  IsInverted: Boolean;
  IsRequirementMet: Boolean;
  AColor: TColor;
  Res: Integer;
  DoContinueScreen: Boolean;
begin
  Res:= -1;

  CmdParts:= TStringList.Create;
  try
    CmdParts.Delimiter:= ' ';
    CmdParts.QuoteChar:= '''';
    CmdParts.DelimitedText:= S;

    if (CmdParts.Count > 0) then begin
      Cmd:= CmdParts[0];

      if ('NEWSCREEN' = Cmd) then begin

        FIsCurrentScreenShownOnce:= False; // by default this screen will be shown again then the list loops
        DoContinueScreen:= False;          // by default, NEWSCREEN will stop animations, clear/reset variable information, etc.

        if (CmdParts.Count >= 2) then begin
          // p1 = screen limitations
          if ('ONCE' = CmdParts[1]) then begin
            // display this screen only once
            FIsCurrentScreenShownOnce:= True;
          end else if ('CONTINUE' = CmdParts[1]) then begin
            // do not stop animations, do not clear/reset variable infomration, etc.
            DoContinueScreen:= True;
          end else if (CmdParts[1].StartsWith('REQUIRE')) then begin
            // this screen shall only be shown if a requirement is met
            IsRequirementMet:= False; // False by default, must be set to True in code below

            if ('REQUIREWINAMP' = CmdParts[1]) then begin
              // check if Winamp is running
              if (FWinampControl.IsWinampRunning) then
                IsRequirementMet:= True;
            end;

            if (not IsRequirementMet) then begin
              //LogEvent(lvINFO, 'Requirment ' + CmdParts[1] + ' is not met -> skip to next screen', Now);

              // seek next 'NEWSCREEN' line; starting with next line
              ITmp:= FListIndex + 1;
              if (ITmp >= ListBox.Items.Count) then
                ITmp:= 0;
              Res:= SeekTo(ITmp, sdForward, 'NEWSCREEN');

              if (-1 = Res) then
                LogEvent(lvERROR, 'Could not find the next screen in the list.', Now);

            end; // if requirement not met
          end; // if something required
        end; // if 2nd parameter with screen limitations given

        if (not DoContinueScreen) then begin
          StopProcessing;
        end;

        if (nil <> FDisplay) then begin
          FDisplay.ShowScreen(BOTH_LAYERS);
          FPreviewDisplay.LayerMode:= lmXOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
          if (not FStudioConfig.DisplayConfig.IsBrightnessControlledByList) then begin
            if (nil <> FDisplay) then
              FDisplay.SetBrightness(FStudioConfig.DisplayConfig.DisplayBrightness);
          end;
        end;


      end else if ('SCREENEND' = Cmd) then begin
        if (True = FIsCurrentScreenShownOnce) then begin
          // remove this screen from the list

          // go back to last 'NEWSCREEN' line
          ITmp:= SeekTo(FListIndex, sdBackward, 'NEWSCREEN');

          if ((-1 = ITmp) or (ITmp > FListIndex)) then
            LogEvent(lvERROR, 'Could not find start of current screen.', Now)
          else begin
            LogEvent(lvINFO, 'This screen was once. Clearing lines ' + IntToStr(ITmp) + '..' + IntToStr(FListIndex), Now);
            ListBox.MultiSelect:= True;
            ListBox.ClearSelection;
            for n:= ITmp to FListIndex do begin
              ListBox.selected[n]:= True;
            end;
            ListBox.DeleteSelected;
            FIsCurrentScreenShownOnce:= False;

            Res:= ITmp - 1;
            if (Res < 0) then
              Res:= ListBox.Items.Count - 1;
          end;
        end;

      end else if ('DSPINIT' = Cmd) then begin
        if (nil <> FDisplay) then
          FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);

      end else if ('ORMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('XORMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmXOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('ANDMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmAND;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('STOP' = Cmd) then begin
         WaitTimer.Enabled:= False;
         StopButton.caption:= RsBtnGo;
         Popupstopbutton.caption:= RsBtnGo;
         StopButton.ImageIndex:= 1;

      end else if ('CLEARSCREEN' = Cmd) then begin
        if (nil <> FDisplay) then
          FDisplay.ClearScreen;
        FPreviewDisplay.ClearScreen;
        FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);

      end else if ('SCREENTIME' = Cmd) then begin
        // p1 = screen time in seconds
        if (CmdParts.Count >= 2) then begin
          P1:= StrToInt(CmdParts[1]);
          WaitTimer.Interval:= P1 * 1000;
          //ScreenTimeLabel.caption:= 'ScreenTime: ' + floattostr(WaitTimer.Interval/1000)+ 'S';
          WaitTimer.Enabled:= True;
        end;

      end else if ('LIGHT' = Cmd) then begin
        // p1 = brightness level
        if (FStudioConfig.DisplayConfig.IsBrightnessControlledByList) then begin
          if (CmdParts.Count >= 2) then begin
            P1:= StrToInt(CmdParts[1]);
            if (nil <> FDisplay) then
              FDisplay.SetBrightness(P1);
          end;
        end;

      end else if ('NOISE' = Cmd) then begin
        // p1 = amount of pixels, p2 = inverted or not [optional]
         if (CmdParts.Count >= 2) then begin
           P1:= StrToInt(CmdParts[1]);
           if ((CmdParts.Count >= 3) and ((CmdParts[2].ToUpper = 'TRUE') or (CmdParts[2] = '1'))) then begin
             IsInverted:= True;
             AColor:= clWhite;
           end else begin
             IsInverted:= False;
             AColor:= clBlack;
           end;
           Randomize;
           for ITmp:= 1 to P1 do begin
             X:= Random(FStudioConfig.DisplayConfig.ResX);
             Y:= Random(FStudioConfig.DisplayConfig.ResY);
             if (nil <> FDisplay) then
               FDisplay.PaintPixel(X, Y, IsInverted);
             FPreviewDisplay.GraphicsLayer.Canvas.Pixels[X, Y]:= AColor;
           end;
           FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
         end;

      end else if ('PIXEL' = Cmd) then begin
        // p1 = x, p2 = y, p3 = inverted or not [optional]
        if (CmdParts.Count >= 3) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          if ((CmdParts.Count >= 4) and ((CmdParts[3].ToUpper = 'TRUE') or (CmdParts[3] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P1, P2]:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P1, P2]:= clBlack;
          end;
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('LINE' = Cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          end;
          FPreviewDisplay.GraphicsLayer.Canvas.Line(P1, P2, P3, P4);
          FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P3, P4]:= FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color;
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('FRAME' = Cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          end;
          FPreviewDisplay.GraphicsLayer.Canvas.Frame(P1, P2, P3 + 1, P4 + 1);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('PLAINTEXT' = Cmd) then begin
        // p1 = text, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$') ) then begin
            HandleTextOutput(CmdParts[1], P2, P3, '', 0);
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintString(CmdParts[1], P2, P3);
            FPreviewDisplay.PaintString(CmdParts[1], P2, P3);
            FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
          end;
        end;

      end else if ('TEXTOUT' = Cmd) then begin
        // p1 = text, p2 = x, p3 = y, p4 = font size, p5 = font name
        if (CmdParts.Count >= 6) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$') ) then begin
            HandleTextOutput(CmdParts[1], P2, P3, CmdParts[5], P4);
          end else begin
            DrawFontedText(CmdParts[1], P2, P3, CmdParts[5], P4);
          end;
        end;

      end else if ('ANIMATE' = Cmd) then begin
        // p1 = file name, p2 = animation speed, p3 = x, p4 = y, p5 = frame width
        if (False = FAnimationData.IsAnimationDisplayed) then begin
          // only one animation per screen is supported
          if (CmdParts.Count >= 6) then begin
            P2:= StrToInt(CmdParts[2]);
            P3:= StrToInt(CmdParts[3]);
            P4:= StrToInt(CmdParts[4]);
            P5:= StrToInt(CmdParts[5]);
            Animate(CmdParts[1], P2, P3, P4, P5);
            FAnimationData.IsAnimationDisplayed:= True;
          end;
        end;

      end else if ('BITMAP' = Cmd) then begin
        // p1 = file path, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          BitmapToVFD(CmdParts[1], P2, P3);
        end;

      end else if ('DRIVEUSAGE' = Cmd) then begin
        // p1 = drive letter, P2 = x, P3 = y, P4 = width (in characters)
        if (CmdParts.Count >= 5) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          DrawDriveUsage(CmdParts[1][1], P2, P3, P4);

        end;

      end else if ('CLOCK' = Cmd) then begin
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

      end else if ('THEMATRIX' = Cmd) then begin
        // p1 = speed
        if (CmdParts.Count >= 2) then begin
          P1:= StrToInt(CmdParts[1]);

          // Setting up the drops
          InitTheMatrix;
          ExtraTimer.Interval:= P1;
          ExtraTimer.Enabled:= True;

        end;

      end else if ('CPUMONITOR' = Cmd) then begin
        // p1 = number of rows to use, p2 = bottom-most row
          if (CmdParts.Count >= 3) then begin
            if (False = FCpuUsageData.IsUsageMonitorDisplayed) then begin
              P1:= StrToInt(CmdParts[1]);
              P2:= StrToInt(CmdParts[2]);
              FCpuUsageData.NumRows:= P1;
              FCpuUsageData.BottomRow:= P2;
              FCpuUsageData.IsUsageMonitorDisplayed:= True;
            end;
          end;

      end else if ('RAMMONITOR' = Cmd) then begin
        // p1 = number of rows to use, p2 = bottom-most row
          if (CmdParts.Count >= 3) then begin
            if (False = FMemUsageData.IsUsageMonitorDisplayed) then begin
              P1:= StrToInt(CmdParts[1]);
              P2:= StrToInt(CmdParts[2]);
              FMemUsageData.NumRows:= P1;
              FMemUsageData.BottomRow:= P2;
              FMemUsageData.IsUsageMonitorDisplayed:= True;
            end;
          end;

      end;


    end; // endif (CmdParts.Count > 0)

  finally
    CmdParts.Free;
  end;

  {
     if (S='NEWSCREEN') or (S='SCREENEND') then begin
      WaitTimer.Enabled:= False;
      ExtraTimer.Enabled:= False;
      ClearInfoStrings;
      DisableClocks;
  //    vfd.WriteCommand(CMD_Light_Lev1);  // volle Helligkeit
      vfd.stopClock;           // Uhr anhalten
      vfd.StopAnimation;       // Animation anhalten
      ScreenAnimated:= False;   // Voreinstellung: Screen enthält keine Animation (kann von ANIMATION - Befehl geändert werden);
      vfd.ClearBitmap;         // Bitmapzeichenfläche löschen
     end
     else if (Pos('SELECTSCREEN', S)= 1 ) then begin
      p1:= Copy(S, 14, 1); // Screennummer ermitteln
      vfd.SelectScreen(StrToInt(p1));
     end
     else if (Pos('CLEARSCREEN', S)= 1 ) then begin
      p1:= Copy(S, 13, 1); // Screennummer ermitteln
      vfd.ClearScreen(StrToInt(p1));
      if (p1='1' ) then vfd.ClearBitmap; //wenn GFX_Screen gelöscht wurde, auch Bitmapobjekt löschen
     end
     else if (Pos('FADEOUT', S)= 1 ) then begin
      p1:= Copy(S, 9, l - 8); // Geschwindigkeit ermitteln
      vfd.Fade(True, StrToInt(p1));
     end
     else if (Pos('FADEIN', S)= 1 ) then begin
      p1:= Copy(S, 8, l - 7); // Geschwindigkeit ermitteln
      vfd.Fade(False, StrToInt(p1));
     end
     else if (Pos('CPUMONITOR', S)= 1 ) then begin
      CPUMONITORenabled:= True;
      showCPUMONITOR;
     end
     else if (Pos('CPUMONCHAR', S)= 1 ) then begin
      CPUMONITORcharenabled:= True;
      showCPUMONITORchar;
     end
     else if (Pos('RAMMONITOR', S)= 1 ) then begin
      RAMMONITORenabled:= True;
      showRAMMONITOR;
     end
     else if (Pos('RAMMONCHAR', S)= 1 ) then begin
      RAMMONITORcharenabled:= True;
      showRAMMONITORchar;
     end
     else if (Pos('PRINTERS', S)= 1 ) then begin
      showPrinterScreen;
     end
     else if (Pos('DRIVES', S)= 1 ) then begin
      showDrivesScreen;
     end
     else if (Pos('STRETCHBITMAP', S)= 1 ) then begin
      p1:= Copy(S, 15, l - 14);           //Dateiname
      gfx:= TBitmap.Create;
      gfx.Width:= 256;
      gfx.Height:= 64;
      gfx.LoadFromFile(p1);
      vfd.bitmap.Canvas.StretchDraw(rect(0, 0, 256, 64), GFX);
      gfx.free;
     end
     else if (Pos('ANIMATE', S)= 1 ) then begin
       p3:= Copy(S, l - 3, 4);  //Speed in ms ermitteln
       p2:= Copy(S, l - 7, 3);  //X - Position
       p1:= Copy(S, 9, l - 17); //Filename
       vfd.Animate(p1, StrToInt(p2), StrToInt(p3));
      if (not PlayOnlyOnIdle) or isIdle then begin  //nur wenn isIdle oder PlayOnlyOnIdle abgeschaltet - andernfalls Befehl ignorieren
       AnimationPaused:= False;
      end else begin
       vfd.PauseAnimation;   //Animation anhalten
       AnimationPaused:= True;
      end;
      ScreenAnimated:= True;           // Screen enthält eine Animation (relevant für Timer)
     end
     else if (Pos('PIXELBYTE', S)= 1 ) then begin
      p3:= Copy(S, l, 1);  //Letztes Zeichen= Y - Position (einstellig[0..7])
      p2:= Copy(S, l - 4, 3);  //X - Position (dreistellig)
      p1:= Copy(S, l - 8, 3); //Pixelbyte (dreistellig)
      vfd.SetPixelbyte(StrToInt(p1), StrToInt(p2), StrToInt(p3));
     end
     else if (Pos('TIME', S)= 1 ) then begin
      p2:= Copy(S, l - 3, 4);  //Position
      p1:= Copy(S, 6, l - 9); //Analog oder Digital?
      if (p1= 'ANALOG' ) then vfd.Time(True, StrToInt(p2))
      else vfd.time(False, StrToInt(p2));
     end
     else if (Pos('PAINTBITMAP', S)= 1 ) then begin
        p6:= Copy(S, 13, l - 11);  //'PAINTBITMAP ' aus S entfernen
        p5:= p6;
      p1:= Copy(p6, 0, (Pos('?', p5)) - 1 );     // FileName    (? darf in Dateinamen nicht vorkommen)
      p2:= Copy(S, 15 + Length(p1), 3);          // XDisplay
      p3:= Copy(S, 19 + Length(p1), 1);          // YDisplay  (nur einstellig)
      p4:= Copy(S, 21 + Length(p1), 3);          // Breite in Pixeln
      p5:= Copy(S, 25 + Length(p1), 3);          // X - Position im Bild
      p6:= Copy(S, 29 + Length(p1), l - 28 + Length(p1)); //MalFarbe
      BitmapToVFD(p1, StrToInt(p2), StrToInt(p3), StrToInt(p4), StrToInt(p5), stringtocolor(p6));
     end;
}
  Result:= Res;
end;


procedure TMainForm.WaitTimerStartTimer(Sender: TObject);
begin
  FRemainingSeconds:= WaitTimer.Interval div 1000;
  UpdateTimeLabel;
end;

procedure TMainForm.WaitTimerStopTimer(Sender: TObject);
begin
  UpdateTimeLabel;
end;


procedure TMainForm.WaitTimerTimer(Sender: TObject);
begin
  WaitTimer.Enabled:= False;

  CreateScreen;
end;


procedure TMainForm.LoadConfig(const AFilePath: string);
var
  IniFile: TIniFile;
  ColorString: string;
begin
  try
    IniFile:= TIniFile.Create(AFilePath);

    { application section }
    FStudioConfig.ApplicationConfig.Language:= IniFile.ReadString('APPLICATION', 'Language', 'en');
    ColorString:= IniFile.ReadString('APPLICATION', 'DspColor', '$FFFFFF');
    ColorString:= ColorString.Replace('#', '$');
    FStudioConfig.ApplicationConfig.PreviewDisplayColor:= StringToColor(ColorString);
    FStudioConfig.ApplicationConfig.DoStartMinimized:= IniFile.ReadBool('APPLICATION', 'StartMinimized', False);

    { display section }
    FStudioConfig.DisplayConfig.DisplayType:=   IniFile.ReadString( 'DISPLAY', 'Type',        '');
    FStudioConfig.DisplayConfig.ResX:=          IniFile.ReadInteger('DISPLAY', 'ResX',        128);
    FStudioConfig.DisplayConfig.ResY:=          IniFile.ReadInteger('DISPLAY', 'ResY',        64);
    FStudioConfig.DisplayConfig.IntName:=       IniFile.ReadString( 'DISPLAY', 'Interface',   'COM1');
    FStudioConfig.DisplayConfig.Baudrate:=      IniFile.ReadInteger('DISPLAY', 'Baud',        115200);
    FStudioConfig.DisplayConfig.DoClearOnExit:= IniFile.ReadBool(   'DISPLAY', 'ClearOnExit', False);
    FStudioConfig.DisplayConfig.IsBrightnessControlledByList:= IniFile.ReadBool('DISPLAY', 'BrightnessByList', True);
    FStudioConfig.DisplayConfig.DisplayBrightness:= Min(100, IniFile.ReadInteger('DISPLAY', 'Brightness', 100));

    { list section }
    FStudioConfig.ListConfig.ListName:= IniFile.ReadString('LIST', 'Listname', 'Default.lst');

    { animations section }
    FStudioConfig.AnimationConfig.PlayOnlyOnIdle:= IniFile.ReadBool(   'ANIMATIONS', 'PlayOnlyOnIdle', True);
    FStudioConfig.AnimationConfig.IdlePercent:=    Min(100, IniFile.ReadInteger('ANIMATIONS', 'IdleLevel', 50));

  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.SaveConfig(const AFilePath: string);
var
  IniFile: TIniFile;
  ColorString: string;
begin
  try
    IniFile:= TIniFile.Create(AFilePath);

    { application section }
    IniFile.WriteString('APPLICATION', 'Language', FStudioConfig.ApplicationConfig.Language);
    ColorString:= Format('$%.6x', [FStudioConfig.ApplicationConfig.PreviewDisplayColor]);
    IniFile.WriteString('APPLICATION', 'DspColor', ColorString);
    IniFile.WriteBool('APPLICATION', 'StartMinimized', FStudioConfig.ApplicationConfig.DoStartMinimized);

    { display section }
    IniFile.WriteString( 'DISPLAY', 'Type',          FStudioConfig.DisplayConfig.DisplayType);
    IniFile.WriteInteger('DISPLAY', 'ResX',          FStudioConfig.DisplayConfig.ResX);
    IniFile.WriteInteger('DISPLAY', 'ResY',          FStudioConfig.DisplayConfig.ResY);
    IniFile.WriteString( 'DISPLAY', 'Interface',     FStudioConfig.DisplayConfig.IntName);
    IniFile.WriteInteger('DISPLAY', 'Baud',          FStudioConfig.DisplayConfig.Baudrate);
    IniFile.WriteBool('DISPLAY', 'ClearOnExit',      FStudioConfig.DisplayConfig.DoClearOnExit);
    IniFile.WriteBool('DISPLAY', 'BrightnessByList', FStudioConfig.DisplayConfig.IsBrightnessControlledByList);
    IniFile.WriteInteger('DISPLAY', 'Brightness',    FStudioConfig.DisplayConfig.DisplayBrightness);

    { list section }
    IniFile.WriteString('LIST', 'Listname', FStudioConfig.ListConfig.ListName);

    { animations section }
    IniFile.WriteBool(   'ANIMATIONS', 'PlayOnlyOnIdle', FStudioConfig.AnimationConfig.PlayOnlyOnIdle);
    IniFile.WriteInteger('ANIMATIONS', 'IdleLevel',      FStudioConfig.AnimationConfig.IdlePercent);

  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFilePath: string;
begin
  Randomize;
  FSMBios:= TSMBios.Create;


  FPreviewDisplay:= TPreviewDisplay.Create(Self);
  FPreviewDisplay.LayerMode:= lmXOR;

  LogEvent(lvINFO, 'Application started. Version ' + VERSION_STR , Now);

  IniFilePath:= ExtractFilePath(application.ExeName) + 'vfdstudio.ini';
  LoadConfig(IniFilePath);

  // set controls accordingly to loaded settings
  if (FStudioConfig.ApplicationConfig.DoStartMinimized) then
    Hide;
  ColorButton.ButtonColor:= FStudioConfig.ApplicationConfig.PreviewDisplayColor;
  FPreviewDisplay.DisplayColor:= FStudioConfig.ApplicationConfig.PreviewDisplayColor;;
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
  StartMinimizedCheckBox.Checked:= FStudioConfig.ApplicationConfig.DoStartMinimized;
  BrightListRadioButton.Checked:= FStudioConfig.DisplayConfig.IsBrightnessControlledByList;
  BrightSettingsRadioButton.Checked:= not FStudioConfig.DisplayConfig.IsBrightnessControlledByList;
  BrightnessTrackBar.Position:= FStudioConfig.DisplayConfig.DisplayBrightness;
  BrightnessPercentLabel.Caption:= IntToStr(FStudioConfig.DisplayConfig.DisplayBrightness) + '%';
  OnlyOnIdleBox.Checked:= FStudioConfig.AnimationConfig.PlayOnlyOnIdle;
  IdleTrackBar.Position:= FStudioConfig.AnimationConfig.IdlePercent;
  IdlePercentageLabel.Caption:= IntToStr(FStudioConfig.AnimationConfig.IdlePercent) + '%';
  ClearOnCloseCheckBox.Checked:= FStudioConfig.DisplayConfig.DoClearOnExit;


  if ('NTK800' = FStudioConfig.DisplayConfig.DisplayType) then begin
    FDisplay:= TNTK800.Create(Self);
  end else if ('NTK300' = FStudioConfig.DisplayConfig.DisplayType) then begin
    FDisplay:= TNTK300.Create(Self);
  end;

  if (nil <> FDisplay) then begin
    //VirtualDisplayGraphicsLayer.Width:= FStudioConfig.DisplayConfig.ResX;
    //VirtualDisplayGraphicsLayer.Height:= FStudioConfig.DisplayConfig.ResY;
    FDisplay.OnDbgMessage:= @LogEvent;
    FDisplay.Connect(FStudioConfig.DisplayConfig.IntName);
    FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);
  end;
  PreviewImage.Width:= FStudioConfig.DisplayConfig.ResX * 2;
  PreviewImage.Height:= FStudioConfig.DisplayConfig.ResY * 2;
  PreviewImage.Picture.Bitmap.Width:= FStudioConfig.DisplayConfig.ResX;
  PreviewImage.Picture.Bitmap.Height:= FStudioConfig.DisplayConfig.ResY;
  FPreviewDisplay.SetSize(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);

  FSysInfo:= TSysInfo.Create(Self);
  FWinampControl:= TWinampControl.Create(Self);

  FAnimationData.AnimationBitmap:= TBitmap.create;

  TrayIcon1.Hint:= 'VFD-Studio 2: ' + FStudioConfig.DisplayConfig.DisplayType + '@' + FStudioConfig.DisplayConfig.IntName;

  VersionLabel.Caption:= 'v' + VERSION_STR;

  // Starting the application might require quite some (CPU)time, so it's a good
  // idea to give the system some time. That's why loading the list is moved
  // to a timer which will add a small delay to this and which will disable
  // itself afterwards.
  LoadListTimer.Enabled:= True;

end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  IniFilePath: string;
  AStringList: TStringList;
  I: Integer;
begin

  if (FStudioConfig.DisplayConfig.DoClearOnExit) then begin
    if (nil <> FDisplay) then
      FDisplay.ClearScreen;
    // we might also clear the preview display, but since the application is about to close we skip that
  end;

  LogEvent(lvINFO, 'Application closed.', Now);

  IniFilePath:= ExtractFilePath(application.ExeName) + 'vfdstudio.ini';
  SaveConfig(IniFilePath);

  AStringList:= TStringList.Create;
  try
    AStringList.Add('#;Message;Level;Time');
    for I:= 0 to (LogListView.Items.Count - 1) do
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

  CanClose:= True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSMBios.Free;
  FSysInfo.Free;
  FWinampControl.Free;
  FDisplay.Free;
  FAnimationData.AnimationBitmap.Free;
  SetLength(FTheMatrix.Drops, 0);
  SetLength(FCpuUsageData.UsageHistory, 0);
  SetLength(FMemUsageData.UsageHistory, 0);
  FPreviewDisplay.Free;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (wsMinimized = WindowState) then
    Hide;
end;

procedure TMainForm.LanguageGroupBoxResize(Sender: TObject);
begin
  LanguageGroupBox.ChildSizing.ControlsPerLine:= LanguageGroupBox.Width div (LangDeButton.Width + LanguageGroupBox.ChildSizing.HorizontalSpacing);
end;

procedure TMainForm.LoadListTimerTimer(Sender: TObject);
begin
  LoadListTimer.Enabled:= False; // self-disabling

  // load last list as specified in ini file
  Mainform.LoadList(ExtractFilePath(Application.ExeName) + 'Lists\' + FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.InfoButtonClick(Sender: TObject);
begin
  InfoForm.Show;
end;

procedure TMainForm.UpdateTimeLabel;
var
  H, M, S: Integer;
begin
  if (WaitTimer.Enabled) then begin
    H:= Trunc(FRemainingSeconds / 3600);
    M:= Trunc((FRemainingSeconds mod 3600) / 60);
    S:= FRemainingSeconds mod 60;
    ScreenTimeLabel.caption:= RsNextScreenText + Format(' %d:%.02d:%.02d', [H, M, S]);
  end else begin
    ScreenTimeLabel.caption:= RsNextScreenText + ' -: - -: - -';
  end;
end;

procedure TMainForm.InfoTimerTimer(Sender: TObject);
var
  I: Integer;
  IsVariableInfo: Boolean;
begin

  UpdateTimeLabel;
  if (FRemainingSeconds > 0) then
    Dec(FRemainingSeconds);

  // check if there are any varaiable infos
  IsVariableInfo:= False;
  for I:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    if (FVariableInfo[I].Text <> '') then begin
      IsVariableInfo:= True;
      Break;
    end;
  end;
  if (IsVariableInfo) then
    RefreshTextOutputs;

  RefreshClocks;

end;

procedure TMainForm.HyperlinkLabelClick(Sender: TObject);
begin
  OpenURL('http://cypax.net');
end;

procedure TMainForm.LangEnButtonClick(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'en';
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
end;

procedure TMainForm.LangDeButtonClick(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'de';
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
end;

procedure TMainForm.LangItButtonClick(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'it';
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
end;


// User whishes to jump to a specific line
procedure TMainForm.ListBoxDblClick(Sender: TObject);
var
  I: Integer;
begin
  // seek to the corresponding NEWSCREEN instruction
  I:= SeekTo(ListBox.ItemIndex, sdBackward, 'NEWSCREEN');
  if (-1 <> I) then begin
    StopProcessing;
    FListIndex:= I;
    CreateScreen;
  end;
end;

procedure TMainForm.Listeladen1Click(Sender: TObject);
begin
  ListTestButtonClick(Self);
end;

// Halts all timers related to display output and clears all animation,
// information and clock data
procedure TMainForm.StopProcessing;
begin
  WaitTimer.Enabled:= False;
  ExtraTimer.Enabled:= False;
  StopAnimation; // this also disables the animation timer
  DisableClocks;
  ClearInfoStrings;
  FCpuUsageData.IsUsageMonitorDisplayed:= False;
  FMemUsageData.IsUsageMonitorDisplayed:= False;
  StopButton.Caption:= RsBtnStop;
  PopupStopButton.Caption:= RsBtnStop;
end;

procedure TMainForm.ListTestButtonClick(Sender: TObject);
begin
  OpenDialog.InitialDir:= extractFileDir(application.exename) + '\Lists';
  if (OpenDialog.Execute) then begin
    LoadList(OpenDialog.FileName);
    WaitTimer.Enabled:= True;
    FStudioConfig.ListConfig.ListName:= ExtractFileName(OpenDialog.FileName);
  end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  ReloadButtonClick(Self);
end;

procedure TMainForm.NchsterScreen1Click(Sender: TObject);
begin
  NextButtonClick(Sender);
end;

procedure TMainForm.NextButtonClick(Sender: TObject);
var
   LI: Integer;
begin
  StopProcessing;

  LI:= SeekTo(FListIndex, sdForward, 'NEWSCREEN');

  if (-1 = LI) then
    LogEvent(lvERROR, 'Could not find next NEWSCREEN command.', Now)
  else begin
    FListIndex:= LI;
    CreateScreen;
  end;
end;

procedure TMainForm.OKButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TMainForm.OnlyOnIdleBoxChange(Sender: TObject);
begin
  FStudioConfig.AnimationConfig.PlayOnlyOnIdle:= OnlyOnIdleBox.Checked;
end;

procedure TMainForm.PopupStopButtonClick(Sender: TObject);
begin
  StopButtonClick(Sender);
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  Mainform.LoadList(ExtractFilePath(application.ExeName) + 'Lists\' + FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.SaveLogButtonClick(Sender: TObject);
var
  AStringList: TStringList;
  I: Integer;
begin
  if (SaveDialog.Execute) then begin

    AStringList:= TStringList.Create;
    try
      AStringList.Add('#;Message;Level;Time');
      for I:= 0 to (LogListView.Items.Count - 1) do
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

procedure TMainForm.Show1Click(Sender: TObject);
begin
  WindowState:= wsNormal;
  Show;
end;

procedure TMainForm.StartMinimizedCheckBoxChange(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.DoStartMinimized:= StartMinimizedCheckBox.Checked;
end;

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  WaitTimer.Enabled:= not WaitTimer.Enabled;
  if (True = Waittimer.Enabled) then begin
    StopButton.caption:= RsBtnStop;
    StopButton.ImageIndex:= 0;
    Popupstopbutton.caption:= RsBtnStop;
    PopupStopButton.ImageIndex:= 0;
  end
  else begin
    StopButton.caption:= RsBtnGo;
    StopButton.ImageIndex:= 1;
    PopupStopButton.caption:= RsBtnGo;
    PopupStopButton.ImageIndex:= 1;
  end;
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure TMainForm.UsageTimerTimer(Sender: TObject);
var
  CpuUsage: Byte;
  I: Integer;
  AvgValue: Single;
  TextWidth: Integer;
  PhysMem, FreeMem: QWord;
  MemUsage: DWord;
begin
  if (nil <> FSysInfo) then begin

    if (nil <> FDisplay) then
      TextWidth:= FDisplay.TextWidth // get number of characters one line can show
    else
      TextWidth:= FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP);

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

    if (FCpuUsageData.IsUsageMonitorDisplayed) then begin
      UpdateCpuMonitor;
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

    if (FMemUsageData.IsUsageMonitorDisplayed) then begin
      UpdateMemMonitor;
    end;

  end; // FSysInfo not nil
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  MainForm.close;
end;

procedure TMainForm.ExtraTimerTimer(Sender: TObject);

begin

  if (Length(FTheMatrix.Drops) > 0) then begin
    UpdateMatrixDrops;
    Inc(FTheMatrix.CycleCounter);
  end;

end;



procedure TMainForm.Exit1Click(Sender: TObject);
begin
  MainForm.close;
end;

procedure TMainForm.BitmapToVFD(FileName: string; X, Y: Word);
var
  TmpBitmap: TBitmap;
begin
  TmpBitmap:= TBitmap.Create;
  TmpBitmap.LoadFromFile(FileName);
  //LogEvent(lvINFO, 'Loading bitmap. Size ' + IntToStr(TmpBitmap.Width) + 'x' + IntToStr(TmpBitmap.Height), Now);

  // clip bitmap to display if needed
  if (TmpBitmap.Height + Y >= FStudioConfig.DisplayConfig.ResY) then
    TmpBitmap.Height:= TmpBitmap.Height - (TmpBitmap.Height + Y - FStudioConfig.DisplayConfig.ResY);
  if (TmpBitmap.Width + X >= FStudioConfig.DisplayConfig.ResX) then
    TmpBitmap.Width:= TmpBitmap.Width - (TmpBitmap.Width + X - FStudioConfig.DisplayConfig.ResX);

  if (nil <> FDisplay) then
    FDisplay.PaintBitmap(TmpBitmap, X, Y);
  FPreviewDisplay.GraphicsLayer.Canvas.Draw(X, Y, TmpBitmap);
  FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
  TmpBitmap.Free;
end;

procedure TMainForm.TrimBitmap(var Bmp: TBitmap);
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

function TMainForm.DrawFontedText(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): TPoint;
var
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
    if (TmpBitmap.Height + Y >= FStudioConfig.DisplayConfig.ResY) then
      TmpBitmap.Height:= TmpBitmap.Height - (TmpBitmap.Height + Y - FStudioConfig.DisplayConfig.ResY);
    if (TmpBitmap.Width + X >= FStudioConfig.DisplayConfig.ResX) then
      TmpBitmap.Width:= TmpBitmap.Width - (TmpBitmap.Width + X - FStudioConfig.DisplayConfig.ResX);

    if (nil <> FDisplay) then
      FDisplay.PaintBitmap(TmpBitmap, X, Y);
    FPreviewDisplay.GraphicsLayer.Canvas.Draw(X, Y, TmpBitmap);
    FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
    TmpBitmap.Canvas.Pixels[0, 0]:= TmpBitmap.Canvas.Pixels[0, 0]; // this seems like nonsense but is required to actually load the bitmap in memory
    ResultPoint.X:= TmpBitmap.Width;
    ResultPoint.Y:= TmpBitmap.Height;
  finally
    TmpBitmap.Free;
  end;

  Result:= ResultPoint;
end;



procedure TMainForm.Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, FrameWidth: Word);
begin
  try
    AnimateTimer.Enabled:= False;

    FAnimationData.AnimationBitmap.LoadFromFile(FileName);

    FAnimationData.FrameIndex:=  0;
    FAnimationData.FrameCount:= (FAnimationData.AnimationBitmap.Width div FrameWidth);
    FAnimationData.FrameWidth:=  FrameWidth;
    FAnimationData.XPos:=        XPosition;
    FAnimationData.YPos:=        YPosition;
    AnimateTimer.Interval:=      AnimationSpeed;
    AnimateTimer.Enabled:=       True;
  except
    on E: Exception do begin
      AnimateTimer.Enabled:= False;
      LogEvent(lvERROR, 'Could not load animation from file ' + FileName, Now);
    end;
  end;

end;

procedure TMainForm.PauseAnimation;
begin
  if (nil <> FAnimationData.AnimationBitmap) then
    AnimateTimer.Enabled:= not AnimateTimer.Enabled;    //nil in FAnimationBitmap würde bei bei Animation zu Exception führen
end;

procedure TMainForm.StopAnimation;
begin
  AnimateTimer.Enabled:= False;
  FAnimationData.FrameIndex:= 0;
  FAnimationData.IsAnimationDisplayed:= False;
end;

procedure TMainForm.AnimateTimerTimer(Sender: TObject);
begin
  if (not FStudioConfig.AnimationConfig.PlayOnlyOnIdle) or (FStudioConfig.AnimationConfig.IdlePercent > FCpuUsageData.AverageCpuUsage) then begin

    DrawAnimationFrame(FAnimationData.AnimationBitmap, FAnimationData.XPos, FAnimationData.YPos, FAnimationData.FrameIndex, FAnimationData.FrameWidth);

    Inc(FAnimationData.FrameIndex);
    if (FAnimationData.FrameIndex >= FAnimationData.FrameCount) then
      FAnimationData.FrameIndex:= 0;

  end;
end;

procedure TMainForm.BrightListRadioButtonChange(Sender: TObject);
begin
  FStudioConfig.DisplayConfig.IsBrightnessControlledByList:= BrightListRadioButton.Checked;
end;

procedure TMainForm.BrightnessTrackBarChange(Sender: TObject);
begin
  BrightnessPercentLabel.Caption:= IntToStr(BrightnessTrackBar.Position) + '%';
  FStudioConfig.DisplayConfig.DisplayBrightness:= BrightnessTrackBar.Position;
end;

procedure TMainForm.ClearOnCloseCheckBoxChange(Sender: TObject);
begin
  FStudioConfig.DisplayConfig.DoClearOnExit:= ClearOnCloseCheckBox.Checked;
end;

procedure TMainForm.ColorButtonClick(Sender: TObject);
begin
end;

procedure TMainForm.ColorButtonColorChanged(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.PreviewDisplayColor:= ColorButton.ButtonColor;
  FPreviewDisplay.DisplayColor:= ColorButton.ButtonColor;
end;

procedure TMainForm.IdleTrackBarChange(Sender: TObject);
begin
  IdlePercentageLabel.Caption:= IntTosTr(IdleTrackBar.Position) + '%';
  FStudioConfig.AnimationConfig.IdlePercent:= IdleTrackBar.Position;
end;

procedure TMainForm.ExpertViewButtonClick(Sender: TObject);
begin
  LogfileGroupBox.Visible:= not LogfileGroupBox.Visible;
  SettingsScrollBox.Visible:= LogfileGroupBox.Visible;
  ListGroupBox.Visible:= LogfileGroupBox.Visible;

  if (LogfileGroupBox.Visible) then begin
    ExpertViewButton.Caption:= RsModeViewNormal;
    ExpertViewButton.ImageIndex:= 2;
  end else begin
    ExpertViewButton.Caption:= RsModeViewExpert;
    ExpertViewButton.ImageIndex:= 3;
  end;

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
    TmpBitmap:= TBitmap.Create;
    TmpBitmap.Height:= ABitmap.Height;
    TmpBitmap.Width:= FrameWidth;
    sRect:= Rect(Frame * FrameWidth, 0, Frame * FrameWidth + FrameWidth, ABitmap.Height);
    dRect:= Rect(0, 0, FrameWidth, ABitmap.Height);
    TmpBitmap.Canvas.CopyRect(dRect, ABitmap.Canvas, sRect);
    if (nil <> FDisplay) then
      FDisplay.PaintBitmap(TmpBitmap, X, Y);
    FPreviewDisplay.GraphicsLayer.Canvas.Draw(X, Y, TmpBitmap);
    FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
    TmpBitmap.Canvas.Pixels[0, 0]:= TmpBitmap.Canvas.Pixels[0, 0]; // this seems like nonsense but is required to actually load the bitmap in memory
  finally
    TmpBitmap.Free;
  end;
end;

procedure TMainForm.LogEvent(const LogLevel: TLogLevel; const AText: string; const Timestamp: TDateTime);
var
  Item: TListItem;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(Timestamp, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);

  Item:= LogListView.Items.Add;
  Item.Caption:= IntToStr(LogListView.Items.Count);
  Item.Subitems.Add(AText);
  case LogLevel of
   lvINFO:    Item.Subitems.Add('INFO');
   lvWARNING: Item.Subitems.Add('WARNING');
   lvERROR:   Item.Subitems.Add('ERROR');
   else       Item.Subitems.Add('CRITICAL');
  end;
  Item.Subitems.Add(Format('%.04d-%.02d-%.02d %.02d:%.02d:%.02d.%.03d', [AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond]));
end;

// Starting at StartIndex this function seeks forward/backward to the next line in ListBox which starts with the questioned string.
// String comparison is not cases-sensitive.
// Will continue on start/end of list.
// Returns the index of the line or -1 if it could not be found.
// Returns -1 if StartIndex is out of bounds or the requested string is empty.
function TMainForm.SeekTo(StartIndex: Integer; SearchDirection: TSearchDirection; Cmd: string): Integer;
var
  I: Integer;
  S: string;
  Res: Integer;
begin

  if (('' = Cmd) or (StartIndex >= ListBox.Items.Count) or (StartIndex < 0)) then begin
    Result:= -1;
  end else begin
    I:= StartIndex;
    Res:= -1;

    repeat
      S:= ListBox.Items[I];
      if (S.ToUpper.StartsWith(Cmd.ToUpper)) then begin
        Res:= I;
        Break;
      end;

      if (sdForward = SearchDirection) then begin
        Inc(I);
        if (I >= ListBox.Items.Count) then
          I:= 0;
      end else begin
        Dec(I);
        if (I < 0) then
          I:= ListBox.Items.Count - 1;
      end;

      // have we reached the point where we started from?
      if (StartIndex = I) then
        Break;
    until (-1 <> Res);

    Result:= Res;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
end;

procedure TMainForm.ColorPanelClick(Sender: TObject);
begin
end;

procedure TMainForm.ColorShapeChangeBounds(Sender: TObject);
begin

end;


procedure TMainForm.AddClock(Offset: Integer; X, Y, HourHandLength, MinuteHandLength, SecondsHandLength: Word);
var
  I: Integer;
  IsFreeSlotFound: Boolean;
begin
  IsFreeSlotFound:= False;

  // add clock to next free slot
  for I:= 0 to (MAX_CLOCKS - 1) do begin
    if (not FClocks[I].IsActive) then begin
      FClocks[I].Offset:= Offset;
      FClocks[I].X:= X;
      FClocks[I].Y:= Y;
      FClocks[I].HourHandLength:= HourHandLength;
      FClocks[I].MinuteHandLength:= MinuteHandLength;
      FClocks[I].SecondsHandLength:= SecondsHandLength;
      FClocks[I].IsActive:= True;
      IsFreeSlotFound:= True;
      Break;
    end;
  end;

  if (not IsFreeSlotFound) then begin
    LogEvent(lvERROR, 'Not more than ' + IntToStr(MAX_CLOCKS) + ' clocks can be shown at the same time.', Now);
  end;
end;

procedure TMainForm.DisableClocks;
var
  I: Integer;
begin
  for I:= 0 to (MAX_CLOCKS - 1) do begin
    FClocks[I].IsActive:= False;
    FClocks[I].HourPoint:= Point(-1, -1);
    FClocks[I].MinutePoint:= Point(-1, -1);
    FClocks[I].SecondsPoint:= Point(-1, -1);
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
  CurrentDateTime:= Now;

  for I:= 0 to (MAX_CLOCKS - 1) do begin
    if (FClocks[I].IsActive) then begin
      ClockDateTime:= IncMinute(CurrentDateTime, FClocks[I].Offset);
      DecodeTime(ClockDateTime, HH, MM, SS, MS);

      X0:= FClocks[I].X;
      Y0:= FClocks[I].Y;

      // draw seconds hand
      if (FClocks[I].SecondsHandLength > 0) then begin

        // calculate X1, Y1
        // convert seconds to angle (in radians)
        Angle:= DegToRad((SS mod 60) * 6); // 6 * 60 = 360°

        // calculate X1, Y1 coordinates
        X1:= Round(X0 + FClocks[I].SecondsHandLength * Sin(Angle));
        Y1:= Round(Y0 - FClocks[I].SecondsHandLength * Cos(Angle));


        // do something in case we have new coordinates
        if ((X1 <> FClocks[I].SecondsPoint.X) or (Y1 <> FClocks[I].SecondsPoint.Y)) then begin
          // clear previous hand
          if (FClocks[I].SecondsPoint.X <> -1) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].SecondsPoint.X, FClocks[I].SecondsPoint.Y, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
            FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, FClocks[I].SecondsPoint.X, FClocks[I].SecondsPoint.Y);
          end;
          // draw new hand
          if (nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, X1, Y1);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
          // remember coordinates
          FClocks[I].SecondsPoint.X:= X1;
          FClocks[I].SecondsPoint.Y:= Y1;
        end;

      end; // if SecondsHandLength > 0


      // draw minutes hand
      if (FClocks[I].MinuteHandLength > 0) then begin

        // calculate X1, Y1
        // convert minute to angle (in radians)
        Angle:= DegToRad((MM mod 60) * 6); // 6 * 60 = 360°

        // calculate X1, Y1 coordinates
        X1:= Round(X0 + FClocks[I].MinuteHandLength * Sin(Angle));
        Y1:= Round(Y0 - FClocks[I].MinuteHandLength * Cos(Angle));

        // check if the seconfs hand is near the minute hand
        if (FClocks[I].SecondsHandLength > 0) then begin
          Distance:= Abs(MM - SS);
          if (Distance > 30) then
            Distance:= Abs(60 - Distance);
        end else
          Distance:= MIN_DISTANCE;

        // do something in case we have new coordinates or if SS is near MM
        if ((X1 <> FClocks[I].MinutePoint.X) or (Y1 <> FClocks[I].MinutePoint.Y) or (Distance < MIN_DISTANCE)) then begin
          // clear previous hand
          if (FClocks[I].MinutePoint.X <> -1) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].MinutePoint.X, FClocks[I].MinutePoint.Y, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
            FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, FClocks[I].MinutePoint.X, FClocks[I].MinutePoint.Y);
          end;
          // draw new hand
          if (nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, X1, Y1);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
          // remember coordinates
          FClocks[I].MinutePoint.X:= X1;
          FClocks[I].MinutePoint.Y:= Y1;
        end;

      end; // if MinuteHandLength > 0


      // draw hour hand
      if (FClocks[I].HourHandLength > 0) then begin

        // calculate X1, Y1
        // convert hour to angle (in radians)
        Angle:= DegToRad(((HH mod 12) * 30) + (MM * 30 div 60)); // 12 * 30 = 360°

        // calculate X1, Y1 coordinates
        X1:= Round(X0 + FClocks[I].HourHandLength * Sin(Angle));
        Y1:= Round(Y0 - FClocks[I].HourHandLength * Cos(Angle));

        // check if the seconds hand is near the hour hand
        if (FClocks[I].SecondsHandLength > 0) then begin
          Distance:= Abs((HH * 5 + (MM * 5 div 60)) - SS);
          if (Distance > 30) then
            Distance:= Abs(60 - Distance);
        end else
          Distance:= MIN_DISTANCE;

        // now, if Distance in not already below MIN_DISTANCE, then check if the minute hand is near the hour hand
        if (Distance >= MIN_DISTANCE) then begin
          Distance:= Abs((HH * 5) - MM);
          if (Distance > 30) then
            Distance:= Abs(60 - Distance);
        end;

        // do something in case we have new coordinates or if SS or MM is near HH
        if ((X1 <> FClocks[I].HourPoint.X) or (Y1 <> FClocks[I].HourPoint.Y) or (Distance < MIN_DISTANCE)) then begin
          // clear previous hand
          if (FClocks[I].HourPoint.X <> -1) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].HourPoint.X, FClocks[I].HourPoint.Y, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
            FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, FClocks[I].HourPoint.X, FClocks[I].HourPoint.Y);
          end;
          // draw new hand
          if (nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          FPreviewDisplay.GraphicsLayer.Canvas.Line(X0, Y0, X1, Y1);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
          // remember coordinates
          FClocks[I].HourPoint.X:= X1;
          FClocks[I].HourPoint.Y:= Y1;
        end;

      end; // if HandHandLength > 0

    end;
  end;
end;


procedure TMainForm.AddMatrixDrop(const AText: string; MaxTextLen, Row: Integer; SlownessFactor: Byte);
var
  Drop: TMatrixDrop;
begin
  SetLength(FTheMatrix.Drops, Length(FTheMatrix.Drops) + 1);

  Drop.Text:= AText;
  Drop.MaxTextLen:= MaxTextLen;
  Drop.Row:= Row;
  Drop.SlownessFactor:= SlownessFactor;

  FTheMatrix.Drops[High(FTheMatrix.Drops)]:= Drop;
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
  DspRowCount:= (FStudioConfig.DisplayConfig.ResY div GLYPH_H);

  for I:= 0 to High(FTheMatrix.Drops) do begin
    PDrop:= @FTheMatrix.Drops[I];

    DoRepaintFirstChar:= False;
    DoRepaintAll:= False;


    // randomly select a new character
    Pos:= Random(Length(MATRIXLETTERS)) + 1;
    C:= MATRIXLETTERS[Pos];

    if (Length(PDrop^.Text) < PDrop^.MaxTextLen) then begin
      // insert new character at begin if max length is not yet reached
      PDrop^.Text:= C + PDrop^.Text;
      DoRepaintAll:= True;
      DoRepaintFirstChar:= True;
    end else begin
      // just change first character
      PDrop^.Text[1]:= C;
      DoRepaintFirstChar:= True;
    end;

    // check if this drop is to be moved at all
    if ((FTheMatrix.CycleCounter mod PDrop^.SlownessFactor) = 0) then begin
      // inc position and restart at begin if needed
      Inc(PDrop^.Row);
      DoRepaintAll:= True;
      DoRepaintFirstChar:= True;

      if (PDrop^.Row > (DspRowCount + Length(PDrop^.Text) + 1)) then begin
        // restart drop at random position above the visible display area
        PDrop^.Row:= (Length(PDrop^.Text) + Random(20)) * -1;
        PDrop^.SlownessFactor:= Random(3) + 1;
        DoRepaintAll:= False;
        DoRepaintFirstChar:= False;
        // replace drop characters with random content
        for Pos:= 1 to (PDrop^.MaxTextLen - 1) do begin
          C:= MATRIXLETTERS[Random(Length(MATRIXLETTERS)) + 1];
          PDrop^.Text[Pos]:= C;
        end;
      end;
    end;

    BottomRow:= PDrop^.Row;
    TopRow:= PDrop^.Row - Length(PDrop^.Text);

    // check if the drop is (at least partially) within the visible display area
    if ((BottomRow >= 0) and (TopRow < (DspRowCount - 1))) then begin

      if (DoRepaintAll) then
        N:= Length(PDrop^.Text)
      else
        N:= 1;

      // check if the drop actually needs to be repainted
      if (DoRepaintAll or DoRepaintFirstChar) then begin

        for Pos:= 1 to N do begin
          ARow:= BottomRow - Pos + 1;
          if (ARow < 0) then
            Break;
          if (ARow >= DspRowCount) then
            Continue;

          FPreviewDisplay.PaintString(PDrop^.Text[Pos], I, ARow);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);

          if (nil <> FDisplay) then
            FDisplay.PaintString(PDrop^.Text[Pos], I, ARow);

        end;
      end;
    end;



  end;
end;


procedure TMainForm.InitTheMatrix;
var
  I: Integer;
  AText: string;
  MaxTextLen, Row: Integer; SlownessFactor: Byte;
begin

  SetLength(FTheMatrix.Drops, 0);
  for I:= 1 to (FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP)) do begin
    MaxTextLen:= Random(4) + 4;
    SlownessFactor:= Random(3) + 1;
    AText:= ' ';
    Row:= (Random(10) + 1) * -1;
    AddMatrixDrop(AText, MaxTextLen, Row, SlownessFactor);
  end;
end;


procedure TMainForm.UpdateCpuMonitor;
var
  I: Integer;
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

        FPreviewDisplay.PaintString(C, I, FCpuUsageData.BottomRow - Row);
        FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        if (nil <> FDisplay) then
          FDisplay.PaintString(C, I, FCpuUsageData.BottomRow - Row);

      end; // for Row

    end; // end for loop
  end;
end;


procedure TMainForm.UpdateMemMonitor;
var
  I: Integer;
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

        FPreviewDisplay.PaintString(C, I, FMemUsageData.BottomRow - Row);
        FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        if (nil <> FDisplay) then
          FDisplay.PaintString(C, I, FMemUsageData.BottomRow - Row);

      end; // for Row

    end; // end for loop
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
  TotalMem:= FSysInfo.GetDiskSpace(DriveLetter);
  FreeMem:= FSysInfo.GetFreeDiskSpace(DriveLetter);

  // calculate percentage of free bytes
  PercentFree:= FreeMem / TotalMem;

  // calculate number of characters in bar which are used
  UsedNum:= Round(BarWidth * (1.0 - PercentFree));

  for X:= 0 to (BarWidth - 1) do begin
    if (X < UsedNum) then
      C:= Chr($87)
    else
      C:= Chr($8E);
    if (nil <> FDisplay) then
      FDisplay.PaintString(C, X + Col, Row);
    FPreviewDisplay.PaintString(C, X + Col, Row);
    FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
  end;
end;

end.

