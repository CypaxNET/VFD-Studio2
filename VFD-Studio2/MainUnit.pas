unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  NTK300, NTK800, VFDisplay, IniFiles, Menus, ExtCtrls, InfoUnit, uSMBIOS,
  SysInfo, WinampControl, LCLTranslator, ComCtrls, DateUtils, Math,
  StudioCommon, Glyphs, lclintf;

const MAX_VARIABLE_INFO = 10;
const MAX_CLOCKS = 4;
const VERSION_STR = '2.0.0.0';

type

  TSearchDirection = (sdForward, sdBackward);

  TApplicationConfig = record
    Language: string;
    PreviewDisplayColor: TColor; // color of the pixels in the preview display
  end;

  TDisplayConfig = record
    DisplayType: string; // e.g. 'NTK800'
    ResX: Word;          // display resolution in x direction
    ResY: Word;          // display resolution in y direction
    IntName: string;     // interface name (e.g. 'COM5')
    Baudrate: Cardinal;  // baudrate for serial display connection
  end;

  TListConfig = record
    ListName: string;
  end;

  TAnimationConfig = record
    PlayOnlyOnIdle: Boolean; // play animations only then the CPU is idle?
    IdlePercent: Byte;       // which percentage of CPU usage still counts as idle?
    IdleTime: Word;          // number of seconds the CPU must have been idle?
  end;

  TExtraConfig = record
    ShowTime: Word;
    OVerviewTime: Word;
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
    BitBtn1: TBitBtn;
    ExpertViewButton: TBitBtn;
    ImageList1: TImageList;
    HyperlinkLabel: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MenuItem1: TMenuItem;
    Panel6: TPanel;
    SaveDialog: TSaveDialog;
    VersionLabel: TLabel;
    LangDeButton: TBitBtn;
    LangEnButton: TBitBtn;
    ColorPanel: TPanel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    OnlyOnIdleBox: TCheckBox;
    Panel5: TPanel;
    GroupBox2: TGroupBox;
    Panel4: TPanel;
    SaveLogButton: TBitBtn;
    ClearLogButton: TBitBtn;
    ExitButton: TBitBtn;
    ExtListBox: TListBox;
    InfoButton: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ListBox: TListBox;
    ListTestButton: TBitBtn;
    ColorDialog: TColorDialog;
    CombinedImage: TImage;
    LogListView: TListView;
    NextButton: TBitBtn;
    OKButton: TBitBtn;
    Panel1: TPanel;
    Exit1: TMenuItem;
    ExtraTimer: TTimer;
    HideTimer: TTimer;
    InfoTimer: TTimer;
    Listeladen1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    NchsterScreen1: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    PopupStopButton: TMenuItem;
    ReloadButton: TBitBtn;
    SCR_Time_Label: TLabel;
    Show1: TMenuItem;
    AnimateTimer: TTimer;
    StopButton: TBitBtn;
    TrayIcon1: TTrayIcon;
    UsageTimer: TTimer;
    WaitTimer: TTimer;
    procedure AnimateTimerTimer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
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
    procedure GroupBox1Resize(Sender: TObject);
    procedure HideTimerTimer(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure InfoTimerTimer(Sender: TObject);
    procedure HyperlinkLabelClick(Sender: TObject);
    procedure LangDeButtonClick(Sender: TObject);
    procedure LangEnButtonClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure Listeladen1Click(Sender: TObject);
    procedure ListTestButtonClick(Sender: TObject);
    procedure LoadListButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure NchsterScreen1Click(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OnlyOnIdleBoxChange(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure PopupStopButtonClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure SaveLogButtonClick(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure UsageTimerTimer(Sender: TObject);
    procedure ViewListPanelClick(Sender: TObject);
    procedure LoadList(ListFileName: TFileName);
    procedure CreateScreen;
    function InterpreteListCommand(S:string): Integer;
    procedure VirtualDisplayLayer0Paint(Sender: TObject);
    procedure WaitTimerStartTimer(Sender: TObject);
    procedure WaitTimerStopTimer(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);
    procedure ClearInfoStrings;

    function SeekTo(StartIndex: Integer; SearchDirection: TSearchDirection; Cmd: string): Integer;
    procedure StopProcessing;

    procedure HandleTextOutput(AText: string; X, Y: Byte; FontName: string; FontSize: Integer);
    procedure RefreshTextOutputs;
    procedure BitmapToVFD(FileName: string; X, Y: Word);
    function DrawFontedText(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): TPoint;
    procedure UpdateTimeLabel;
    procedure Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, fWidth: Word);
    procedure DrawAnimationFrame(ABitmap: TBitmap; X, Y, Frame, FrameWidth: Word);
    procedure PauseAnimation;
    procedure StopAnimation;
    procedure AddClock(Offset: Integer; X, Y, HourHandLength, MinuteHandLength, SecondsHandLength: Word);
    procedure DisableClocks;
    procedure RefreshClocks;
    procedure TrimBitmap(var Bmp: TBitmap);

    procedure LogEvent(const LogLevel: TLogLevel; const AText: string; const Timestamp: TDateTime);
    procedure RepaintVirtualDisplay;
    procedure PaintStringOnVirtualDisplay(AText: string; Col, Row: Integer);
    procedure CombineVirtualLayers(Sender: TObject);

    procedure LoadConfig(const AFilePath: string);
    procedure SaveConfig(const AFilePath: string);

    function SubstituteStaticInfo(AText: string): string;
    function SubstituteVariableInfo(AText: string): string;
    function AddVariableInfo(AText: string; X, Y: Byte; FontName: string; FontSize: Integer): Boolean;

  private
  protected

    FVirtualLayer0: TBitmap;
    FVirtualLayer1: TBitmap;

    FStudioConfig: TStudioConfig;
    FDisplay: TVFDisplay;
    FSysInfo: TSysInfo;
    FWinampControl: TWinampControl;

    FListIndex: Integer;   // position in ListBox

    FVariableInfo: Array[0..(MAX_VARIABLE_INFO - 1)] of TVariableInfo;  // holds textual information to be refreshed periodically

    FClocks: Array[0..(MAX_CLOCKS - 1)] of TClockData; // holds data of analog clocks

    //Für Extratimer
    ShowTime, OverviewTime: Integer;

    FAnimationData: TAnimationData;

    FIsCurrentScreenShownOnce: Boolean;
    FRemainingSeconds: Integer; // remaining number of seconds the current screen is shown

    FScrollStringIndex: Word;


    //ExtraScreens
    CPUMONITORenabled: Boolean;
    CPUMONITORcharEnabled: Boolean;
    RAMMONITORenabled: Boolean;
    RAMMONITORcharEnabled: Boolean;

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

  {Next screen announcement }
  RsNextScreenText = 'Next screen in';

{ TMainForm }


// clear info strings
procedure TMainForm.ClearInfoStrings;
var
  I: Integer;
begin
  for I:= 0 to (MAX_VARIABLE_INFO-1) do begin
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
  SMBios: TSMBios;
  S: string;
begin
  S:= AText;

  SMBios:= TSMBios.Create;

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

  while (Pos('$MEMORY$', S) <> 0 ) do begin
    I:= Pos('$MEMORY$', S);
    Delete(S, I, Length('$MEMORY$'));
    Insert(IntToStr(Round(FSysInfo.GetTotalMemory / 1048576)), S, I);
  end;

  while (Pos('$OS$', S) <> 0 ) do begin
    I:= Pos('$OS$', S);
    Delete(S, I, Length('$OS$'));
    Insert(FSysInfo.GetOperatingSystem, S, I);
  end;

  while (Pos('$CPUCORES$', S) <> 0 ) do begin
    I:= Pos('$CPUCORES$', S);
    Delete(S, I, Length('$CPUCORES$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(IntToStr(SMBios.ProcessorInfo[0].RAWProcessorInformation^.CoreCount), S, I);
    end else begin
      Insert('?', S, I);
    end;
  end;

  while (Pos('$CPUMAX$', S) <> 0 ) do begin
    I:= Pos('$CPUMAX$', S);
    Delete(S, I, Length('$CPUMAX$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(IntToStr(SMBios.ProcessorInfo[0].RAWProcessorInformation^.MaxSpeed), S, I);
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
    if (SMBios.HasProcessorInfo) then begin
     Insert(SMBios.ProcessorInfo[0].SocketDesignationStr, S, I);
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
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.ProcessorInfo[0].ProcessorFamilyStr, S, I);
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
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BiosInfo.VendorStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSVERSION$', S) <> 0 ) do begin
    I:= Pos('$BIOSVERSION$', S);
    Delete(S, I, Length('$BIOSVERSION$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BiosInfo.VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSDATE$', S) <> 0 ) do begin
    I:= Pos('$BIOSDATE$', S);
    Delete(S, I, Length('$BIOSDATE$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BiosInfo.ReleaseDateStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVENDOR$', S) <> 0 ) do begin
    I:= Pos('$BOARDVENDOR$', S);
    Delete(S, I, Length('$BOARDVENDOR$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BaseBoardInfo[0].ManufacturerStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDPRODUCT$', S) <> 0 ) do begin
    I:= Pos('$BOARDPRODUCT$', S);
    Delete(S, I, Length('$BOARDPRODUCT$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BaseBoardInfo[0].ProductStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVERSION$', S) <> 0 ) do begin
    I:= Pos('$BOARDVERSION$', S);
    Delete(S, I, Length('$BOARDVERSION$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.BaseBoardInfo[0].VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;


  (* usefull?
  while (Pos('$SLOT$', S) <> 0 ) do begin
    I:= Pos('$SLOT$', S);
    Delete(S, I, Length('$SLOT$'));
    if (SMBios.HasProcessorInfo) then begin
      Insert(SMBios.SystemSlotInfo[0].GetCurrentUsage, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;
  *)

  SMBios.Free;

  Result:= S;
end;


function TMainForm.SubstituteVariableInfo(AText: string): string;
var
  I: Integer;
  S: string;
  Sec: string;  //Seconds (Winamp)
  SMBios: TSMBios;
  CurrentDateTime: TDateTime;
  DoW: Word;
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
begin
  SMBios:= TSMBios.Create;
  S:= AText;
  CurrentDateTime:= Now;
  DecodeDateTime(CurrentDateTime, Year, Month, Day, Hour, Minute, Second, MilliSecond);

  while (Pos('$FREEMEM$', S) <> 0 ) do begin
    I:= Pos('$FREEMEM$', S);
    Delete(S, I, Length('$FREEMEM$'));
    Insert(IntToStr(Round(FSysInfo.GetFreeVirtualMemory / 1048576)), S, I);
  end;

  {
  while (Pos('$AVERAGECPU$', S) <> 0 ) do begin
    I:= Pos('$AVERAGECPU$', S);
    Delete(S, I, Length('$AVERAGECPU$'));
    Insert(format('%f', [averageCPUusage]), S, I);
  end;
  }

  while (Pos('$CPUUSAGE$', S) <> 0 ) do begin
    I:= Pos('$CPUUSAGE$', S);
    Delete(S, I, Length('$CPUUSAGE$'));
    Insert(IntToStr(Round(FSysInfo.GetCpuUsage*100.0)), S, I);
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
    if (SMBios.HasProcessorInfo) then begin
      Insert(IntToStr(SMBios.ProcessorInfo[0].RAWProcessorInformation^.CurrentSpeed), S, I);
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
        if(nil <> FDisplay) then
          FDisplay.PaintString(S, X, Y);
        PaintStringOnVirtualDisplay(S, X, Y);
      end else begin
        DrawFontedText(S, X, Y, FontName, FontSize);
      end;
    end;

  end else begin
    // no (more) '$' in the string -> its just some staic text
    if (FontName = '') or (FontSize = 0) then begin
      if (nil <> FDisplay ) then
        FDisplay.PaintString(S, X, Y);
      PaintStringOnVirtualDisplay(S, X, Y);
    end else begin
      DrawFontedText(S, X, Y, FontName, FontSize);
    end;
  end;

  RefreshTextOutputs;

end;


//Infos, die sich ständig ändern
procedure TMainForm.RefreshTextOutputs;
var
  z: Integer;
  OldText: string;
  S: string;
  tWidth: Integer;
  overlength: Integer;
  subs: string;
  p0: Integer;
  SMBios: TSMBios;
  BmpDimensions: TPoint;
begin
  SMBios:= TSMBios.Create;

  for z:= 0 to (MAX_VARIABLE_INFO - 1) do begin
    S:= FVariableInfo[z].Text;
    OldText:= FVariableInfo[z].SubsText;
    S:= SubstituteVariableInfo(S);
    if(nil <> FDisplay) then
      tWidth:= FDisplay.TextWidth // get number of characters one line can show
    else
      tWidth:= FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP);

    tWidth:= tWidth - FVariableInfo[z].x;
    overlength:= Length(S) - tWidth;

    if ((Trim(S) <> Trim(OldText)) or (overlength > 0)) then begin    // repainting is only required if the text has changed or if it has overlength

      //while (Length(S)<43 ) do S:= S + ' ';   // string bis Displayende mit Leerstellen füllen
      if (FVariableInfo[z].FontName = '') or (FVariableInfo[z].FontSize = 0) then begin
        if (overlength > 0) then begin  // the text is to long to be displayed

          overlength:= overlength + 6; // we add 6 'virtual' character to the overlength to stay a little longer at the start and the end

          p0:= FScrollStringIndex mod Word(overlength);
          if p0 <3 then
            p0:=0
          else
            p0:= p0-3;

          if (p0 >= overlength-6) then
            p0:= overlength-6;

          subs:= S.Substring(p0, tWidth);
          FVariableInfo[z].PrevWidth:= Length(subs);
          if(nil <> FDisplay) then
            FDisplay.PaintString(subs, FVariableInfo[z].x, FVariableInfo[z].y);
          PaintStringOnVirtualDisplay(subs, FVariableInfo[z].x, FVariableInfo[z].y);
        end else begin
          while (FVariableInfo[z].PrevWidth > Length(S) ) do begin
           S:= S + ' '; // if the text was longer previously, we need to add whitespaces to clear the remainings of the previous text
          end;
          FVariableInfo[z].PrevWidth:= Length(S);
          if(nil <> FDisplay) then
            FDisplay.PaintString(S, FVariableInfo[z].x, FVariableInfo[z].y);
          PaintStringOnVirtualDisplay(S, FVariableInfo[z].x, FVariableInfo[z].y);
        end;
      end else begin // it is text with font
        BmpDimensions:= DrawFontedText(S, FVariableInfo[z].x, FVariableInfo[z].y, FVariableInfo[z].FontName, FVariableInfo[z].FontSize);
        if (BmpDimensions.X < FVariableInfo[z].PrevWidth) then begin
          // the previous text was longer
          // TODO
        end;
        if (BmpDimensions.Y < FVariableInfo[z].PrevHeight) then begin
          // the previous text was higher
          // TODO
        end;
        FVariableInfo[z].PrevWidth:= BmpDimensions.X;
        FVariableInfo[z].PrevHeight:= BmpDimensions.Y;
      end;
    end;

    FVariableInfo[Z].SubsText:= S;
  end;  //for z

  SMBios.Free;
  Inc(FScrollStringIndex);

end;


procedure TMainForm.LoadList(ListFileName: TFileName);
var
   I: Integer;
   S: string;
   hs: string; //Hilfsstring
   z: Integer; //Hilfszähler
begin
  StopProcessing;
  FListIndex:= 0;

  //Liste laden
  ListBox.Items.LoadFromFile(ListFileName);

  //externe Listen einfügen
  I:= 0;
  while (I < ListBox.Items.Count) do begin
    S:= ListBox.items[I];
    if (Pos('LOADSCREEN', S) = 1) then begin
      hs:= Copy(S, 12, Length(S) - 11);
      ExtListBox.Items.loadfromfile(hs);
      ListBox.Items.Delete(I);
      // an [I] die Strings aus ExtListBox einfügen
      for z:= ExtListBox.Items.Count - 1 downto 0 do begin
        ListBox.Items.Insert(I, ExtListBox.Items[z]);
      end;
    end;
    Inc(I);
  end;


  //Kommentare und Leerzeilen aus Liste entfernen
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
  cmdparts: TStringList;
  cmd: string;
  n, ITmp: Integer;
  X, Y: Integer;
  P1, P2, P3, P4, P5, P6: Integer;
  IsInverted: Boolean;
  IsRequirementMet: Boolean;
  AColor: TColor;
  Res: Integer;
begin
  Res:= -1;

  cmdparts:= TStringList.Create;
  try
    cmdparts.Delimiter:= ' ';
    cmdparts.QuoteChar:= '''';
    cmdparts.DelimitedText:= S;

    if (cmdparts.Count > 0) then begin
      cmd:= cmdparts[0];

      if ('NEWSCREEN' = cmd) then begin
        StopProcessing;
        if(nil <> FDisplay) then
          FDisplay.ShowScreen(BOTH_LAYERS);

        FIsCurrentScreenShownOnce:= False; // by default this screen will be shown again then the list loops
        if (cmdparts.Count >= 2) then begin
          // p1 = screen limitations
          if ('ONCE' = cmdparts[1]) then begin
            // display this screen only once
            FIsCurrentScreenShownOnce:= True;
          end else if (cmdparts[1].StartsWith('REQUIRE')) then begin
            // this screen shall only be shown if a requirement is met
            IsRequirementMet:= False; // False by default, must be set to True in code below

            if ('REQUIREWINAMP' = cmdparts[1]) then begin
              // check if Winamp is running
              if (FWinampControl.IsWinampRunning) then
                IsRequirementMet:= True;
            end;

            if (not IsRequirementMet) then begin
              //LogEvent(lvINFO, 'Requirment ' + cmdparts[1] + ' is not met -> skip to next screen', Now);

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

      end else if ('SCREENEND' = cmd) then begin
        if (True = FIsCurrentScreenShownOnce) then begin
          // remove this screen from the list

          // go back to last 'NEWSCREEN' line
          ITmp:= SeekTo(FListIndex, sdBackward, 'NEWSCREEN');

          if ((-1 = ITmp) or (ITmp > FListIndex)) then
            LogEvent(lvERROR, 'Could not find start of current screen.', Now)
          else begin
            LogEvent(lvINFO, 'This screen was once. Clearing lines ' + IntToStr(ITmp) + '..' + IntToStr(FListIndex), Now);
            ListBox.MultiSelect:= True;
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

      end else if ('DSPINIT' = cmd) then begin
         if(nil <> FDisplay) then
           FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);

      end else if ('STOP' = cmd) then begin
         WaitTimer.Enabled:= False;
         StopButton.caption:= RsBtnGo;
         Popupstopbutton.caption:= RsBtnGo;
         StopButton.ImageIndex:= 1;

      end else if ('CLEARSCREEN' = cmd) then begin
        if(nil <> FDisplay) then
          FDisplay.ClearScreen;
        FVirtualLayer0.Canvas.Brush.Color:= clWhite;;
        FVirtualLayer0.Canvas.FillRect(0, 0, FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);
        FVirtualLayer1.Canvas.Brush.Color:= clWhite;;
        FVirtualLayer1.Canvas.FillRect(0, 0, FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);
        CombineVirtualLayers(Self);

      end else if ('SCREENTIME' = cmd) then begin
        // p1 = screen time in seconds
        if (cmdparts.Count >= 2) then begin
          P1:= strtoint(cmdparts[1]);
          WaitTimer.Interval:= P1 * 1000;
          //SCR_Time_Label.caption:= 'ScreenTime: ' + floattostr(WaitTimer.Interval/1000)+ 'S';
          waittimer.Enabled:= True;
        end;

      end else if ('LIGHT' = cmd) then begin
        // p1 = brightness level
       if (cmdparts.Count >= 2) then begin
         P1:= strtoint(cmdparts[1]);
         if(nil <> FDisplay) then
           FDisplay.SetBrightness(P1);
       end;

      end else if ('NOISE' = cmd) then begin
        // p1 = amount of pixels, p2 = inverted or not [optional]
         if (cmdparts.Count >= 2) then begin
           P1:= strtoint(cmdparts[1]);
           if ((cmdparts.Count >= 3) and ((cmdparts[2].ToUpper = 'TRUE') or (cmdparts[2] = '1'))) then begin
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
             if(nil <> FDisplay) then
               FDisplay.PaintPixel(X, Y, IsInverted);
             FVirtualLayer0.Canvas.Pixels[X, Y]:= AColor;
           end;
           CombineVirtualLayers(Self);
         end;

      end else if ('PIXEL' = cmd) then begin
        // p1 = x, p2 = y, p3 = inverted or not [optional]
        if (cmdparts.Count >= 3) then begin
          P1:= strtoint(cmdparts[1]);
          P2:= strtoint(cmdparts[2]);
          if ((cmdparts.Count >= 4) and ((cmdparts[3].ToUpper = 'TRUE') or (cmdparts[3] = '1'))) then begin
            if(nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, True);
            FVirtualLayer0.Canvas.Pixels[P1, P2]:= clWhite;
          end else begin
            if(nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, False);
            FVirtualLayer0.Canvas.Pixels[P1, P2]:= clBlack;
          end;
          CombineVirtualLayers(Self);
        end;

      end else if ('LINE' = cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (cmdparts.Count >= 5) then begin
          P1:= strtoint(cmdparts[1]);
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          P4:= strtoint(cmdparts[4]);
          if ((cmdparts.Count >= 6) and ((cmdparts[5].ToUpper = 'TRUE') or (cmdparts[5] = '1'))) then begin
            if(nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, True);
            FVirtualLayer0.Canvas.Pen.Color:= clWhite;
          end else begin
            if(nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, False);
            FVirtualLayer0.Canvas.Pen.Color:= clBlack;
          end;
          FVirtualLayer0.Canvas.Line(P1, P2, P3 + 1, P4 + 1);
          CombineVirtualLayers(Self);
        end;

      end else if ('FRAME' = cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (cmdparts.Count >= 5) then begin
          P1:= strtoint(cmdparts[1]);
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          P4:= strtoint(cmdparts[4]);
          if ((cmdparts.Count >= 6) and ((cmdparts[5].ToUpper = 'TRUE') or (cmdparts[5] = '1'))) then begin
            if(nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, True);
            FVirtualLayer0.Canvas.Pen.Color:= clWhite;
          end else begin
            if(nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, False);
            FVirtualLayer0.Canvas.Pen.Color:= clBlack;
          end;
          FVirtualLayer0.Canvas.Frame(P1, P2, P3 + 1, P4 + 1);
          CombineVirtualLayers(Self);
        end;

      end else if ('PLAINTEXT' = cmd) then begin
        // p1 = text, p2 = x, p3 = y
        if (cmdparts.Count >= 4) then begin
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          // does the text to be displayed include any '$' characters?
          if (cmdparts[1].Contains('$') ) then begin
            HandleTextOutput(cmdparts[1], P2, P3, '', 0);
          end else begin
            if(nil <> FDisplay) then
              FDisplay.PaintString(cmdparts[1], P2, P3);
            PaintStringOnVirtualDisplay(cmdparts[1], P2, P3);
          end;
        end;

      end else if ('TEXTOUT' = cmd) then begin
        // p1 = text, p2 = x, p3 = y, p4 = font size, p5 = font name
        if (cmdparts.Count >= 6) then begin
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          P4:= strtoint(cmdparts[4]);
          // does the text to be displayed include any '$' characters?
          if (cmdparts[1].Contains('$') ) then begin
            HandleTextOutput(cmdparts[1], P2, P3, cmdparts[5], P4);
          end else begin
            DrawFontedText(cmdparts[1], P2, P3, cmdparts[5], P4);
          end;
        end;

      end else if ('ANIMATE' = cmd) then begin
        // p1 = file name, p2 = animation speed, p3 = x, p4 = y, p5 = frame width
        if (False = FAnimationData.IsAnimationDisplayed) then begin
          // only one animation per screen is supported
          if (cmdparts.Count >= 6) then begin
            P2:= strtoint(cmdparts[2]);
            P3:= strtoint(cmdparts[3]);
            P4:= strtoint(cmdparts[4]);
            P5:= strtoint(cmdparts[5]);
            Animate(cmdparts[1], P2, P3, P4, P5);
            FAnimationData.IsAnimationDisplayed:= True;
          end;
        end;

      end else if ('BITMAP' = cmd) then begin
        // p1 = file path, p2 = x, p3 = y
        if (cmdparts.Count >= 4) then begin
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          BitmapToVFD(cmdparts[1], P2, P3);
        end;

      end else if ('CLOCK' = cmd) then begin
        // p1 = offset in minutes, p2 = x, p3 = y, p4 = hour hand length, p5 = minute hand length, p6 = seconds hand length
        if (cmdparts.Count >= 7) then begin
          P1:= strtoint(cmdparts[1]);
          P2:= strtoint(cmdparts[2]);
          P3:= strtoint(cmdparts[3]);
          P4:= strtoint(cmdparts[4]);
          P5:= strtoint(cmdparts[5]);
          P6:= strtoint(cmdparts[6]);
          AddClock(P1, P2, P3, P4, P5, P6);
        end;
      end;


    end; // endif (cmdparts.Count > 0)

  finally
    cmdparts.Free;
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
      vfd.SelectScreen(strtoint(p1));
     end
     else if (Pos('CLEARSCREEN', S)= 1 ) then begin
      p1:= Copy(S, 13, 1); // Screennummer ermitteln
      vfd.ClearScreen(strtoint(p1));
      if (p1='1' ) then vfd.ClearBitmap; //wenn GFX_Screen gelöscht wurde, auch Bitmapobjekt löschen
     end
     else if (Pos('FADEOUT', S)= 1 ) then begin
      p1:= Copy(S, 9, l - 8); // Geschwindigkeit ermitteln
      vfd.Fade(True, strtoint(p1));
     end
     else if (Pos('FADEIN', S)= 1 ) then begin
      p1:= Copy(S, 8, l - 7); // Geschwindigkeit ermitteln
      vfd.Fade(False, strtoint(p1));
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
       vfd.Animate(p1, strtoint(p2), strtoint(p3));
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
      vfd.SetPixelbyte(strtoint(p1), strtoint(p2), strtoint(p3));
     end
     else if (Pos('TIME', S)= 1 ) then begin
      p2:= Copy(S, l - 3, 4);  //Position
      p1:= Copy(S, 6, l - 9); //Analog oder Digital?
      if (p1= 'ANALOG' ) then vfd.Time(True, strtoint(p2))
      else vfd.time(False, strtoint(p2));
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
      BitmapToVFD(p1, strtoint(p2), strtoint(p3), strtoint(p4), strtoint(p5), stringtocolor(p6));
     end;
}
  Result:= Res;
end;

procedure TMainForm.VirtualDisplayLayer0Paint(Sender: TObject);
begin

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
  // Bei ListEnde wieder zum ListAnfang springen
  if (FListIndex < ListBox.Items.Count - 1) then
    Inc(FListIndex)
  else
    FListIndex:= 0;

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

    { display section }
    FStudioConfig.DisplayConfig.DisplayType:= IniFile.ReadString( 'DISPLAY', 'Type',      '');
    FStudioConfig.DisplayConfig.ResX:=        IniFile.ReadInteger('DISPLAY', 'ResX',      128);
    FStudioConfig.DisplayConfig.ResY:=        IniFile.ReadInteger('DISPLAY', 'ResY',      64);
    FStudioConfig.DisplayConfig.IntName:=     IniFile.ReadString( 'DISPLAY', 'Interface', 'COM1');
    FStudioConfig.DisplayConfig.Baudrate:=    IniFile.ReadInteger('DISPLAY', 'Baud',      115200);

    { list section }
    FStudioConfig.ListConfig.ListName:= IniFile.ReadString('LIST', 'Listname', 'Default.lst');

    { animations section }
    FStudioConfig.AnimationConfig.PlayOnlyOnIdle:= IniFile.ReadBool(   'ANIMATIONS', 'PlayOnlyOnIdle', False);
    FStudioConfig.AnimationConfig.IdlePercent:=    IniFile.ReadInteger('ANIMATIONS', 'IdleLevel',      20);
    FStudioConfig.AnimationConfig.IdleTime:=       IniFile.ReadInteger('ANIMATIONS', 'IdleTime',       60);

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

    { display section }
    IniFile.WriteString( 'DISPLAY', 'Type',      FStudioConfig.DisplayConfig.DisplayType);
    IniFile.WriteInteger('DISPLAY', 'ResX',      FStudioConfig.DisplayConfig.ResX);
    IniFile.WriteInteger('DISPLAY', 'ResY',      FStudioConfig.DisplayConfig.ResY);
    IniFile.WriteString( 'DISPLAY', 'Interface', FStudioConfig.DisplayConfig.IntName);
    IniFile.WriteInteger('DISPLAY', 'Baud',      FStudioConfig.DisplayConfig.Baudrate);

    { list section }
    IniFile.WriteString('LIST', 'Listname', FStudioConfig.ListConfig.ListName);

    { animations section }
    IniFile.WriteBool(   'ANIMATIONS', 'PlayOnlyOnIdle', FStudioConfig.AnimationConfig.PlayOnlyOnIdle);
    IniFile.WriteInteger('ANIMATIONS', 'IdleLevel',      FStudioConfig.AnimationConfig.IdlePercent);
    IniFile.WriteInteger('ANIMATIONS', 'IdleTime',       FStudioConfig.AnimationConfig.IdleTime);

  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFilePath: string;
begin

  FVirtualLayer0:= TBitmap.Create;
  FVirtualLayer1:= TBitmap.Create;

  LogEvent(lvINFO, 'Application started. Version ' + VERSION_STR , Now);

  IniFilePath:= extractfilepath(application.ExeName) + 'vfdstudio.ini';

  LoadConfig(IniFilePath);

  ColorPanel.Color:= FStudioConfig.ApplicationConfig.PreviewDisplayColor;

  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);

  (* obsolete
  ShowTime:= VFDIni.ReadInteger('EXTRAS', 'ShowTime', 3000);
  OverviewTime:= VFDIni.ReadInteger('EXTRAS', 'OverviewTime', 6000);
  cfgDspType:= VFDIni.ReadString('SETTINGS', 'Display', '');
  ToDoList:= VFDIni.ReadString('LIST', 'Listname', '');
  PlayOnlyOnIdle:= VFDIni.ReadBool('ANIMATIONS', 'PlayOnlyOnIdle', False);
  IdleTime:= VFDIni.ReadInteger('ANIMATIONS', 'IdleTime', 45);
  IdleLevel:= VFDIni.ReadInteger('ANIMATIONS', 'IdleLevel', 25);
  *)

  if ('NTK800' = FStudioConfig.DisplayConfig.DisplayType) then begin
    FDisplay:= TNTK800.Create(Self);
  end else if ('NTK300' = FStudioConfig.DisplayConfig.DisplayType) then begin
    FDisplay:= TNTK300.Create(Self);
  end;

  if (nil <> FDisplay) then begin
    //VirtualDisplayLayer0.Width:= FStudioConfig.DisplayConfig.ResX;
    //VirtualDisplayLayer0.Height:= FStudioConfig.DisplayConfig.ResY;
    FDisplay.OnDbgMessage:= @LogEvent;
    FDisplay.Connect(FStudioConfig.DisplayConfig.IntName);
    FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);
  end;
  CombinedImage.Width:= FStudioConfig.DisplayConfig.ResX * 2;
  CombinedImage.Height:= FStudioConfig.DisplayConfig.ResY * 2;
  CombinedImage.Picture.Bitmap.Width:= FStudioConfig.DisplayConfig.ResX;
  CombinedImage.Picture.Bitmap.Height:= FStudioConfig.DisplayConfig.ResY;
  FVirtualLayer0.Width:= FStudioConfig.DisplayConfig.ResX;
  FVirtualLayer0.Height:= FStudioConfig.DisplayConfig.ResY;
  FVirtualLayer1.Width:= FStudioConfig.DisplayConfig.ResX;
  FVirtualLayer1.Height:= FStudioConfig.DisplayConfig.ResY;


  FSysInfo:= TSysInfo.Create(self);
  FWinampControl:= TWinampControl.Create(self);

  AnimateTimer.Enabled:= False;

  try
    FAnimationData.AnimationBitmap:= TBitmap.create;
  except
    FAnimationData.AnimationBitmap.Free;
  end;


  HideTimer.Enabled:= True;

  VersionLabel.Caption:= 'v' + VERSION_STR;

end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  IniFilePath: string;
  AStringList: TStringList;
  I: Integer;
begin
  LogEvent(lvINFO, 'Application closed.', Now);

  IniFilePath:= extractfilepath(application.ExeName) + 'vfdstudio.ini';
  SaveConfig(IniFilePath);

  AStringList:= TStringList.Create;
  try
    AStringList.Add('#;Time;Level;Message');
    for I := 0 to (LogListView.Items.Count - 1) do
      AStringList.Add(LogListView.Items[I].Caption + ',' + // #
        LogListView.Items[i].SubItems[0] + ';' +           // Time
        LogListView.Items[i].SubItems[1] + ';' +           // Level
        '"' + LogListView.Items[i].SubItems[2] + '"');     // Message
    try
      AStringList.SaveToFile(extractfilepath(application.ExeName) + 'vfdstudio.log');
    finally
    end;
  finally
    AStringList.Free;
  end;

  CanClose:= True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSysInfo.Free;
  FWinampControl.Free;
  FDisplay.Free;
  FAnimationData.AnimationBitmap.Free;
  FVirtualLayer0.Free;
  FVirtualLayer1.Free;
end;

procedure TMainForm.GroupBox1Resize(Sender: TObject);
begin
  GroupBox1.ChildSizing.ControlsPerLine:= GroupBox1.Width div (LangDeButton.Width + GroupBox1.ChildSizing.HorizontalSpacing);
end;

procedure TMainForm.HideTimerTimer(Sender: TObject);
begin
  //TODO: Hide;

  HideTimer.Enabled:= False; // self - disabling

  // load last list as specified in ini file
  Mainform.LoadList(extractfilepath(application.ExeName) + 'Lists\' + FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.InfoButtonClick(Sender: TObject);
begin
  InfoForm.Show;
end;

procedure TMainForm.UpdateTimeLabel;
var
  h, m, S: Integer;
begin
  if (WaitTimer.Enabled) then begin
    h:= Trunc(FRemainingSeconds / 3600);
    m:= Trunc((FRemainingSeconds mod 3600) / 60);
    S:= FRemainingSeconds mod 60;
    SCR_Time_Label.caption:= RsNextScreenText + Format(' %d:%.02d:%.02d', [h, m, S]);
  end else begin
    SCR_Time_Label.caption:= RsNextScreenText + ' -: - -: - -';
  end;
end;

procedure TMainForm.InfoTimerTimer(Sender: TObject);
begin

  UpdateTimeLabel;
  if (FRemainingSeconds > 0) then
    Dec(FRemainingSeconds);

  RefreshTextOutputs;
  RefreshClocks;

end;

procedure TMainForm.HyperlinkLabelClick(Sender: TObject);
begin
  OpenURL('http://cypax.net');
end;

procedure TMainForm.LangDeButtonClick(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'de';
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
end;

procedure TMainForm.LangEnButtonClick(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'en';
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
  StopAnimation; // this also disables the animation timer
  DisableClocks;
  ClearInfoStrings;
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

procedure TMainForm.LoadListButtonClick(Sender: TObject);
begin
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

procedure TMainForm.Panel3Click(Sender: TObject);
begin

end;

procedure TMainForm.PopupStopButtonClick(Sender: TObject);
begin
  StopButtonClick(Sender);
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  Mainform.LoadList(extractfilepath(application.ExeName) + 'Lists\' + FStudioConfig.ListConfig.ListName);
end;

procedure TMainForm.SaveLogButtonClick(Sender: TObject);
var
  AStringList: TStringList;
  I: Integer;
begin
  if SaveDialog.Execute then begin

    AStringList:= TStringList.Create;
    try
      AStringList.Add('#;Time;Level;Message');
      for I := 0 to (LogListView.Items.Count - 1) do
        AStringList.Add(LogListView.Items[I].Caption + ',' + // #
          LogListView.Items[i].SubItems[0] + ';' +           // Time
          LogListView.Items[i].SubItems[1] + ';' +           // Level
          '"' + LogListView.Items[i].SubItems[2] + '"');     // Message
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

procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  WaitTimer.Enabled:= not WaitTimer.Enabled;
  if (True = Waittimer.Enabled) then begin
    StopButton.caption:= RsBtnStop;
    StopButton.ImageIndex:= 0;
    Popupstopbutton.caption:= RsBtnStop;
    PopupStopButton.ImageIndex:=0;
  end
  else begin
    StopButton.caption:= RsBtnGo;
    StopButton.ImageIndex:= 1;
    PopupStopButton.caption:= RsBtnGo;
    PopupStopButton.ImageIndex:=1;
  end;
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure TMainForm.UsageTimerTimer(Sender: TObject);
begin
  if (nil <> FSysInfo) then
    FSysInfo.UpdateCpuUsage;
end;

procedure TMainForm.ViewListPanelClick(Sender: TObject);
begin
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  MainForm.close;
end;

procedure TMainForm.ExtraTimerTimer(Sender: TObject);
begin

end;



procedure TMainForm.Exit1Click(Sender: TObject);
begin
  MainForm.close;
end;

procedure TMainForm.BitmapToVFD(FileName: string; X, Y: word);
var
  TmpBitmap: TBitmap;
begin
  TmpBitmap:= TBitmap.Create;
  TmpBitmap.LoadFromFile(FileName);
  //LogEvent(lvINFO, 'Loading bitmap. Size ' + IntToStr(TmpBitmap.Width) + 'x' + IntToStr(TmpBitmap.Height), Now);
  if(nil <> FDisplay) then
    FDisplay.PaintBitmap(TmpBitmap, X, Y);
  FVirtualLayer0.Canvas.Draw(X, Y, TmpBitmap);
  CombineVirtualLayers(Self);
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
  for Y := Bmp.Height - 1 downto 1 do begin // 'downto 1' and not 'downto 0' because the reaining image should be at least 1 pixel in height
    IsNonWhitePixelFound:= False;
    for X := 0 to (Bmp.Width - 1) do begin
      if Bmp.Canvas.Pixels[X, Y] <> clWhite then begin
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
  for X := Bmp.Width - 1 downto 1 do begin // 'downto 1' and not 'downto 0' because the reaining image should be at least 1 pixel in width
    IsNonWhitePixelFound:= False;
    for Y := 0 to (Bmp.Height - 1) do begin
      if Bmp.Canvas.Pixels[X, Y] <> clWhite then begin
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

  TmpBitmap := TBitmap.Create;
  try
    TmpBitmap.Canvas.Font.Color:= clblack;
    TmpBitmap.Canvas.Font.Name:= FontName;
    TmpBitmap.Canvas.Font.Size:= FontSize;
    TmpBitmap.Canvas.Font.Bold:= False;
    TmpBitmap.Canvas.Font.Italic:= False;
    TmpBitmap.SetSize(TmpBitmap.Canvas.TextWidth(AText), TmpBitmap.Canvas.TextHeight(AText));
    TmpBitmap.Canvas.AntialiasingMode:= amOff;
    TmpBitmap.Canvas.TextOut(0, 0, AText);
    //TrimBitmap(TmpBitmap);
    if(nil <> FDisplay) then
      FDisplay.PaintBitmap(TmpBitmap, X, Y);
    FVirtualLayer0.Canvas.Draw(X, Y, TmpBitmap);
    CombineVirtualLayers(Self);
    TmpBitmap.Canvas.Pixels[0, 0]:= TmpBitmap.Canvas.Pixels[0, 0]; // this seems like nonsense but is required to actually load the bitmap in memory
    ResultPoint.X:= TmpBitmap.Width;
    ResultPoint.Y:= TmpBitmap.Height;
  finally
    TmpBitmap.Free;
  end;

  Result:= ResultPoint;
end;



procedure TMainForm.Animate(FileName: TFilename; AnimationSpeed, XPosition, YPosition, fWidth: Word);
begin
  try
    AnimateTimer.Enabled:= False;

    FAnimationData.AnimationBitmap.LoadFromFile(FileName);

    FAnimationData.FrameIndex:=  0;
    FAnimationData.FrameCount:= (FAnimationData.AnimationBitmap.Width div fWidth);
    FAnimationData.FrameWidth:=  fWidth;
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
  DrawAnimationFrame(FAnimationData.AnimationBitmap, FAnimationData.XPos, FAnimationData.YPos, FAnimationData.FrameIndex, FAnimationData.FrameWidth);

  Inc(FAnimationData.FrameIndex);
  if (FAnimationData.FrameIndex >= FAnimationData.FrameCount) then
    FAnimationData.FrameIndex:= 0;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  FStudioConfig.ApplicationConfig.Language:= 'it';
  SetDefaultLang(FStudioConfig.ApplicationConfig.Language);
end;

procedure TMainForm.ExpertViewButtonClick(Sender: TObject);
begin
     Panel3.Visible:= not Panel3.Visible;
     Panel5.Visible:= Panel3.Visible;

     if (Panel3.Visible) then begin
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
    TmpBitmap.Height := ABitmap.Height;
    TmpBitmap.Width:= FrameWidth;
    sRect:= Rect(Frame * FrameWidth, 0, Frame * FrameWidth + FrameWidth, ABitmap.Height);
    dRect:= Rect(0, 0, FrameWidth, ABitmap.Height);
    TmpBitmap.Canvas.CopyRect(dRect, ABitmap.Canvas, sRect);
    if(nil <> FDisplay) then
      FDisplay.PaintBitmap(TmpBitmap, X, Y);
    FVirtualLayer0.Canvas.Draw(X, Y, TmpBitmap);
    CombineVirtualLayers(Self);
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

  Item := LogListView.Items.Add;
  Item.Caption:= IntToStr(LogListView.Items.Count);
  Item.Subitems.Add(Format('%.04d-%.02d-%.02d %.02d:%.02d:%.02d.%.03d', [AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond]));
  case LogLevel of
   lvINFO:    Item.Subitems.Add('INFO');
   lvWARNING: Item.Subitems.Add('WARNING');
   lvERROR:   Item.Subitems.Add('ERROR');
   else       Item.Subitems.Add('CRITICAL');
  end;
  Item.Subitems.Add(AText);
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
  if ColorDialog.Execute then begin
    ColorPanel.Color:= ColorDialog.Color;
    FStudioConfig.ApplicationConfig.PreviewDisplayColor:= ColorDialog.Color;
  end;
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
            if(nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].SecondsPoint.X, FClocks[I].SecondsPoint.Y, True);
            FVirtualLayer0.Canvas.Pen.Color:= clWhite;
            FVirtualLayer0.Canvas.Line(X0, Y0, FClocks[I].SecondsPoint.X, FClocks[I].SecondsPoint.Y);
          end;
          // draw new hand
          if(nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FVirtualLayer0.Canvas.Pen.Color:= clBlack;
          FVirtualLayer0.Canvas.Line(X0, Y0, X1, Y1);
          CombineVirtualLayers(Self);
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
            if(nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].MinutePoint.X, FClocks[I].MinutePoint.Y, True);
            FVirtualLayer0.Canvas.Pen.Color:= clWhite;
            FVirtualLayer0.Canvas.Line(X0, Y0, FClocks[I].MinutePoint.X, FClocks[I].MinutePoint.Y);
          end;
          // draw new hand
          if(nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FVirtualLayer0.Canvas.Pen.Color:= clBlack;
          FVirtualLayer0.Canvas.Line(X0, Y0, X1, Y1);
          CombineVirtualLayers(Self);
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
            if(nil <> FDisplay) then
              FDisplay.PaintLine(X0, Y0, FClocks[I].HourPoint.X, FClocks[I].HourPoint.Y, True);
            FVirtualLayer0.Canvas.Pen.Color:= clWhite;
            FVirtualLayer0.Canvas.Line(X0, Y0, FClocks[I].HourPoint.X, FClocks[I].HourPoint.Y);
          end;
          // draw new hand
          if(nil <> FDisplay) then
            FDisplay.PaintLine(X0, Y0, X1, Y1, False);
          FVirtualLayer0.Canvas.Pen.Color:= clBlack;
          FVirtualLayer0.Canvas.Line(X0, Y0, X1, Y1);
          CombineVirtualLayers(Self);
          // remember coordinates
          FClocks[I].HourPoint.X:= X1;
          FClocks[I].HourPoint.Y:= Y1;
        end;

      end; // if HandHandLength > 0

    end;
  end;
end;

procedure TMainForm.RepaintVirtualDisplay;
begin
end;

procedure TMainForm.PaintStringOnVirtualDisplay(AText: string; Col, Row: Integer);
var
  C: Char;         // current character
  Pixels: Byte;    // one vertical block of pixels within a glyph
  X, Y: Integer;   // position of pixel in virtual display layer
  Gx, Gy: Integer; // position within a glyph
  CurrentCol: Integer;  // current column
  CValue: Byte;         // Ord(C)
begin
  CurrentCol:= Col;
  for C in AText do begin
    if (CurrentCol >= (FStudioConfig.DisplayConfig.ResX div (GLYPH_W + GLYPH_GAP))) then
      Break;

    CValue:= Ord(C);
    for Gx:= 0 to (GLYPH_W - 1) do begin
      Pixels:= charMap8x6[CValue, Gx];
      X:= CurrentCol * (GLYPH_W + GLYPH_GAP) + Gx;
      for Gy:= 0 to 7 do begin
        Y:= Row * GLYPH_H + Gy;
        if ((Pixels and (1 shl Gy)) <> 0) then begin
          FVirtualLayer1.Canvas.Pixels[X, Y]:= clBlack;
        end else begin
          FVirtualLayer1.Canvas.Pixels[X, Y]:= clWhite;
        end;
      end;
    end;
    Inc(CurrentCol);
  end;
  CombineVirtualLayers(Self);
end;


procedure TMainForm.CombineVirtualLayers(Sender: TObject);
var
  TmpBitmap: TBitmap;
begin
  CombinedImage.Picture.Bitmap.Canvas.CopyMode:= cmSrcCopy;
  CombinedImage.Picture.Bitmap.Canvas.Draw(0, 0, FVirtualLayer0);
  CombinedImage.Picture.Bitmap.Canvas.CopyMode:= cmSrcInvert;
  CombinedImage.Picture.Bitmap.Canvas.Draw(0, 0, FVirtualLayer1);
  TmpBitmap:= TBitmap.Create;
  TmpBitmap.Width:= CombinedImage.Picture.Bitmap.Width;
  TmpBitmap.Height:= CombinedImage.Picture.Bitmap.Height;
  TmpBitmap.Canvas.Brush.Color:= ColorPanel.Color;
  TmpBitmap.Canvas.FillRect(0, 0, TmpBitmap.Width, TmpBitmap.Height);
  CombinedImage.Picture.Bitmap.Canvas.CopyMode:= cmSrcAnd;
  CombinedImage.Picture.Bitmap.Canvas.Draw(0, 0, TmpBitmap);
  TmpBitmap.Free;
end;

end.

