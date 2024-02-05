unit StudioCommon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TLogLevel = (lvINFO, lvWARNING, lvERROR, lvCRITICAL);

type
  TLayerMode = (lmOR, lmAND, lmXOR);

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

implementation

end.

