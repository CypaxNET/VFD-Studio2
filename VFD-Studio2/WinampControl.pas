{
TWinampControl 0.99 - By SpECTre (spectre_coder@hotmail.com

Function Explanation:

Note: The function IsWinampRunning is the only function that works if winamp isnt running,
so use this before all other functions - if you use another function and winamp isnt running,
the function will either return false, an empty string or 0.
Functions with a boolean result will return true if winamp is open and false if winamp is not running.
That means that you cant really tell if a command has been succesfully executed.
That goes as well for functions with integer or string results - if result is 0 or an empty string,
winamp is not running or the result of the function is 0 or an empty string

GetVersion: Gets the winamp version
GetTrackTitle: Gets the title of the currently playing track
IsWinampRunning: Returns true if winamp is running
IsPlaying: Returns true if winamp is currently playing
IsPaused: Returns true if winamp is currently paused
NextTrack: Skips to next track
PrevTrack: Skips to previous track
Stop: Stops playback
Pause: Pauses playback
StartPlay: If winamp is playing, restarts track - if winamp is not playing, starts playback
FadeOutAndStop: Fades out the current track and stops playback
StopAfterTrack: Stops playback when the current track has finished playing
FastForward: Skips 5 seconds forward
FastRewind: Rewinds 5 seconds
GetLength: Gets length of current track in second
GetOffset: Gets current offset (number of seconds played of current track)
Seek(Offset: integer): Sets offset to Offset seconds
StartOfPL: Goes to start of playlist
EndOfPL: Goes to end of playlist
ExecVISPlugin: Executes the selected Visualisation plug-in
ToggleSOT: Toggles "Stay-on-top"
ToggleDoubleSize: Toggles DoubleSize
ToggleEqualizer: Shows/hides equalizer
TogglePlayList: Shows/hides playlist editor
ToggleMainWindow: Shows/hides main window
ToggleMiniBrowser: Shows/hides mini browser
VolumeUp: Raises volume by 1%
VolumeDown: Lowers volume by 1%
SetVolume(Volume: byte): Sets volume (0 is minimum - 255 is maximum)
SetBalance(Balance: byte): Sets balance/panning (0 is left - 128 is center - 255 is right)
ToggleRepeat: Sets the repeat function on/off
ToggleShuffle: Sets the shuffle function on/off
CloseWinamp: Closes Winamp
RestartWinamp: Restarts Winamp

Enjoy!

If you use this component, please send an email to spectre_coder@hotmail.com and tell me.
It is the only thing i want for this TWinampControl component.

- SpECTre

}

unit WinampControl;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TWinampControl = class(TComponent)
  private
   function SendToWA(Msg: Cardinal; wParam: integer; lParam: integer): boolean;
   function SendToWAResult(Msg: Cardinal; wParam: integer; lParam: integer; var value: integer): boolean;
  protected
    { Protected declarations }
  public
   function GetVersion: string;
   function GetTrackTitle: string;
   function IsWinampRunning: boolean;
   function IsPlaying: boolean;
   function IsPaused: boolean;
   function NextTrack: boolean;
   function PrevTrack: boolean;
   function Stop: boolean;
   function Pause: boolean;
   function StartPlay: boolean;
   function FadeOutAndStop: boolean;
   function StopAfterTrack: boolean;
   function FastForward: boolean;
   function FastRewind: boolean;
   function GetLength: integer;
   function GetOffset: integer;
   function Seek(Offset: integer): boolean;
   function StartOfPL: boolean;
   function EndOfPL: boolean;
   function ExecVISPlugin: boolean;
   function ToggleSOT: boolean;
   function ToggleDoubleSize: boolean;
   function ToggleEqualizer: boolean;
   function TogglePlayList: boolean;
   function ToggleMainWindow: boolean;
   function ToggleMiniBrowser: boolean;
   function VolumeUp: boolean;
   function VolumeDown: boolean;
   function SetVolume(Volume: byte): boolean;
   function SetBalance(Balance: byte): boolean;
   function ToggleRepeat: boolean;
   function ToggleShuffle: boolean;
   function CloseWinamp: boolean;
   function RestartWinamp: boolean;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SpECTre', [TWinampControl]);
end;

function TWinampControl.SendToWA(Msg: Cardinal; wParam: integer; lParam: integer): boolean;
var
 Handle: HWND;
begin
Handle:=FindWindow('Winamp v1.x',nil);
if Handle<>0 then
 begin
  SendMessage(Handle, Msg, wParam, lParam);
  Result:=true;
 end
else Result:=false;
end;

function TWinampControl.SendToWAResult(Msg: Cardinal; wParam: integer; lParam: integer; var value: integer): boolean;
var
 Handle: HWND;
begin
value:=0;
Handle:=FindWindow('Winamp v1.x',nil);
if Handle<>0 then
 begin
  value:=SendMessage(Handle, Msg, wParam, lParam);
  Result:=true;
 end
else Result:=false;
end;

function TWinampControl.GetVersion: string;
var
 i: integer;
begin
result:='';
if SendToWAResult(WM_USER,0,0,i) then
 result:=copy(inttohex(i,4),1,1)+'.'+copy(inttohex(i,4),2,1)+copy(inttohex(i,4),4,1);
end;

function TWinampControl.GetTrackTitle: string;
var
 P: PChar;
 Handle: HWND;
begin
Result:='';
getmem(p,255);
Handle:=FindWindow('Winamp v1.x',nil);
if Handle<>0 then
 begin
  GetWindowText(Handle,P,255);
  if pos('- Winamp',P)>0 then
   begin
    Result:=Copy(P,1,-2+Length(P)-(Length(P)-Pos('- Winamp',P)));
    Delete(Result,1,Pos(' ',Result));
   end;
 end;
end;

function TWinampControl.IsWinampRunning: boolean;
begin
Result:=SendToWA(0,0,0);
end;

function TWinampControl.IsPlaying: boolean;
var
 i: integer;
begin
SendToWAResult(WM_USER,0,104,i);
if i=1 then result:=true
 else result:=false;
end;

function TWinampControl.IsPaused: boolean;
var
 i: integer;
begin
SendToWAResult(WM_USER,0,104,i);
if i=3 then result:=true
 else result:=false;
end;

function TWinampControl.NextTrack: boolean;
begin
Result:=SendToWA(WM_COMMAND,40048,0);
end;

function TWinampControl.PrevTrack: boolean;
begin
Result:=SendToWA(WM_COMMAND,40044,0);
end;

function TWinampControl.Stop: boolean;
begin
Result:=SendToWA(WM_COMMAND,40047,0);
end;

function TWinampControl.Pause: boolean;
begin
Result:=SendToWA(WM_COMMAND,40046,0);
end;

function TWinampControl.StartPlay: boolean;
begin
Result:=SendToWA(WM_COMMAND,40045,0);
end;

function TWinampControl.FadeOutAndStop: boolean;
begin
Result:=SendToWA(WM_COMMAND,40147,0);
end;

function TWinampControl.StopAfterTrack: boolean;
begin
Result:=SendToWA(WM_COMMAND,40157,0);
end;

function TWinampControl.FastForward: boolean;
begin
Result:=SendToWA(WM_COMMAND,40148,0);
end;

function TWinampControl.FastRewind: boolean;
begin
Result:=SendToWA(WM_COMMAND,40144,0);
end;

function TWinampControl.GetLength: integer;
var
 i: integer;
begin
SendToWAResult(WM_USER,1,105,i);
Result:=i;
end;

function TWinampControl.GetOffset: integer;
var
 i: integer;
begin
SendToWAResult(WM_USER,0,105,i);
Result:=i*1000;
end;

function TWinampControl.Seek(Offset: integer): boolean;
begin
Result:=SendToWA(WM_USER,Offset*1000,106);
end;

function TWinampControl.StartOfPL: boolean;
begin
Result:=SendToWA(WM_COMMAND,40154,0);
end;

function TWinampControl.EndOfPL: boolean;
begin
Result:=SendToWA(WM_COMMAND,40158,0);
end;

function TWinampControl.ExecVISPlugin: boolean;
begin
Result:=SendToWA(WM_COMMAND,40192,0);
end;

function TWinampControl.ToggleSOT: boolean;
begin
Result:=SendToWA(WM_COMMAND,40019,0);
end;

function TWinampControl.ToggleDoubleSize: boolean;
begin
Result:=SendToWA(WM_COMMAND,40165,0);
end;

function TWinampControl.ToggleEqualizer: boolean;
begin
Result:=SendToWA(WM_COMMAND,40036,0);
end;

function TWinampControl.TogglePlayList: boolean;
begin
Result:=SendToWA(WM_COMMAND,40040,0);
end;

function TWinampControl.ToggleMainWindow: boolean;
begin
Result:=SendToWA(WM_COMMAND,40258,0);
end;

function TWinampControl.ToggleMiniBrowser: boolean;
begin
Result:=SendToWA(WM_COMMAND,40298,0);
end;

function TWinampControl.VolumeUp: boolean;
begin
Result:=SendToWA(WM_COMMAND,40058,0);
end;

function TWinampControl.VolumeDown: boolean;
begin
Result:=SendToWA(WM_COMMAND,40059,0);
end;

function TWinampControl.SetVolume(Volume: byte): boolean;
begin
Result:=SendToWA(WM_USER,Volume,122);
end;

function TWinampControl.SetBalance(Balance: byte): boolean;
begin
Result:=SendToWA(WM_USER,Balance,123);
end;

function TWinampControl.ToggleRepeat: boolean;
begin
Result:=SendToWA(WM_COMMAND,40022,0);
end;

function TWinampControl.ToggleShuffle: boolean;
begin
Result:=SendToWA(WM_COMMAND,40023,0);
end;

function TWinampControl.CloseWinamp: boolean;
begin
Result:=SendToWA(WM_COMMAND,40001,0);
end;

function TWinampControl.RestartWinamp: boolean;
begin
Result:=SendToWA(WM_USER,0,135);
end;

end.
