program vfdstudio2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, VFDisplay, NTK800, NTK300, SysInfo, uSMBIOS, WinampControl,
  InfoUnit, Glyphs, StudioCommon;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='VFD-Studio 2';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.

