program VFDStudio2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrameViewer09, MainUnit, SysInfo, uSMBIOS, WinampControl, SettingsForm,
  InfoUnit, StudioCommon, DisplayManager, Glyphs, NTK300, NTK800, VFDisplay,
  PreviewDisplay, U8G2;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='VFD-Studio 2';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.
