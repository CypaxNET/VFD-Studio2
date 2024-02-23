!include "MUI2.nsh"

!define NAME "VFD-Studio 2"
!define REGPATH_UNINSTSUBKEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"

Name "${NAME}"

OutFile "vfdstudio-installer-2-0-0-0.exe"
RequestExecutionLevel User ; We don't need UAC elevation
InstallDir "" ; Don't set a default $InstDir so we can detect /D= and InstallDirRegKey

!include LogicLib.nsh
!include WinCore.nsh
!include Integration.nsh

LicenseData "..\LICENSE"


!define MUI_ICON "..\source\VFDStudio2.ico"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "..\resources\logo.bmp"
!define MUI_HEADERIMAGE_RIGHT


;!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "../LICENSE"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!define MUI_FINISHPAGE_RUN "$INSTDIR\VFDStudio2.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Run VFD-Studio 2 now"
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

Function .onInit
  SetShellVarContext Current

  ${If} $InstDir == "" ; No /D= nor InstallDirRegKey?
    GetKnownFolderPath $InstDir ${FOLDERID_UserProgramFiles} ; This folder only exists on Win7+
    StrCmp $InstDir "" 0 +2 
    StrCpy $InstDir "$LocalAppData\Programs" ; Fallback directory

    StrCpy $InstDir "$InstDir\$(^Name)"
  ${EndIf}
FunctionEnd

Function un.onInit
  SetShellVarContext Current
FunctionEnd


/* INSTALL */

Section "Program files (Required)"
  SectionIn Ro
  
  SetOutPath $INSTDIR
  
  File "..\source\VFDStudio2.exe"
  File "..\source\ListEditor.exe"
  
  SetOverwrite off ; we don't want to overwrite user settings
    File "vfdstudio.ini"
    File "listeditor.ini"
  SetOverwrite on  
  
  WriteUninstaller "$InstDir\Uninst.exe"
  
  WriteUninstaller "$InstDir\Uninst.exe"
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "DisplayName" "${NAME}"
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "DisplayIcon" "$InstDir\VFDStudio2.exe,0"
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "UninstallString" '"$InstDir\Uninst.exe"'
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "QuietUninstallString" '"$InstDir\Uninst.exe" /S'
  WriteRegDWORD HKCU "${REGPATH_UNINSTSUBKEY}" "NoModify" 1
  WriteRegDWORD HKCU "${REGPATH_UNINSTSUBKEY}" "NoRepair" 1  
  
SectionEnd

; LISTS
Section ""
  SetOutPath $INSTDIR\Lists 
  File "..\source\Lists\Default.vfdlst"
SectionEnd

; BITMAPS / ANIMATIONS
Section ""
  SetOutPath $INSTDIR\Bitmaps 
  File "..\source\Bitmaps\VFDStudio128x17.bmp"
SectionEnd


; LANGUAGES
Section ""
  SetOutPath $INSTDIR\languages 
  File "..\source\languages\*.mo"
SectionEnd


Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\${NAME}"
  CreateShortCut "$SMPROGRAMS\${NAME}\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\${NAME}\VFD-Studio2.lnk" "$INSTDIR\VFDStudio2.exe" "" "$INSTDIR\VFDStudio2.exe" 0
  CreateShortCut "$SMPROGRAMS\${NAME}\ListEditor.lnk" "$INSTDIR\ListEditor.exe" "" "$INSTDIR\ListEditor.exe" 0
SectionEnd


/* UNINSTALL */

Section "Uninstall"
  Delete "$InstDir\Uninst.exe"
  Delete "$INSTDIR\VFDStudio2.exe"
  Delete "$INSTDIR\ListEditor.exe"
  Delete "$INSTDIR\vfdstudio.ini"
  Delete "$INSTDIR\listeditor.ini"
  Delete "Lists\Default.vfdlst"
  Delete "Bitmaps\VFDStudio128x17.bmp"
  RMDir $INSTDIR
  DeleteRegKey HKCU "${REGPATH_UNINSTSUBKEY}"

  ${UnpinShortcut} "$SMPrograms\${NAME}.lnk"
  Delete "$SMPrograms\${NAME}.lnk"
  RMDir "$SMPROGRAMS\${NAME}" 
SectionEnd
