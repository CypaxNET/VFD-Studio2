!include "MUI2.nsh"

!define INSTALLERVERSION "1.0.0.0"

!define /file BUNDLEVERSION "..\BUNDLEVERSION.txt"
!searchreplace BUNDLEVERSION_HYPENATED ${BUNDLEVERSION} "." "-"
!searchreplace BUNDLEVERSION_NOQUOTES ${BUNDLEVERSION} "'" ""

!define NAME "VFD-Studio 2"
!define DESCRIPTION "My awesome application"

!define REGPATH_UNINSTSUBKEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"

Name "${NAME}"

OutFile "vfdstudio-installer_${BUNDLEVERSION_HYPENATED}.exe"


RequestExecutionLevel User ; We don't need UAC elevation
InstallDir "" ; Don't set a default $InstDir so we can detect /D= and InstallDirRegKey

!include LogicLib.nsh
!include WinCore.nsh
!include Integration.nsh

LicenseData "..\..\LICENSE"


!define MUI_ICON "..\vfdstudio\VFDStudio2.ico"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "..\..\resources\logo.bmp"
!define MUI_HEADERIMAGE_RIGHT

!define MUI_WELCOMEPAGE_TITLE  "VFD-Studio ${BUNDLEVERSION_NOQUOTES} Setup"

!define MUI_WELCOMEPAGE_TEXT  "Setup will install VFD-Studio version ${BUNDLEVERSION_NOQUOTES}.$\r$\n$\r$\nCheck the GitHub project page from the link below for newer versions.$\r$\n$\r$\nClick Next to continue."

!define MUI_PAGE_CUSTOMFUNCTION_SHOW WelcomePageShow
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "../../LICENSE"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_UNPAGE_COMPONENTS

; Function to be called when the Welcome page is shown
Function WelcomePageShow
  Pop $0 ; don't forget to pop HWND of the stack
  
  ; place a link at the bottom of the page
  ${NSD_CreateLink} 120u 90% -100u 12u "https://github.com/CypaxNET/VFD-Studio2/"
  Pop $R9
  ${NSD_OnClick} $R9 onLinkClick

  ; Make the background color of the link transparent
  SetCtlColors $R9 0x0066CC transparent

  ; ^Font and ^FontSize are LangString vars containing the installer's set font and font size
  ;Create a new font based on it that is underlined ( Font 'weight' of 400 = regular )
  CreateFont $1 "$(^Font)" "$(^FontSize)" "400" /UNDERLINE
  ; and assign the font to the link
  SendMessage $R9 ${WM_SETFONT} $1 1

FunctionEnd

; Function to be called when the Link on the Welcome page is clicked
Function onLinkClick
  Pop $0 ; don't forget to pop HWND of the stack
  ExecShell "open" "https://github.com/CypaxNET/VFD-Studio2/"
FunctionEnd


!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!define MUI_FINISHPAGE_RUN "$INSTDIR\VFDStudio2.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Run VFD-Studio 2 now"
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"


;--------------------------------
;Version Information

  VIProductVersion "${INSTALLERVERSION}"  ; file version
  VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "VFD-Studio 2"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "cypax.net"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "Copyright by cypax.net"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "VFD-Studio Installer"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "${INSTALLERVERSION}"

;--------------------------------


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

Section "Program files (Required)"  SecPrg
  SectionIn Ro
  
  SetOutPath $INSTDIR
  
  File "..\vfdstudio\VFDStudio2.exe"
  File "..\vfdstudio\ListEditor.exe"
  
  File "..\vfdstudio\1.ico"
  File "..\vfdstudio\2.ico"
  File "..\vfdstudio\3.ico"
  File "..\vfdstudio\4.ico"
  File "..\vfdstudio\VFDStudio2.ico"
  
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
  
  ; remove previous installation from autostart
  DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run" "VFD-Studio 2"
  
SectionEnd

Section "Add to autostart" SecAutostart
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Run" "VFD-Studio 2" "$INSTDIR\VFDStudio2.exe"
SectionEnd

; LISTS
Section ""
  SetOutPath $INSTDIR\Lists 
  File "..\vfdstudio\Lists\Default.vfdlst"
  File "..\vfdstudio\Lists\left-eye.vfdlst"
  File "..\vfdstudio\Lists\right-eye.vfdlst"
  File "..\vfdstudio\Lists\Features_128x64.vfdlst"
SectionEnd

; BITMAPS / ANIMATIONS
Section ""
  SetOutPath $INSTDIR\Bitmaps 
  File "..\vfdstudio\Bitmaps\VFDStudio128x17.bmp"
  File "..\vfdstudio\Bitmaps\clockface31x31.bmp"
  File "..\vfdstudio\Bitmaps\clockface64x64.bmp"
  File "..\vfdstudio\Bitmaps\column8x22.bmp"
  SetOutPath $INSTDIR\Bitmaps\128x64
  File "..\vfdstudio\Bitmaps\128x64\l1.bmp"
  File "..\vfdstudio\Bitmaps\128x64\r1.bmp"
  SetOutPath $INSTDIR\Bitmaps\animations
  File "..\vfdstudio\Bitmaps\animations\lefteye128x64_7.bmp"
  File "..\vfdstudio\Bitmaps\animations\righteye128x64_7.bmp"
  File "..\vfdstudio\Bitmaps\animations\cursor8x1_2.bmp"
  File "..\vfdstudio\Bitmaps\animations\robot46x48_10.bmp"
  File "..\vfdstudio\Bitmaps\animations\globe32x32_30.bmp"
  File "..\vfdstudio\Bitmaps\animations\globe64x64_30.bmp"
SectionEnd

; LANGUAGES
Section ""
  SetOutPath $INSTDIR\languages 
  File "..\vfdstudio\languages\*.mo"
SectionEnd

; ARDUINO VFD DRIVER
Section "Arduino VFD driver" SecVfdDriver
  SetOutPath $INSTDIR\ArduinoNTKDriver 
  File "..\ArduinoNTKDriver\ArduinoNTKDriver.ino"
  File "..\ArduinoNTKDriver\ntk_commands.h"
SectionEnd

; ARDUINO LCD/OLED DRIVER
Section "Arduino LCD/OLED driver (SSD1309 example)" SecDspDriver
  SetOutPath $INSTDIR\ArduinoSSD1309Driver 
  File "..\ArduinoSSD1309Driver\ArduinoSSD1309Driver.ino"
  File "..\ArduinoSSD1309Driver\glyph.h"
SectionEnd

Section "Start Menu Shortcuts" SecStartMenu
  CreateDirectory "$SMPROGRAMS\${NAME}"
  CreateShortCut "$SMPROGRAMS\${NAME}\Uninstall.lnk" "$INSTDIR\Uninst.exe" "" "$INSTDIR\Uninst.exe" 0
  CreateShortCut "$SMPROGRAMS\${NAME}\VFD-Studio2.lnk" "$INSTDIR\VFDStudio2.exe" "" "$INSTDIR\VFDStudio2.exe" 0
  CreateShortCut "$SMPROGRAMS\${NAME}\ListEditor.lnk" "$INSTDIR\ListEditor.exe" "" "$INSTDIR\ListEditor.exe" 0
SectionEnd


;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecPrg ${LANG_ENGLISH} "VFD-Studio2 application, Editor for list files, a set of predefined lists and bitmaps."
  LangString DESC_SecStartMenu ${LANG_ENGLISH} "Add VFD-Studio 2 to the Windows start menu."
  LangString DESC_SecAutostart ${LANG_ENGLISH} "Start VFD-Studio 2 when Windows starts."
  LangString DESC_SecVfdDriver ${LANG_ENGLISH} "Install the Arduino driver program for VFDs (VF-Displays)."
  LangString DESC_SecDspDriver ${LANG_ENGLISH} "Install the Arduino driver program for LCDs / OLED displays."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecPrg} $(DESC_SecPrg)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecStartMenu} $(DESC_SecStartMenu)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecAutostart} $(DESC_SecAutostart)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVfdDriver} $(DESC_SecVfdDriver)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDspDriver} $(DESC_SecDspDriver)
!insertmacro MUI_FUNCTION_DESCRIPTION_END



/* UNINSTALL */

Section /o "un.Delete settings"
  Delete "$INSTDIR\*.ini"
SectionEnd

Section /o "un.Delete all List Files in installation directory"
  Delete "$INSTDIR\Lists\*.vfdlst"
  RMDir "$INSTDIR\Lists"
SectionEnd

Section /o "un.Delete all images in installation directory"
  Delete "$INSTDIR\Bitmaps\128x64\*"
  Delete "$INSTDIR\Bitmaps\256x64\*"
  Delete "$INSTDIR\Bitmaps\animations\*"
  Delete "$INSTDIR\Bitmaps\*"
  RMDir "$INSTDIR\Bitmaps\128x64"  
  RMDir "$INSTDIR\Bitmaps\256x64"  
  RMDir "$INSTDIR\Bitmaps\animations"  
  RMDir "$INSTDIR\Bitmaps"  
SectionEnd

Section "un.Uninstall application files"
  SectionIn Ro
  
  ; unistall binaries
  Delete "$InstDir\Uninst.exe"
  Delete "$INSTDIR\VFDStudio2.exe"
  Delete "$INSTDIR\ListEditor.exe"
  ; icons
  Delete "$INSTDIR\1.ico"  
  Delete "$INSTDIR\2.ico"  
  Delete "$INSTDIR\3.ico"  
  Delete "$INSTDIR\4.ico"  
  Delete "$INSTDIR\VFDStudio2.ico"
  ; log files
  Delete "$INSTDIR\*.log" 
  ; language files
  Delete "$INSTDIR\languages\*.mo" 
  RMDir "$INSTDIR\languages"
  ; Arduino stuff
  Delete "$INSTDIR\ArduinoNTKDriver\ArduinoNTKDriver.ino"
  Delete "$INSTDIR\ArduinoNTKDriver\ntk_commands.h"
  RMDir "$INSTDIR\ArduinoNTKDriver"
  Delete "$INSTDIR\\ArduinoSSD1309Driver\ArduinoSSD1309Driver.ino"
  Delete "$INSTDIR\ArduinoSSD1309Driver\glyph.h"  
  RMDir "$INSTDIR\ArduinoSSD1309Driver"
  
  ; autostart entry
  RMDir $INSTDIR
  DeleteRegKey HKCU "${REGPATH_UNINSTSUBKEY}"
  DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run" "VFD-Studio 2"
  
  ; program menu entry
  ${UnpinShortcut} "$SMPrograms\${NAME}.lnk"
  Delete "$SMPrograms\${NAME}.lnk"
  RMDir "$SMPROGRAMS\${NAME}" 
SectionEnd