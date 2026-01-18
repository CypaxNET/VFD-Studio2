unit SysInfo;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, winsock, ExtCtrls, JwaWinBase, Win32Proc, resource,
  versiontypes, versionresource, ComObj, Variants, Process, ActiveX;

type
  TSysInfo = class(TComponent)
  private
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
    currentCpuUsage: single;
    LastSystemIdleTime, LastSystemKernelTime, LastSystemUserTime: int64;
    function ASCII(s: string): string;
  public
    { Public-Deklarationen }

    function GetOhmValue(OhmComp, OhmType, OhmName: string): string;

    function ResourceVersionInfo: string;

    function GetCurrentUserName: string;
    function ComputerName: string;
    function GetIPAdress: string;

    function GetTotalMemory: QWord;  //physikalischer Speicher
    function GetFreeMemory: QWord;
    function GetTotalVirtualMemory: QWord;  //virtueller Speicher
    function GetFreeVirtualMemory: QWord;
    function GetMemoryUsage: DWord;   //RAM-Nutzung

    function GetCpuUsage: single;
    procedure UpdateCpuUsage;

    function GetCPUSpeed: string;
    function GetCPUVendorIdentifier: string;
    function GetCPUName: string;
    function GetCPUIdentifier: string;

    function GetTimeZone: string;
    function GetDayOfTheWeek: string;
    function GetUptime: string;

    procedure GetPrinterInfos(StrngLst: TStringList);

    function GetPartitionName(const ADrive: char): string;
    function GetDriveTyp(Drive: char): string;
    function GetDiskSpace(Drive: char): QWord;
    function GetFreeDiskSpace(Drive: char): QWord;

    function GetDesktopResolution: string;
    function GetDesktopColors: string;
    function GetScreenFrequency: string; // nur ab WinNT!!!

    function GetMostRecentDirectDraw: string;
    function GetMostRecentDirect3D: string;

    function GetOperatingSystem: string;
    function GetOperatingSystemFullName: string;

    function GetRegistryString(Path, Key: string): string;

  published
    { Published-Deklarationen }
  end;

procedure Register;

implementation


function TSysInfo.GetOhmValue(OhmComp, OhmType, OhmName: string): string;
var
  Locator: olevariant;
  WMIService: olevariant;
  WbemObjectSet: olevariant;
  WbemObject: olevariant;
  Enum: IEnumVariant;
  SensorType, SensorName, SensorParent: string;
  SensorValue: double;
  I: integer;
  ResStr: string;
  Value: cardinal;
begin
  ResStr := '?';
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    WMIService := Locator.ConnectServer(WideString('.'), 'root\OpenHardwareMonitor');

    // OhmComponent should be something like "/intelcpu/0". If it's not, we need to search for it based on the given name:
    if (not OhmComp.StartsWith('/')) then
    begin
      WbemObjectSet := WMIService.ExecQuery(
        'SELECT * FROM Hardware WHERE Name="' + OhmComp + '"');
      Enum := WbemObjectSet._NewEnum;
      if (WbemObjectSet.Count > 0) then
      begin
        while (Enum.Next(1, WbemObject, Value) = S_OK) do
        begin
          OhmComp := WbemObject.Properties_.Item('Identifier').Value;
        end;

      end
      else
      begin
        OhmComp := '';
      end;
    end;

    if (OhmComp <> '') then
    begin

      WbemObjectSet := WMIService.ExecQuery('SELECT * FROM Sensor WHERE SensorType="' +
        OhmType + '" AND Parent="' + OhmComp + '"');
      Enum := WbemObjectSet._NewEnum;
      while (Enum.Next(1, WbemObject, Value) = S_OK) do
      begin
        SensorType := WbemObject.Properties_.Item('SensorType').Value;
        SensorName := WbemObject.Properties_.Item('Name').Value;
        SensorValue := WbemObject.Properties_.Item('Value').Value;
        SensorParent := WbemObject.Properties_.Item('Parent').Value;
        if (SensorName = OhmName) then
        begin
          //ResStr := Format('%.1f', [SensorValue]);
          ResStr := IntToStr(Round(SensorValue));
          Break;
        end;
      end;
    end;
  finally
    Result := Trim(ResStr);
  end;
end;

(*
OBSOLETE
function TSysInfo.GetOhmValue(OhmComp, OhmType, OhmName: String): String;
var
  Cmd: String;
  SensorType, SensorName, SensorParent: String;
  SensorValue: Double;
  I: Integer;
  ResStr: String;
  AProcess: TProcess;
  OutputStream: TMemoryStream;
  Buffer: array[1..2048] of Byte;
  BytesRead: LongInt;
  OutputString: TStringList;
begin
  ResStr := '?';
  AProcess := TProcess.Create(nil);
  try
    AProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    AProcess.Executable := 'wmic.exe';
    // OhmComponent should be something like "/intelcpu/0". If it's not, we need to search for it based on the given name:
    if (not OhmComp.StartsWith('/')) then begin
      Cmd:= '/namespace:\\ROOT\OpenHardwareMonitor path Hardware where "name=''' + OhmComp + '''" get name, identifier';
      AProcess.Parameters.Add(Cmd);
      AProcess.Execute;

      OutputStream := TMemoryStream.Create;
      try
        repeat
          BytesRead := AProcess.Output.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            OutputStream.Write(Buffer, BytesRead);
        until BytesRead = 0;

        OutputStream.Position := 0;
        OutputString := TStringList.Create;
        Application.Title:= Inttostr(OutputString.Count) + ' lines';
        try
          OutputString.LoadFromStream(OutputStream);
          //AOutput.Assign(OutputString);
        finally
          OutputString.Free;
        end;
      finally
        OutputStream.Free;
      end;

    end;

    if (OhmComp <> '') then begin
    end;

  finally
    AProcess.Free;
  end;
  Result:= Trim(ResStr);
end;
*)




function TSysInfo.ResourceVersionInfo: string;
var
  Stream: TResourceStream;
  vr: TVersionResource;
  fi: TVersionFixedInfo;
begin
  Result := '';
  { This raises an exception if version info has not been incorporated into the
    binary (Lazarus Project -> Project Options -> Version Info -> Version numbering). }
  Stream := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
  try
    vr := TVersionResource.Create;
    try
      vr.SetCustomRawDataStream(Stream);
      fi := vr.FixedInfo;
      Result := Trim(Format('%d.%d.%d.%d', [fi.FileVersion[0],
        fi.FileVersion[1], fi.FileVersion[2], fi.FileVersion[3]]));
    finally
      vr.Free
    end;
  finally
    Stream.Free
  end;
end;


function TSysInfo.ASCII(s: string): string;
  // ersetzt ö,ä,ü durch oe,ae,ue zwecks ASCII Kompatiblität
var
  i: integer;
begin
  while pos('ö', s) <> 0 do
  begin
    i := pos('ö', s);
    Delete(s, i, 1);
    insert('oe', s, i);
  end;
  while pos('ä', s) <> 0 do
  begin
    i := pos('ä', s);
    Delete(s, i, 1);
    insert('ae', s, i);
  end;
  while pos('ü', s) <> 0 do
  begin
    i := pos('ü', s);
    Delete(s, i, 1);
    insert('ue', s, i);
  end;
  while pos('Ö', s) <> 0 do
  begin
    i := pos('Ö', s);
    Delete(s, i, 1);
    insert('Oe', s, i);
  end;
  while pos('Ä', s) <> 0 do
  begin
    i := pos('Ä', s);
    Delete(s, i, 1);
    insert('Ae', s, i);
  end;
  while pos('Ü', s) <> 0 do
  begin
    i := pos('Ü', s);
    Delete(s, i, 1);
    insert('Ue', s, i);
  end;
  Result := s;
end;

function TSysInfo.GetDesktopColors: string;
var
  DesktopDC: THandle;
begin
  DesktopDC := GetDC(0);
  Result := Trim(IntToStr(GetDeviceCaps(DesktopDC, BITSPIXEL)) + ' Bit');
  ReleaseDC(0, DesktopDC);
end;

function TSysInfo.GetDesktopResolution: string;
begin
  Result := IntToStr(screen.Width) + 'x' + IntToStr(screen.Height);
end;


function TSysInfo.GetScreenFrequency: string; // nur ab WinNT!!!
var
  DesktopDC: THandle;
begin
  Result := '?';
  DesktopDC := GetDC(0);
  try
    Result := Trim(IntToStr(GetDeviceCaps(DesktopDC, VREFRESH)) + ' Hz');
  except
  end;
  ReleaseDC(0, DesktopDC);
end;


function TSysInfo.ComputerName: string;
var
  size: DWord;
begin
  size := Max_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, Size);
  if GetComputerName(PChar(Result), Size) then
    SetLength(Result, Size)
  else
    Result := '';
  Result := Trim(ASCII(Result));
end;

function TSysInfo.GetCurrentUserName: string;
var
  u: array[0..127] of char;
  sz: DWord;
begin
  Result := '?';
  sz := SizeOf(u);
  GetUserName(u, sz);
  Result := u;
  Result := Trim(ASCII(Result));
end;

function TSysInfo.GetIPAdress: string;
var
  phoste: PHostEnt;
  Buffer: array[0..100] of char;
  WSAData: TWSADATA;
begin
  Result := '';
  if WSAStartup($0101, WSAData) <> 0 then exit;
  GetHostName(Buffer, SizeOf(Buffer));
  phoste := GetHostByName(buffer);
  if phoste = nil then Result := 'IP not found'
  else
    Result := Trim(StrPas(inet_ntoa(PInAddr(phoste^.h_addr_list^)^)));
  WSACleanup;
end;

function TSysInfo.GetMemoryUsage: DWord;
var
  memory: TMemoryStatusEx;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatusEx(memory);
  Result := memory.dwMemoryLoad;
end;

function TSysInfo.GetTotalMemory: QWord;
var
  memory: TMemoryStatusEx;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatusEx(memory);
  Result := memory.ullTotalPhys;
end;

function TSysInfo.GetFreeVirtualMemory: QWord;
var
  memory: TMemoryStatusEx;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatusEx(memory);
  Result := memory.ullAvailVirtual;
end;

function TSysInfo.GetTotalVirtualMemory: QWord;
var
  memory: TMemoryStatusEx;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatusEx(memory);
  Result := memory.ullTotalVirtual;
end;

function TSysInfo.GetFreeMemory: QWord;
var
  memory: TMemoryStatusEx;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatusEx(memory);
  Result := memory.ullAvailPhys;
end;



function TSysInfo.GetCPUSpeed: string;
var
  Reg: TRegistry;
begin
  Result := '?';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Trim(IntToStr(Reg.ReadInteger('~MHz')));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
end;

function TSysInfo.GetCPUVendorIdentifier: string;
var
  Reg: TRegistry;
begin
  Result := '---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Trim(Reg.ReadString('VendorIdentifier'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  Result := Trim(ASCII(Result));
end;



function TSysInfo.GetCpuUsage: single;
begin
  Result := currentCpuUsage;
end;

procedure TSysInfo.UpdateCpuUsage;
var
  CurrentIdleTime, CurrentKernelTime, CurrentUserTime: int64;
  Divisor: int64;
begin
  if GetSystemTimes(@CurrentIdleTime, @CurrentKernelTime, @CurrentUserTime) then
  begin
    // Berechnung der CPU-Auslastung

    Divisor := CurrentUserTime + CurrentKernelTime - LastSystemUserTime -
      LastSystemKernelTime;
    if (Divisor > 0) then
      currentCpuUsage := 1.0 - ((CurrentIdleTime - LastSystemIdleTime) / Divisor);

    // Aktualisierung der letzten Systemzeiten
    LastSystemIdleTime := CurrentIdleTime;
    LastSystemKernelTime := CurrentKernelTime;
    LastSystemUserTime := CurrentUserTime;
  end
  else
    currentCpuUsage := 0.0; // Fehler beim Abrufen der Systemzeiten
end;

function TsysInfo.GetCPUName: string;
var
  Reg: TRegistry;
begin
  Result := '---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Trim(Reg.ReadString('ProcessorNameString'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  Result := Trim(ASCII(Result));
end;


function TSysInfo.GetCPUIdentifier: string;
var
  Reg: TRegistry;
begin
  Result := '---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Trim(Reg.ReadString('Identifier'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  Result := Trim(ASCII(Result));
end;


function TSysInfo.GetTimeZone: string;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZoneInfo);
  Result := TimeZoneInfo.StandardName;
end;

procedure TSysInfo.GetPrinterInfos(StrngLst: TStringList);
var
  reg: TRegistry;       // Registry
  s: string;            // HilfsString
  Printers: TStrings;   // Liste mit PrinteSchlüsseln
  NrOfPrinters: byte;   // Anzahl der Printer
  i: byte;              // Zähler
  Rs: TStrings;         // Ausgabe
begin

  { Initialisierung }
  Printers := TStringList.Create;
  Rs := TStringList.Create;
  s := '';
  reg := TRegistry.Create;

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly('System\CurrentControlSet\Control\Print\Printers');

    if reg.HasSubKeys = True then
    begin

      reg.GetKeyNames(Printers);
      NrOfPrinters := Printers.Count;
      reg.CloseKey;
      reg.Free;
      for i := 1 to NrOfPrinters do
      begin
        reg := TRegistry.Create;
        reg.RootKey := HKEY_LOCAL_MACHINE;
        s := 'System\CurrentControlSet\Control\Print\Printers' + '\' + Printers[i - 1];
        reg.OpenKeyReadOnly(s);
        rs.add(reg.ReadString('Name'));
        rs.add(reg.ReadString('Port'));
        rs.add(reg.ReadString('Share Name'));
        rs.add(reg.ReadString('Printer Driver'));
        reg.CloseKey;
        reg.Free;
      end;  // end for

    end; //end if HasSubKeys
  except   //end try
    Rs.add('---');
  end;

  strnglst.AddStrings(Rs);


  Printers.Free;
  Rs.Free;
end;

function TSysInfo.GetPartitionName(const ADrive: char): string;
var
  tmp: longword;
  buffer: array[0..19] of char;
begin
  GetVolumeInformation(PChar(ADrive + ':\'), @buffer[0],
    SizeOf(buffer),
    nil,
    tmp,
    tmp,
    nil,
    0);
  Result := Trim(buffer);
end;

function TSysInfo.GetDriveTyp(Drive: char): string;
var
  p: array[0..2] of char;
begin
  strpcopy(p, Drive + ':');
  case GetDriveType(p) of
    DRIVE_REMOVABLE: begin
      if (Drive = 'A') or (Drive = 'B') then Result := 'Diskettenlaufwerk'
      else
        Result := 'RamDisk';
    end;
    DRIVE_FIXED: Result := 'Festplatte';
    DRIVE_REMOTE: Result := 'Netzlaufwerk';
    DRIVE_RAMDISK: Result := 'RamDisk';
    DRIVE_CDROM: Result := 'CD-ROM';
    1: Result := 'nicht vorhanden';
    else
      Result := '?';
  end; //case
end;

function TSysInfo.GetDayOfTheWeek: string;
begin
  case DayOfWeek(now) of
    1: Result := 'Sonntag';
    2: Result := 'Montag';
    3: Result := 'Dienstag';
    4: Result := 'Mittwoch';
    5: Result := 'Donnerstag';
    6: Result := 'Freitag';
    7: Result := 'Samstag';
  end;
end;


function TSysInfo.GetUptime: string;
var
  d, h, m, s: integer;
  uptime: longint;
  uptimedate: TDateTime;
begin
  uptime := GetTickCount;

  uptime := trunc(uptime / 1000);

  s := uptime mod 60;
  uptime := uptime div 60;

  m := uptime mod 60;
  uptime := uptime div 60;

  h := uptime mod 60;
  uptime := uptime div 60;

  d := uptime mod 24;


  uptimedate := encodeTime(h, m, s, 0);

  if d > 0 then Result := IntToStr(d) + ' Tage, ' + TimeToStr(uptimedate)
  else
    Result := TimeToStr(uptimedate);
end;



function TSysInfo.GetMostRecentDirectDraw: string;
var
  Reg: TRegistry;
begin
  Result := '?';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Software\Microsoft\DirectDraw\MostRecentApplication') then
    begin
      Result := Trim(Reg.ReadString('Name'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  Result := Trim(ASCII(Result));

end;


function TSysInfo.GetMostRecentDirect3D: string;
var
  Reg: TRegistry;
begin
  Result := '?';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Direct3D\MostRecentApplication') then
    begin
      Result := Trim(Reg.ReadString('Name'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  Result := Trim(ASCII(Result));
end;


function TSysInfo.GetOperatingSystemFullName: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion') then
    begin
      Result := Trim(Reg.ReadString('ProductName'));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
end;

// Operating system short name
function TSysInfo.GetOperatingSystem: string;
var
  OSVersion: string;
begin
  if WindowsVersion = wv95 then OSVersion := 'Windows 95'
  else if WindowsVersion = wvNT4 then OSVersion := 'Windows NT v.4'
  else if WindowsVersion = wv98 then OSVersion := 'Windows 98'
  else if WindowsVersion = wvMe then OSVersion := 'Windows ME'
  else if WindowsVersion = wv2000 then OSVersion := 'Windows 2000'
  else if WindowsVersion = wvXP then OSVersion := 'Windows XP'
  else if WindowsVersion = wvServer2003 then OSVersion := 'Windows Server 2003'
  else if WindowsVersion = wvVista then OSVersion := 'Windows Vista'
  else if WindowsVersion = wv7 then OSVersion := 'Windows 7'
  else if WindowsVersion = wv8 then OSVersion := 'Windows 8'
  else if WindowsVersion = wv8_1 then OSVersion := 'Windows 8.1'
  else if WindowsVersion = wv10 then OSVersion := 'Windows 10'
  else if WindowsVersion = wv11 then OSVersion := 'Windows 11'
  else
    OSVersion := 'Windows';

  Result := Trim(OSVersion);
end;

function TSysInfo.GetRegistryString(Path, Key: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(Path) then
    begin
      Result := Trim(Reg.ReadString(Key));
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
end;

function TSysInfo.GetFreeDiskSpace(Drive: char): QWord;
var
  FtCBytes: ULARGE_INTEGER;
  TotalBytes: ULARGE_INTEGER;
  FreeBytes: ULARGE_INTEGER;
begin
  if GetDiskFreeSpaceEx(PChar(Drive + ':\'), FtCBytes, TotalBytes, @FreeBytes) then
    GetFreeDiskSpace := FreeBytes.QuadPart
  else
    GetFreeDiskSpace := 0;
end;


function TSysInfo.GetDiskSpace(Drive: char): QWord;
var
  FtCBytes: ULARGE_INTEGER;
  TotalBytes: ULARGE_INTEGER;
  FreeBytes: ULARGE_INTEGER;
begin
  if GetDiskFreeSpaceEx(PChar(Drive + ':\'), FtCBytes, TotalBytes, @FreeBytes) then
    GetDiskSpace := TotalBytes.QuadPart
  else
    GetDiskSpace := 0;
end;


procedure Register;
begin
  RegisterComponents('Hardware', [TSysInfo]);
end;

end.
