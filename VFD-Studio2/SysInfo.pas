unit SysInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, registry, winsock, ExtCtrls, JwaWinBase;
type
  TSysInfo = class(TComponent)
  private
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
    currentCpuUsage: Single;
    LastSystemIdleTime, LastSystemKernelTime, LastSystemUserTime: Int64;
    function ASCII(s: string): string;
  public
    { Public-Deklarationen }

    function GetCurrentUserName: string;
    function ComputerName: string;
    function GetIPAdress: string;

    function GetTotalMemory: QWord;  //physikalischer Speicher
    function GetFreeMemory: QWord;
    function GetTotalVirtualMemory: QWord;  //virtueller Speicher
    function GetFreeVirtualMemory: QWord;
    function GetMemoryUsage: DWord;   //RAM-Nutzung

    function GetCpuUsage: Single;
    procedure UpdateCpuUsage;

    function GetCPUSpeed: string;
    function GetCPUVendorIdentifier: string;
    function GetCPUName: string;
    function GetCPUIdentifier: string;

    function GetTimeZone: string;
    function GetDayOfTheWeek:string;
    function GetUptime:string;

    procedure GetPrinterInfos(StrngLst: TStringList);

    function GetPartitionName(const ADrive: Char): String;
    function GetDriveTyp(Drive: Char): string;

    function GetDesktopResolution: string;
    function GetDesktopColors: string;
    function GetScreenFrequency: string; // nur ab WinNT!!!

    function GetMostRecentDirectDraw: string;
    function GetMostRecentDirect3D: string;

    function GetOperatingSystem: String;

  published
    { Published-Deklarationen }
  end;

procedure Register;

implementation


function TSysInfo.ASCII(s: string): string;     // ersetzt ö,ä,ü durch oe,ae,ue zwecks ASCII Kompatiblität
var
   i: integer;
begin
     while pos('ö',s)<>0 do begin
      i:=pos('ö',s);
      delete(s,i,1);
      insert('oe',s,i);
     end;
     while pos('ä',s)<>0 do begin
      i:=pos('ä',s);
      delete(s,i,1);
      insert('ae',s,i);
     end;
     while pos('ü',s)<>0 do begin
      i:=pos('ü',s);
      delete(s,i,1);
      insert('ue',s,i);
     end;
     while pos('Ö',s)<>0 do begin
      i:=pos('Ö',s);
      delete(s,i,1);
      insert('Oe',s,i);
     end;
     while pos('Ä',s)<>0 do begin
      i:=pos('Ä',s);
      delete(s,i,1);
      insert('Ae',s,i);
     end;
     while pos('Ü',s)<>0 do begin
      i:=pos('Ü',s);
      delete(s,i,1);
      insert('Ue',s,i);
     end;
     result:=s;
end;

function TSysInfo.GetDesktopColors: string;
var
   DesktopDC: THandle;
begin
     DesktopDC:=GetDC(0);
     result:=inttostr(GetDeviceCaps(DesktopDC,BITSPIXEL))+ ' Bit';
     ReleaseDC(0,DesktopDC);
end;

function TSysInfo.GetDesktopResolution: string;
begin
     result:=inttostr(screen.width)+'x'+inttostr(screen.height);
end;


function TSysInfo.GetScreenFrequency: string; // nur ab WinNT!!!
var
   DesktopDC: THandle;
begin
     result:='unbekannt';
     DesktopDC:=GetDC(0);
     try
      result:=inttostr(GetDeviceCaps(DesktopDC,VREFRESH))+' Hz';
     except
     end;
     ReleaseDC(0,DesktopDC);
end;


function TSysInfo.ComputerName: string;
var
   size: DWord;
begin
     size:=Max_COMPUTERNAME_LENGTH+1;
     SetLength(Result, Size);
     if GetComputerName(PChar(Result), Size) then
        SetLength(Result, Size)
     else Result:='';
     result:=ASCII(result);
end;

function TSysInfo.GetCurrentUserName: string;
var
   u: array[0..127] of Char;
   sz: DWord;
begin
     Result:='Unbekannter User';
     sz:=SizeOf(u);
     GetUserName(u,sz);
     Result:=u;
     result:=ASCII(result);
end;

function TSysInfo.GetIPAdress: string;
var
   phoste: PHostEnt;
   Buffer: array[0..100] of Char;
   WSAData: TWSADATA;
begin
     result:='';
     if WSAStartup($0101, WSAData)<>0 then exit;
     GetHostName(Buffer, SizeOf(Buffer));
     phoste:=GetHostByName(buffer);
     if phoste = nil then result:='IP not found'
     else result:=StrPas(inet_ntoa(PInAddr(phoste^.h_addr_list^)^));
     WSACleanup;
end;

function TSysInfo.GetMemoryUsage: DWord;
var
  memory: TMemoryStatus;
begin
  memory.dwLength:=SizeOf(memory);
  GlobalMemoryStatus(memory);
  result:=memory.dwMemoryLoad;
end;

function TSysInfo.GetTotalMemory: QWord;
var
  memory: TMemoryStatus;
begin
  memory.dwLength:=SizeOf(memory);
  GlobalMemoryStatus(memory);
  result:=memory.dwTotalPhys;
end;

function TSysInfo.GetFreeVirtualMemory: QWord;
var
  memory: TMemoryStatus;
begin
  memory.dwLength:=SizeOf(memory);
  GlobalMemoryStatus(memory);
  result:=memory.dwAvailVirtual;
end;

function TSysInfo.GetTotalVirtualMemory: QWord;
var
   memory: TMemoryStatus;
begin
  memory.dwLength:=SizeOf(memory);
  GlobalMemoryStatus(memory);
  result:=memory.dwTotalVirtual;
end;

function TSysInfo.GetFreeMemory: QWord;
var
   memory: TMemoryStatus;
begin
  memory.dwLength:=SizeOf(memory);
  GlobalMemoryStatus(memory);
  result:=memory.dwAvailPhys;
end;



function TSysInfo.GetCPUSpeed: string;
var
  Reg: TRegistry;
begin
  Result:='unbekannt';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := IntToStr(Reg.ReadInteger('~MHz'));
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
  Result:='---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Reg.ReadString('VendorIdentifier');
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  result:=ASCII(result);
end;



function TSysInfo.GetCpuUsage: Single;
begin
  Result:= currentCpuUsage;
end;

procedure TSysInfo.UpdateCpuUsage;
var
  CurrentIdleTime, CurrentKernelTime, CurrentUserTime: Int64;
  Divisor: Int64;
begin
  if GetSystemTimes(@CurrentIdleTime, @CurrentKernelTime, @CurrentUserTime) then begin
    // Berechnung der CPU-Auslastung

   Divisor:= CurrentUserTime + CurrentKernelTime - LastSystemUserTime - LastSystemKernelTime;
   if (Divisor > 0) then
     currentCpuUsage := 1.0 - ((CurrentIdleTime - LastSystemIdleTime) / Divisor);

    // Aktualisierung der letzten Systemzeiten
    LastSystemIdleTime := CurrentIdleTime;
    LastSystemKernelTime := CurrentKernelTime;
    LastSystemUserTime := CurrentUserTime;
  end else
    currentCpuUsage := 0.0; // Fehler beim Abrufen der Systemzeiten
end;

function TsysInfo.GetCPUName: string;
var
  Reg: TRegistry;
begin
  Result:='---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result:= Reg.ReadString('ProcessorNameString');
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  result:=ASCII(result);
end;


function TSysInfo.GetCPUIdentifier: string;
var
  Reg: TRegistry;
begin
  Result:='---';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := Reg.ReadString('Identifier');
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  result:=ASCII(result);
end;


function TSysInfo.GetTimeZone: string;
var
   reg: TRegistry;
begin
     Result:='---';
     reg:=TRegistry.Create;
     try
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      reg.OpenKeyReadOnly('System\CurrentControlSet\Control\TimeZoneInformation');
      Result:=reg.ReadString('StandardName');
      finally
      reg.free;
      end;
      result:=ASCII(result);
end;

procedure TSysInfo.GetPrinterInfos(StrngLst: TStringList);
var
   reg: TRegistry;       // Registry
   s: string;            // HilfsString
   Printers: TStrings;   // Liste mit PrinteSchlüsseln
   NrOfPrinters: Byte;   // Anzahl der Printer
   i: byte;              // Zähler
   Rs: TStrings;         // Ausgabe
begin

     { Initialisierung }
     Printers := TStringList.Create;
     Rs:= TStringList.Create;
     s:='';
     reg:=TRegistry.Create;

     try
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      reg.OpenKeyReadOnly('System\CurrentControlSet\Control\Print\Printers');

      if reg.HasSubKeys= true then begin

        reg.GetKeyNames(Printers);
        NrOfPrinters:=Printers.count;
        reg.CloseKey;
        reg.free;
       for i:= 1 to NrOfPrinters do begin
        reg:=TRegistry.create;
        reg.RootKey:=HKEY_LOCAL_MACHINE;
        s:='System\CurrentControlSet\Control\Print\Printers'+'\'+Printers[i-1];
        reg.OpenKeyReadOnly(s);
        rs.add(reg.ReadString('Name'));
        rs.add('Anschluss: '+reg.ReadString('Port'));
        rs.add('Freigabename: '+reg.ReadString('Share Name'));
        rs.add('------------------------------------------');
        reg.CloseKey;
        reg.free;
       end;  // end for

     end; //end if HasSubKeys
     except   //end try
      Rs.add('---');
     end;

     strnglst.AddStrings(Rs);


     Printers.Free;
     Rs.free;
end;

function TSysInfo.GetPartitionName(const ADrive: Char): String;
var
  tmp: Integer;
  buffer: array[0..19] of Char;
begin
     {
  GetVolumeInformation(PChar(ADrive+':\'),
                       @buffer[0], SizeOf(buffer),
                       nil,
                       tmp,
                       tmp,
                       nil,
                       0);
  Result := buffer;
  }
  Result:= '';
end;

function TSysInfo.GetDriveTyp(Drive: Char): string;
var
   p: Array[0..2] of char;
begin
     strpcopy(p,Drive+':');
      case GetDriveType(p) of
       DRIVE_REMOVABLE : begin
                          if (Drive='A') or (Drive='B') then result:='Diskettenlaufwerk'
                          else result:='RamDisk';
                         end;
       DRIVE_FIXED : result:='Festplatte';
       DRIVE_REMOTE : result:='Netzlaufwerk';
       DRIVE_RAMDISK : result:='RamDisk';
       DRIVE_CDROM : result:='CD-ROM';
       1 : result:='nicht vorhanden';
       else result:='unbekannt';
      end; //case
end;

function TSysInfo.GetDayOfTheWeek:string;
begin
     case DayOfWeek(now) of
      1: result:='Sonntag';
      2: result:='Montag';
      3: result:='Dienstag';
      4: result:='Mittwoch';
      5: result:='Donnerstag';
      6: result:='Freitag';
      7: result:='Samstag';
     end;
end;


function TSysInfo.GetUptime:string;
var
   d,h,m,s: integer;
   uptime: longint;
   uptimedate: TDateTime;
begin
    uptime:=GetTickCount;

    uptime:=trunc(uptime/1000);

    s:=uptime mod 60;
    uptime:=uptime div 60;

    m:=uptime mod 60;
    uptime:=uptime div 60;

    h:=uptime mod 60;
    uptime:=uptime div 60;

    d:=uptime mod 24;


    uptimedate:=encodeTime(h,m,s,0);

    if d>0 then result:=inttostr(d)+' Tage, '+TimeToStr(uptimedate)
    else result:=TimeToStr(uptimedate);
end;



function TSysInfo.GetMostRecentDirectDraw: string;
var
  Reg: TRegistry;
begin
  Result:='unbekannt';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Software\Microsoft\DirectDraw\MostRecentApplication') then
    begin
      Result := Reg.ReadString('Name');
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  result:=ASCII(result);

end;


function TSysInfo.GetMostRecentDirect3D: string;
var
  Reg: TRegistry;
begin
  Result:='unbekannt';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Direct3D\MostRecentApplication') then
    begin
      Result := Reg.ReadString('Name');
      Reg.CloseKey;
    end;
  except
  end;
  Reg.Free;
  result:=ASCII(result);
end;



  //Betriebssystem
function TSysInfo.GetOperatingSystem: String;
begin
  Result := 'unbekannt';
  // TODO
end;


procedure Register;
begin
  RegisterComponents('Hardware', [TSysInfo]);
end;

end.
