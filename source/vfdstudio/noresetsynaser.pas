{==============================================================================|
| TBlockSerialNoReset - a TBlockSerial variant without DTR/RTS so ESP8266 MCUs |
| are not set into programming mode when opening a serial port                 |
|==============================================================================|
| Content: Serial port support                                                 |
|==============================================================================}


unit noresetsynaser;

interface

uses
  {$IFNDEF MSWINDOWS}
    {$IFDEF POSIX}
      Posix.Termios, Posix.Fcntl, Posix.Unistd, Posix.Stropts, Posix.SysSelect, Posix.SysTime,
      {$IFDEF LINUX}
        Linuxapi.KernelIoctl,
      {$ENDIF}
    {$ELSE}
      {$IFNDEF NO_LIBC}
         Libc,
         KernelIoctl,
      {$ELSE}
         termio, baseunix, unix,
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF FPC}
    Types,
    {$ENDIF}
  {$ELSE}
    Windows, registry,
    {$IFDEF FPC}
    winver,
    {$ENDIF}
  {$ENDIF}
    synafpc,
    Classes, SysUtils, synautil, synaser;

const
  {:stopbit value for 1 stopbit}
  SB1 = 0;
  {:stopbit value for 1.5 stopbit}
  SB1andHalf = 1;
  {:stopbit value for 2 stopbits}
  SB2 = 2;

type

  TBlockSerialNoReset = class(TBlockSerial)
  protected
  public
    procedure Config(baud, bits: integer; parity: char; stop: integer;
      softflow, hardflow: boolean); override;
    procedure Connect(comport: string); override;
  published
  end;

implementation

procedure TBlockSerialNoReset.Config(baud, bits: integer; parity: char; stop: integer;
  softflow, hardflow: boolean);
begin
  FillChar(dcb, SizeOf(dcb), 0);
  GetCommState;
  dcb.DCBlength := SizeOf(dcb);
  dcb.BaudRate := baud;
  dcb.ByteSize := bits;
  case parity of
    'N', 'n': dcb.parity := 0;
    'O', 'o': dcb.parity := 1;
    'E', 'e': dcb.parity := 2;
    'M', 'm': dcb.parity := 3;
    'S', 's': dcb.parity := 4;
  end;
  dcb.StopBits := stop;
  dcb.XonChar := #17;
  dcb.XoffChar := #19;
  dcb.XonLim := FRecvBuffer div 4;
  dcb.XoffLim := FRecvBuffer div 4;
  dcb.Flags := dcb_Binary;
  if softflow then
    dcb.Flags := dcb.Flags or dcb_OutX or dcb_InX;
  if hardflow then
    dcb.Flags := dcb.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake;
  //else
  //  dcb.Flags := dcb.Flags or dcb_RtsControlEnable;
  //dcb.Flags := dcb.Flags or dcb_DtrControlEnable;
  if dcb.Parity > 0 then
    dcb.Flags := dcb.Flags or dcb_ParityCheck;
  SetCommState;
end;

procedure TBlockSerialNoReset.Connect(comport: string);
{$IFDEF MSWINDOWS}
var
  CommTimeouts: TCommTimeouts;
{$ENDIF}
begin
  // Is this TBlockSerialNoReset Instance already busy?
  if InstanceActive then           {HGJ}
  begin                            {HGJ}
    RaiseSynaError(ErrAlreadyInUse);
    Exit;                          {HGJ}
  end;                             {HGJ}
  FBuffer := '';
  FDevice := comport;
  GetComNr(comport);
{$IFDEF MSWINDOWS}
  SetLastError (sOK);
{$ELSE}
  {$IFNDEF FPC}
  SetLastError (sOK);
  {$ELSE}
  fpSetErrno(sOK);
  {$ENDIF}
{$ENDIF}
{$IFNDEF MSWINDOWS}
  if FComNr <> PortIsClosed then
    FDevice := '/dev/ttyS' + IntToStr(FComNr);
  {$IFDEF USE_LINUX_LOCK}
    // Comport already owned by another process?          {HGJ}
    if FLinuxLock then
      if not cpomComportAccessible then
      begin
        if FileExists(LockfileName) then
          RaiseSynaError(ErrAlreadyOwned)
        else
          RaiseSynaError(ErrAccessDenied);

        Exit;
      end;
  {$ENDIF}

  {$IFNDEF FPC}
    {$IFDEF POSIX}
      FHandle := open(MarshaledAString(AnsiString(FDevice)), O_RDWR or O_SYNC);
    {$ELSE}
      FHandle := THandle(Libc.open(pchar(FDevice), O_RDWR or O_SYNC));
    {$ENDIF}
  {$ELSE}
    FHandle := THandle(fpOpen(FDevice, O_RDWR or O_SYNC));
  {$ENDIF}
  if FHandle = INVALID_HANDLE_VALUE then  //because THandle is not integer on all platforms!
    SerialCheck(-1)
  else
    SerialCheck(0);
  {$IFDEF USE_LINUX_LOCK}
  if FLastError <> sOK then
    if FLinuxLock then
      cpomReleaseComport;
  {$ENDIF}
  ExceptCheck;
  if FLastError <> sOK then
    Exit;
{$ELSE}
  if FComNr <> PortIsClosed then
    FDevice := '\\.\COM' + IntToStr(FComNr + 1);
  FHandle := THandle(CreateFile(PChar(FDevice), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0));
  if FHandle = INVALID_HANDLE_VALUE then  //because THandle is not integer on all platforms!
    SerialCheck(-1)
  else
    SerialCheck(0);
  ExceptCheck;
  if FLastError <> sOK then
    Exit;
  SetCommMask(FHandle, 0);
  SetupComm(Fhandle, FRecvBuffer, 0);
  CommTimeOuts.ReadIntervalTimeout := MAXWORD;
  CommTimeOuts.ReadTotalTimeoutMultiplier := 0;
  CommTimeOuts.ReadTotalTimeoutConstant := 0;
  CommTimeOuts.WriteTotalTimeoutMultiplier := 0;
  CommTimeOuts.WriteTotalTimeoutConstant := 0;
  SetCommTimeOuts(FHandle, CommTimeOuts);
  {$IFDEF WIN32}
  FPortAddr := GetPortAddr;
  {$ENDIF}
{$ENDIF}
  SetSynaError(sOK);
  if not TestCtrlLine then  {HGJ}
  begin
    SetSynaError(ErrNoDeviceAnswer);
    FileClose(FHandle);         {HGJ}
    {$IFDEF USE_LINUX_LOCK}
    if FLinuxLock then
      cpomReleaseComport;                {HGJ}
    {$ENDIF}                             {HGJ}
    Fhandle := INVALID_HANDLE_VALUE;     {HGJ}
    FComNr:= PortIsClosed;               {HGJ}
  end
  else
  begin
    FInstanceActive:= True;
    //RTS := True;
    //DTR := True;
    Purge;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, FDevice);
end;

end.
