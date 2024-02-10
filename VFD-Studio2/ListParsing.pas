

function TMainForm.SubstituteStaticInfo(AText: string): string;
var
  I: Integer;
  Tmp: string;
  S: string;
  RegEx: TRegExpr;
  Match1: string;
  Match2: string;
begin

  RegEx:= TRegExpr.Create;
  S:= AText;

  if (Pos('$REGKEY_', S) <> 0 ) then begin
    RegEx.Expression:= '\$REGKEY_(.+)\/(.+)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      Match2:=RegEx.Match[2];
      S:= RegEx.Replace(S, FSysInfo.GetRegistryString(Match1, Match2), False);
    end;
  end;

  if (Pos('$SLOTUSAGE', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTUSAGE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].GetCurrentUsage, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

  if (Pos('$SLOTTYPE', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTTYPE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].GetSlotType, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

  if (Pos('$SLOTINFO', S) <> 0 ) then begin
    RegEx.Expression:= '\$SLOTINFO(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match1:=RegEx.Match[1];
      try
        I:= StrToIntDef(Match1, 0);
        if (FSMBios.HasProcessorInfo) and (I < Length(FSMBios.SystemSlotInfo)) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[I].SlotDesignationStr, False);
        end else begin
          S:= RegEx.Replace(S, RsInformationUnknown, False);
        end;
      except
        S:= RegEx.Replace(S, RsInformationUnknown, False);
      end;
    end;
  end;

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

  while (Pos('$OS$', S) <> 0 ) do begin
    I:= Pos('$OS$', S);
    Delete(S, I, Length('$OS$'));
    Insert(FSysInfo.GetOperatingSystem, S, I);
  end;

  while (Pos('$OSNAME$', S) <> 0 ) do begin
    I:= Pos('$OSNAME$', S);
    Delete(S, I, Length('$OSNAME$'));
    Insert(FSysInfo.GetOperatingSystemFullName, S, I);
  end;

  while (Pos('$CPUCORES$', S) <> 0 ) do begin
    I:= Pos('$CPUCORES$', S);
    Delete(S, I, Length('$CPUCORES$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.CoreCount), S, I);
    end else begin
      Insert('?', S, I);
    end;
  end;

  while (Pos('$CPUMAX$', S) <> 0 ) do begin
    I:= Pos('$CPUMAX$', S);
    Delete(S, I, Length('$CPUMAX$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.MaxSpeed), S, I);
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
    if (FSMBios.HasProcessorInfo) then begin
     Insert(FSMBios.ProcessorInfo[0].SocketDesignationStr, S, I);
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
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.ProcessorInfo[0].ProcessorFamilyStr, S, I);
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
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.VendorStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSVERSION$', S) <> 0 ) do begin
    I:= Pos('$BIOSVERSION$', S);
    Delete(S, I, Length('$BIOSVERSION$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BIOSDATE$', S) <> 0 ) do begin
    I:= Pos('$BIOSDATE$', S);
    Delete(S, I, Length('$BIOSDATE$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BiosInfo.ReleaseDateStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVENDOR$', S) <> 0 ) do begin
    I:= Pos('$BOARDVENDOR$', S);
    Delete(S, I, Length('$BOARDVENDOR$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].ManufacturerStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDPRODUCT$', S) <> 0 ) do begin
    I:= Pos('$BOARDPRODUCT$', S);
    Delete(S, I, Length('$BOARDPRODUCT$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].ProductStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  while (Pos('$BOARDVERSION$', S) <> 0 ) do begin
    I:= Pos('$BOARDVERSION$', S);
    Delete(S, I, Length('$BOARDVERSION$'));
    if (FSMBios.HasProcessorInfo) then begin
      Insert(FSMBios.BaseBoardInfo[0].VersionStr, S, I);
    end else begin
      Insert(RsInformationUnknown, S, I);
    end;
  end;

  RegEx.Free;

  Result:= S;
end;


function TMainForm.SubstituteVariableInfo(AText: string): string;
var
  I: Integer;
  S: string;
  Sec: string;  //Seconds (Winamp)
  CurrentDateTime: TDateTime;
  DoW: Word;
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
  RegEx: TRegExpr;
  Match: string;
begin

  RegEx:= TRegExpr.Create;
  S:= AText;
  CurrentDateTime:= Now;
  DecodeDateTime(CurrentDateTime, Year, Month, Day, Hour, Minute, Second, MilliSecond);

  if (Pos('$TOTALDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$TOTALDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, IntToStr(Round(FSysInfo.GetDiskSpace(Match[1]) / 1073741824)), False);
    end;
  end;

  if (Pos('$FREEDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$FREEDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, IntToStr(Round(FSysInfo.GetFreeDiskSpace(Match[1]) / 1073741824)), False);
    end;
  end;

  if (Pos('$NAMEDRIVE', S) <> 0 ) then begin
    RegEx.Expression:= '\$NAMEDRIVE(\w)\$';
    if (RegEx.Exec(S)) then begin
      Match:=RegEx.Match[1];
      S:= RegEx.Replace(S, FSysInfo.GetPartitionName(Match[1]),  False);
    end;
  end;

  while (Pos('$MEMORY$', S) <> 0 ) do begin
    I:= Pos('$MEMORY$', S);
    Delete(S, I, Length('$MEMORY$'));
    Insert(IntToStr(FMemUsageData.PhysicalMemory div 1048576), S, I);
  end;

  while (Pos('$FREEMEM$', S) <> 0 ) do begin
    I:= Pos('$FREEMEM$', S);
    Delete(S, I, Length('$FREEMEM$'));
    Insert(IntToStr(FMemUsageData.FreeMemory div 1048576), S, I);
  end;

  while (Pos('$MEMUSAGE$', S) <> 0 ) do begin
    I:= Pos('$MEMUSAGE$', S);
    Delete(S, I, Length('$MEMUSAGE$'));
    Insert(IntToStr(FMemUsageData.CurrentMemUsage), S, I);
  end;

  while (Pos('$AVERAGECPU$', S) <> 0 ) do begin
    I:= Pos('$AVERAGECPU$', S);
    Delete(S, I, Length('$AVERAGECPU$'));
    Insert(IntToStr(FCpuUsageData.AverageCpuUsage), S, I);
  end;

  while (Pos('$CPUUSAGE$', S) <> 0 ) do begin
    I:= Pos('$CPUUSAGE$', S);
    Delete(S, I, Length('$CPUUSAGE$'));
    Insert(IntToStr(FCpuUsageData.CurrentCpuUsage), S, I);
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
    if (FSMBios.HasProcessorInfo) then begin
      Insert(IntToStr(FSMBios.ProcessorInfo[0].RAWProcessorInformation^.CurrentSpeed), S, I);
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

  RegEx.Free;

  Result:= S;
end;

