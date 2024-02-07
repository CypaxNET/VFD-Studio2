

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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].GetCurrentUsage, False);
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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].GetSlotType, False);
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
        if (FSMBios.HasProcessorInfo) then begin
          S:= RegEx.Replace(S, FSMBios.SystemSlotInfo[StrToInt(Match1)].SlotDesignationStr, False);
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



// Interpretes a command (from ListBox), given and parameter S.
// If the command requires jumping formward/backwards the list, the desired new
// list index is returned; otherwise -1.
// Reads FListIndex; does not modify FListIndex.
function TMainForm.InterpreteListCommand(S: string): Integer;
var
  CmdParts: TStringList;
  Cmd: string;
  n, ITmp: Integer;
  X, Y: Integer;
  P1, P2, P3, P4, P5, P6: Integer;
  IsInverted: Boolean;
  IsRequirementMet: Boolean;
  AColor: TColor;
  Res: Integer;
  DoContinueScreen: Boolean;
begin
  Res:= -1;

  CmdParts:= TStringList.Create;
  try
    CmdParts.Delimiter:= ' ';
    CmdParts.QuoteChar:= '''';
    CmdParts.DelimitedText:= S;

    if (CmdParts.Count > 0) then begin
      Cmd:= CmdParts[0];

      if ('NEWSCREEN' = Cmd) then begin

        FIsCurrentScreenShownOnce:= False; // by default this screen will be shown again then the list loops
        DoContinueScreen:= False;          // by default, NEWSCREEN will stop animations, clear/reset variable information, etc.

        if (CmdParts.Count >= 2) then begin
          // p1 = screen limitations
          if ('ONCE' = CmdParts[1]) then begin
            // display this screen only once
            FIsCurrentScreenShownOnce:= True;
          end else if ('CONTINUE' = CmdParts[1]) then begin
            // do not stop animations, do not clear/reset variable infomration, etc.
            DoContinueScreen:= True;
          end else if (CmdParts[1].StartsWith('REQUIRE')) then begin
            // this screen shall only be shown if a requirement is met
            IsRequirementMet:= False; // False by default, must be set to True in code below

            if ('REQUIREWINAMP' = CmdParts[1]) then begin
              // check if Winamp is running
              if (FWinampControl.IsWinampRunning) then
                IsRequirementMet:= True;
            end;

            if (not IsRequirementMet) then begin
              //LogEvent(lvINFO, 'Requirment ' + CmdParts[1] + ' is not met -> skip to next screen', Now);

              // seek next 'NEWSCREEN' line; starting with next line
              ITmp:= FListIndex + 1;
              if (ITmp >= ListBox.Items.Count) then
                ITmp:= 0;
              Res:= SeekTo(ITmp, sdForward, 'NEWSCREEN');

              if (-1 = Res) then
                LogEvent(lvERROR, 'Could not find the next screen in the list.', Now);

            end; // if requirement not met
          end; // if something required
        end; // if 2nd parameter with screen limitations given

        if (not DoContinueScreen) then begin
          StopProcessing;
        end;

        if (nil <> FDisplay) then begin
          FDisplay.ShowScreen(BOTH_LAYERS);
          FPreviewDisplay.LayerMode:= lmXOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
          if (not FStudioConfig.DisplayConfig.IsBrightnessControlledByList) then begin
            if (nil <> FDisplay) then
              FDisplay.SetBrightness(FStudioConfig.DisplayConfig.DisplayBrightness);
          end;
        end;


      end else if ('SCREENEND' = Cmd) then begin
        if (True = FIsCurrentScreenShownOnce) then begin
          // remove this screen from the list

          // go back to last 'NEWSCREEN' line
          ITmp:= SeekTo(FListIndex, sdBackward, 'NEWSCREEN');

          if ((-1 = ITmp) or (ITmp > FListIndex)) then
            LogEvent(lvERROR, 'Could not find start of current screen.', Now)
          else begin
            LogEvent(lvINFO, 'This screen was once. Clearing lines ' + IntToStr(ITmp) + '..' + IntToStr(FListIndex), Now);
            ListBox.MultiSelect:= True;
            ListBox.ClearSelection;
            for n:= ITmp to FListIndex do begin
              ListBox.selected[n]:= True;
            end;
            ListBox.DeleteSelected;
            FIsCurrentScreenShownOnce:= False;

            Res:= ITmp - 1;
            if (Res < 0) then
              Res:= ListBox.Items.Count - 1;
          end;
        end;

      end else if ('DSPINIT' = Cmd) then begin
        if (nil <> FDisplay) then
          FDisplay.DspInit(FStudioConfig.DisplayConfig.ResX, FStudioConfig.DisplayConfig.ResY);

      end else if ('ORMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('XORMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmXOR;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('ANDMODE' = Cmd) then begin
        if (nil <> FDisplay) then begin
          FPreviewDisplay.LayerMode:= lmAND;
          FDisplay.SetLayerMode(FPreviewDisplay.LayerMode);
        end;

      end else if ('STOP' = Cmd) then begin
         WaitTimer.Enabled:= False;
         StopButton.caption:= RsBtnGo;
         Popupstopbutton.caption:= RsBtnGo;
         StopButton.ImageIndex:= 1;

      end else if ('CLEARSCREEN' = Cmd) then begin
        if (nil <> FDisplay) then
          FDisplay.ClearScreen;
        FPreviewDisplay.ClearScreen;
        FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);

      end else if ('SCREENTIME' = Cmd) then begin
        // p1 = screen time in seconds
        if (CmdParts.Count >= 2) then begin
          P1:= StrToInt(CmdParts[1]);
          WaitTimer.Interval:= P1 * 1000;
          //ScreenTimeLabel.caption:= 'ScreenTime: ' + floattostr(WaitTimer.Interval/1000)+ 'S';
          WaitTimer.Enabled:= True;
        end;

      end else if ('LIGHT' = Cmd) then begin
        // p1 = brightness level
        if (FStudioConfig.DisplayConfig.IsBrightnessControlledByList) then begin
          if (CmdParts.Count >= 2) then begin
            P1:= StrToInt(CmdParts[1]);
            if (nil <> FDisplay) then
              FDisplay.SetBrightness(P1);
          end;
        end;

      end else if ('NOISE' = Cmd) then begin
        // p1 = amount of pixels, p2 = inverted or not [optional]
         if (CmdParts.Count >= 2) then begin
           P1:= StrToInt(CmdParts[1]);
           if ((CmdParts.Count >= 3) and ((CmdParts[2].ToUpper = 'TRUE') or (CmdParts[2] = '1'))) then begin
             IsInverted:= True;
             AColor:= clWhite;
           end else begin
             IsInverted:= False;
             AColor:= clBlack;
           end;
           Randomize;
           for ITmp:= 1 to P1 do begin
             X:= Random(FStudioConfig.DisplayConfig.ResX);
             Y:= Random(FStudioConfig.DisplayConfig.ResY);
             if (nil <> FDisplay) then
               FDisplay.PaintPixel(X, Y, IsInverted);
             FPreviewDisplay.GraphicsLayer.Canvas.Pixels[X, Y]:= AColor;
           end;
           FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
         end;

      end else if ('PIXEL' = Cmd) then begin
        // p1 = x, p2 = y, p3 = inverted or not [optional]
        if (CmdParts.Count >= 3) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          if ((CmdParts.Count >= 4) and ((CmdParts[3].ToUpper = 'TRUE') or (CmdParts[3] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P1, P2]:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintPixel(P1, P2, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P1, P2]:= clBlack;
          end;
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('LINE' = Cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintLine(P1, P2, P3, P4, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          end;
          FPreviewDisplay.GraphicsLayer.Canvas.Line(P1, P2, P3, P4);
          FPreviewDisplay.GraphicsLayer.Canvas.Pixels[P3, P4]:= FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color;
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('FRAME' = Cmd) then begin
        // p1 = x0, p2 = y0, p3 = x1, p4 = y1, p5 = inverted or not [optional]
        if (CmdParts.Count >= 5) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          if ((CmdParts.Count >= 6) and ((CmdParts[5].ToUpper = 'TRUE') or (CmdParts[5] = '1'))) then begin
            if (nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, True);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clWhite;
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintFrame(P1, P2, P3, P4, False);
            FPreviewDisplay.GraphicsLayer.Canvas.Pen.Color:= clBlack;
          end;
          FPreviewDisplay.GraphicsLayer.Canvas.Frame(P1, P2, P3 + 1, P4 + 1);
          FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
        end;

      end else if ('PLAINTEXT' = Cmd) then begin
        // p1 = text, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$') ) then begin
            HandleTextOutput(CmdParts[1], P2, P3, '', 0);
          end else begin
            if (nil <> FDisplay) then
              FDisplay.PaintString(CmdParts[1], P2, P3);
            FPreviewDisplay.PaintString(CmdParts[1], P2, P3);
            FPreviewDisplay.CombineVirtualLayers(PreviewImage.Picture.Bitmap);
          end;
        end;

      end else if ('TEXTOUT' = Cmd) then begin
        // p1 = text, p2 = x, p3 = y, p4 = font size, p5 = font name
        if (CmdParts.Count >= 6) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          // does the text to be displayed include any '$' characters?
          if (CmdParts[1].Contains('$') ) then begin
            HandleTextOutput(CmdParts[1], P2, P3, CmdParts[5], P4);
          end else begin
            DrawFontedText(CmdParts[1], P2, P3, CmdParts[5], P4);
          end;
        end;

      end else if ('ANIMATE' = Cmd) then begin
        // p1 = file name, p2 = animation speed, p3 = x, p4 = y, p5 = frame width
        if (False = FAnimationData.IsAnimationDisplayed) then begin
          // only one animation per screen is supported
          if (CmdParts.Count >= 6) then begin
            P2:= StrToInt(CmdParts[2]);
            P3:= StrToInt(CmdParts[3]);
            P4:= StrToInt(CmdParts[4]);
            P5:= StrToInt(CmdParts[5]);
            Animate(CmdParts[1], P2, P3, P4, P5);
            FAnimationData.IsAnimationDisplayed:= True;
          end;
        end;

      end else if ('BITMAP' = Cmd) then begin
        // p1 = file path, p2 = x, p3 = y
        if (CmdParts.Count >= 4) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          BitmapToVFD(CmdParts[1], P2, P3);
        end;

      end else if ('DRIVEUSAGE' = Cmd) then begin
        // p1 = drive letter, P2 = x, P3 = y, P4 = width (in characters)
        if (CmdParts.Count >= 5) then begin
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          DrawDriveUsage(CmdParts[1][1], P2, P3, P4);

        end;

      end else if ('CLOCK' = Cmd) then begin
        // p1 = offset in minutes, p2 = x, p3 = y, p4 = hour hand length, p5 = minute hand length, p6 = seconds hand length
        if (CmdParts.Count >= 7) then begin
          P1:= StrToInt(CmdParts[1]);
          P2:= StrToInt(CmdParts[2]);
          P3:= StrToInt(CmdParts[3]);
          P4:= StrToInt(CmdParts[4]);
          P5:= StrToInt(CmdParts[5]);
          P6:= StrToInt(CmdParts[6]);
          AddClock(P1, P2, P3, P4, P5, P6);
        end;

      end else if ('THEMATRIX' = Cmd) then begin
        // p1 = speed
        if (CmdParts.Count >= 2) then begin
          P1:= StrToInt(CmdParts[1]);

          // Setting up the drops
          InitTheMatrix;
          ExtraTimer.Interval:= P1;
          ExtraTimer.Enabled:= True;

        end;

      end else if ('CPUMONITOR' = Cmd) then begin
        // p1 = number of rows to use, p2 = bottom-most row
          if (CmdParts.Count >= 3) then begin
            if (False = FCpuUsageData.IsUsageMonitorDisplayed) then begin
              P1:= StrToInt(CmdParts[1]);
              P2:= StrToInt(CmdParts[2]);
              FCpuUsageData.NumRows:= P1;
              FCpuUsageData.BottomRow:= P2;
              FCpuUsageData.IsUsageMonitorDisplayed:= True;
            end;
          end;

      end else if ('RAMMONITOR' = Cmd) then begin
        // p1 = number of rows to use, p2 = bottom-most row
          if (CmdParts.Count >= 3) then begin
            if (False = FMemUsageData.IsUsageMonitorDisplayed) then begin
              P1:= StrToInt(CmdParts[1]);
              P2:= StrToInt(CmdParts[2]);
              FMemUsageData.NumRows:= P1;
              FMemUsageData.BottomRow:= P2;
              FMemUsageData.IsUsageMonitorDisplayed:= True;
            end;
          end;

      end;


    end; // endif (CmdParts.Count > 0)

  finally
    CmdParts.Free;
  end;
  Result:= Res;
end;
