
resourcestring

  RsHelpCOMMENT =
    'All lines starting with a ";"-character are considered as comments.' + LineEnding +
    'Comments will not processed and are for your convenience - Example: to describe individual commands or screens.';

  RsHelpINCLUDE =
    'Includes another VFD-Studio list file at the position of this command.' + LineEnding +
    'Note that recursive inclusion is not supported: any INCLUDE commands from an included file will be ignored.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  file path and name of another list file';

  RsHelpNEWSCREEN =
    'Presentations on the display are shown screen by screen.' + LineEnding +
    'This command starts a new screen. ' + LineEnding +
    'By default, it stops all animations, effects, updateable information, etc.' +
    'In an optional parameter you can specify this behavior:' + LineEnding + LineEnding +
    'Param1 [optional]:' + LineEnding +
    '  ONCE - screen shown only once' + LineEnding +
    '  CONTINUE - do not stop previous effects, etc.' + LineEnding +
    '  REQUIREWINAMP - shown only when Winamp is running';

  RsHelpENDSCREEN =
    'End mark of a screen.' + LineEnding +
    'Each screen starting with a NEWSCREEN command must be closed with a ENDSCREEN command. ' +
    'In the editor, the associated NEWSCREEN / ENDSCREEN commands are highlighted when selected.' + LineEnding +
    'No parameters.';

  RsHelpSTOP =
    'Instucts VFD-Studio to stay at this screen.' + LineEnding +
    'When this command is processed, the current screen will be displayed until the user clicks on "GO" in VFD-Studio. '  + LineEnding +
    'Must be placed after the SCREENTIME command to be effective.' + LineEnding +
    'No parameters.';

  RsHelpLIGHT =
    'Sets the display brightness.' + LineEnding +
    'This command has no effect if VFD-Studio is configured to control brightness directly from the application.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  brightness 0..100';

  RsHelpPLAINTEXT =
    'Draws unformatted text on the display using a 8x6 character set.' + LineEnding +
    'The text must be enclosed in ''-quote signs. To place a quote sign inside the text use " or two ''´s in a row.' + LineEnding +
    'The text might include special keywords which start and end with a $-character. ' +
    'Keywords will be replaced by the respective information when displayed. Some keywords like $TIME$ will be periodically updated while being displayed.' + LineEnding +
    'For a list of supported keywords see the keyword list.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  text, MUST be enclosed in ''-quote signs' + LineEnding +
    'Param2:' + LineEnding + '  column of first character' + LineEnding +
    'Param3:' + LineEnding + '  row of first character';

  RsHelpTEXTOUT =
    'Draws formatted text on the display with given font.' + LineEnding +
    'The font name must be enclosed in ''-quote signs.' + LineEnding +
    'The text must be enclosed in ''-quote signs. To place a quote sign inside the text use " or two ''´s in a row.' + LineEnding +
    'The text might include special keywords which start and end with a $-character. ' +
    'Keywords will be replaced by the respective information when displayed. Some keywords like $TIME$ will be periodically updated while being displayed.' + LineEnding +
    'For a list of supported keywords see the keyword list.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  text, MUST be enclosed in ''-quote signs' + LineEnding +
    'Param2:' + LineEnding + '  x coordinate' + LineEnding +
    'Param3:' + LineEnding + '  y coordinate' + LineEnding +
    'Param4:' + LineEnding + '  font size in pixels' + LineEnding +
    'Param5:' + LineEnding + '  font name, MUST be enclosed in ''-quote signs';

  RsHelpPIXEL =
    'Draws a pixel to the display at given coordinates.' + LineEnding +
    'By default, the pixel will be on/lit.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  x coordinate' + LineEnding +
    'Param2:' + LineEnding + '  y coordinate' + LineEnding +
    'Param3 [optional]:' + LineEnding + '  pixel shall be off/black (1 or TRUE)';

  RsHelpLINE =
    'Draws a line from x1,y1 to x2,y2 on the display.' + LineEnding +
    'By default, the line pixels will be on/lit.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  x1 coordinate' + LineEnding +
    'Param2:' + LineEnding + '  y1 coordinate' + LineEnding +
    'Param3:' + LineEnding + '  x2 coordinate' + LineEnding +
    'Param4:' + LineEnding + '  y2 coordinate' + LineEnding +
    'Param5 [optional]:' + LineEnding + '  pixels shall be off/black (1 or TRUE)';

  RsHelpFRAME =
    'Draws a rectangle from x1,y1 to x2,y2 on the display.' + LineEnding +
    'By default, the rectangle pixels will be on/lit.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  x1 coordinate' + LineEnding +
    'Param2:' + LineEnding + '  y1 coordinate' + LineEnding +
    'Param3:' + LineEnding + '  x2 coordinate' + LineEnding +
    'Param4:' + LineEnding + '  y2 coordinate' + LineEnding +
    'Param5 [optional]:' + LineEnding + '  pixels shall be off/black (1 or TRUE)';

  RsHelpNOISE =
    'Draws pixels randomly all over the display.' + LineEnding +
    'By default, the pixels will be on/lit.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  number of pixels' + LineEnding +
    'Param2 [optional]:' + LineEnding + '  pixels shall be off/black (1 or TRUE)';

  RsHelpDRIVEUSAGE =
    'Illustrates the memory usage on a hard drive.' + LineEnding +
    'Make sure, the specified drive letter actually exists.' + LineEnding +
    'It uses special characters to represent free/used disk space.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  drive letter, MUST be enclosed in ''-quote signs' + LineEnding +
    'Param2:' + LineEnding + '  column of first character' + LineEnding +
    'Param3:' + LineEnding + '  row of first character' + LineEnding +
    'Param4:' + LineEnding + '  chart length in characters';

  RsHelpRAMMONITOR =
    'Illustrates the RAM memory usage.' + LineEnding +
    'The RAM monitor prints a history chart of recent memory usage on the display where the most right value represent current memory usage in percent.' +
    'The chart uses the entire text width of the display and is updated once each second.' + LineEnding +
    '(Note: The List Editor previews a chart with random data.)' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  number of rows, must be at least 1' + LineEnding +
    'Param2:' + LineEnding + '  bottom row of chart';

  RsHelpCPUMONITOR =
    'Illustrates the CPU usage.' + LineEnding +
    'The CPU monitor prints a history chart of recent CPU load on the display where the most right value represent current CPU usage in percent.' +
    'The chart uses the entire text width of the display and is updated once each second.' + LineEnding +
    '(Note: The List Editor previews a chart with random data.)' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  number of rows, must be at least 1' + LineEnding +
    'Param2:' + LineEnding + '  bottom row of chart';

  RsHelpCLOCK =
    'Shows an analog clock.' + LineEnding +
    'Draws the hour, minute and seconds hand with given dimensions.' + LineEnding +
    'The radius of each hand can be specified; if 0, the hand will not be shown.' + LineEnding +
    'A clock face is not drawn but can be added with the BITMAP command. ' +
    'However, the moving hands will overwrite any graphic content which is within their radius.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  offset in minutes, can be negative' + LineEnding +
    'Param2:' + LineEnding + '  x coordinate of center' + LineEnding +
    'Param3:' + LineEnding + '  y coordinate of center' + LineEnding +
    'Param4:' + LineEnding + '  radius of hour hand' + LineEnding +
    'Param5:' + LineEnding + '  radius of minute hand' + LineEnding +
    'Param6:' + LineEnding + '  radius of seconds hand';

  RsHelpMATRIX =
    'Raining characters effect.' + LineEnding +
    'Creates an effect like in the movie The Matrix with raining characters.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  update interval in ms';

  RsHelpWAPLAYBAR =
    'Graphical or textual illustration of Winamp play progress.' + LineEnding +
    'Depending on mode (graphical or textual) the params have different units: character position or pixel position.' + LineEnding +
    '(Note: a Screen can only have one play bar at a time.)' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  text column / x coordinate' + LineEnding +
    'Param2:' + LineEnding + '  text row / y coordinate' + LineEnding +
    'Param3:' + LineEnding + '  width in characters / pixels' + LineEnding +
    'Param4 [optional]:' + LineEnding + '  graphical mode (1 or TRUE)';

  RsHelpSCREENTIME =
    'Specifies how long a screen is shown.' + LineEnding +  LineEnding +
    'Param1:' + LineEnding + '  number of seconds the screen is shown';

  RsHelpCLEARSCREEN =
    'Clears all content from the display.' + LineEnding +
    'No parameters.';

  RsHelpBITMAP =
    'Draws an image from file on the display.' + LineEnding +
    'The image should be black&white and in Bitmap format.' + LineEnding +
    'Make sure to enclose the file name in quote signs and that the file actually exists.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  filename, MUST be enclosed in ''-quote signs' + LineEnding +
    'Param2:' + LineEnding + '  x coordinate' + LineEnding +
    'Param3:' + LineEnding + '  y coordinate';

  RsHelpANIMATE =
    'Draws an animation from file on the display.' + LineEnding +
    'An animation image consist of several single frames of same width, horizontally aligned side by side.' + LineEnding +
    'By default, an animpation plays continuosly, add (0 or FALSE) to the command to play only one sequence.' + LineEnding +
    'Based on the image width and the frame width, VFD-Studio calculates the number of frames.' + LineEnding +
    'The animation image should be black & white and in Bitmap format.' + LineEnding +
    'Make sure to enclose the file name in quote signs and that the file actually exists.' + LineEnding + LineEnding +
    'Param1:' + LineEnding + '  filename, MUST be enclosed in ''-quote signs' + LineEnding +
    'Param2:' + LineEnding + '  refresh rate; Example: 500 = every 500ms' + LineEnding +
    'Param3:' + LineEnding + '  x coordinate' + LineEnding +
    'Param4:' + LineEnding + '  y coordinate' + LineEnding +
    'Param5:' + LineEnding + '  frame width in pixels' + LineEnding +
    'Param6 [optional]:' + LineEnding + '  play single sequence (0 or FALSE)';

  RsHelpORMODE =
    'OR combination of display layers.' + LineEnding +
    'Graphical and textual content is presented on separate levels and shown combined on the display.' + LineEnding +
    'It instructs VFD-Studio to OR-combine the layers -  if a pixel is set on any layer it will be lit on the display, otherwise it stays black/off.' + LineEnding +
    '(This command will not be effective in the List Editor.)' + LineEnding +
    'No parameters.';

  RsHelpXORMODE =
    'XOR combination of display layers.' + LineEnding +
    'Graphical and textual content is presented on separate levels and shown combined on the display.' + LineEnding +
    'It instructs VFD-Studio to XOR-combine (exclusive OR) the layers -  if a pixel is set on one layer but not on the other it will be lit on the display, otherwise it stays black/off.' + LineEnding +
    'The XOR-mode if the default mode when a screen is started with NEWSCREEN.' + LineEnding +
    '(This command will not be effective in the List Editor.)' + LineEnding +
    'No parameters.';

  RsHelpANDMODE =
    'AND combination of display layers.' + LineEnding +
    'Graphical and textual content is presented on separate levels and shown combined on the display.' + LineEnding +
    'It instructs VFD-Studio to AND-combine the layers -  if a pixel is set on both layers it will be lit on the display, otherwise it stays black/off.' + LineEnding +
    '(This command will not be effective in the List Editor.)' + LineEnding +
    'No parameters.';

  RsHelpCOLON =
    'Blinking ":"-character.' + LineEnding +
    'For example for custom time in the form "$HH$$:$$MM$"';

  RsHelpAVERAGECPU =
    'Average CPU load in percent.' + LineEnding +
    'Percent-character (%) is not included.';

  RsHelpBIOSDATE =
    'Date of the BIOS.' + LineEnding +
    'The date format may vary.' + LineEnding +
    'Example: "11/08/2018"';

  RsHelpBIOSVENDOR =
    'Company name of the BIOS vendor.' + LineEnding +
    'Example: "AMIBIOS"';

  RsHelpBIOSVERSION =
    'Releaseversion of the BIOS.' + LineEnding +
    'Example: "v1.2b"';

  RsHelpBOARDPRODUCT =
    'Product name of the mainboard.' + LineEnding +
    'Example: "P8Z68 Deluxe"';

  RsHelpBOARDVENDOR =
    'Company name of the mainboard.' + LineEnding +
    'Example: "ASUSTeK"';

  RsHelpBOARDVERSION =
    'Version of the mainboard.' + LineEnding +
    'Example: "Rel. 1.23"';

  RsHelpCOLORS =
    'Current color depth in bits.' + LineEnding +
    'Includes the "Bit" unit.' + LineEnding +
    'Example: "32 Bit"';

  RsHelpCPUCORES =
    'Number of physical CPU cores.' + LineEnding +
    'Example: "4"';

  RsHelpCPUFAMILY =
    'CPU family name.' + LineEnding +
    'Example: "Intel10 Core 2 Duo Processor"';

  RsHelpCPUIDENT =
    'CPU identifier.' + LineEnding +
    'Example: "Intel64 Family 6 Model 42 Stepping 7"';

  RsHelpCPUMAX =
    'Maximum CPU frequency in MHz.' + LineEnding +
    'Example: "3800"';

  RsHelpCPUNAME =
    'Full name of the CPU.' + LineEnding +
    'Example: "Intel Core i7-2600K CPU @ 3.40GHz"';

  RsHelpCPUSOCKET =
    'Type of CPU socket.' + LineEnding +
    'Example: "LGA1155"';

  RsHelpCPUSPEED =
    'Current CPU frequency in MHz (estimated).' + LineEnding +
    'Example: "3507"';

  RsHelpCPUUSAGE =
    'Current CPU load in percent.' + LineEnding +
    'Percent-character (%) is not included.';

  RsHelpCPUVENDOR =
    'Vendor name of the CPU.' + LineEnding +
    'Example: "GenuineIntel"';

  RsHelpDATE =
    'Current system date in local date format.' + LineEnding +
    'Example: "09.02.2024"';

  RsHelpDAY =
    'Current day of the month.' + LineEnding +
    'Example: "09"';

  RsHelpDOW =
    'Day of the week, translated to the language selected in VFD-Studio.' + LineEnding +
    'Example: "Freitag"';

  RsHelpFREEDRIVE =
    'Free disk space in GB.' + LineEnding +
    'Must include a drive letter (see code example).';

  RsHelpFREEMEM =
    'Free physical memory in MB.' + LineEnding +
    'Example: "7564"';

  RsHelpFREQ =
    'Main monitor refresh rate.' + LineEnding +
    'Includes the "Hz" unit.' + LineEnding +
    'Example: "59 Hz"';

  RsHelpHH =
    'Current hour in 24h format.' + LineEnding +
    'Example: "08"';

  RsHelpIP =
    'Assigned IP address of the first network interface.' + LineEnding +
    'Example: "192.168.10.63"';

  RsHelpMEMORY =
    'Total size of physical memory in MB.' + LineEnding +
    'Example: "15890"';

  RsHelpMEMUSAGE =
    'Usage of physical memory in percent.' + LineEnding +
    'Percent-character (%) is not included.';

  RsHelpMM =
    'Current minute.' + LineEnding +
    'Example: "04"';

  RsHelpMON =
    'Current month.' + LineEnding +
    'Example: "02"';

  RsHelpNAMEDRIVE =
    'Label/name of a drive.' + LineEnding +
    'Must include a drive letter (see code example).' + LineEnding +
    'Example: "Backup"';

  RsHelpOS =
    'Short name of operating system.' + LineEnding +
    'Example: "Windows 10"';

  RsHelpOSNAME =
    'Long name of operating system.' + LineEnding +
    'Example: "Windows 10 Pro"';

  RsHelpPCNAME =
    'Name of the computer.' + LineEnding +
    'Example: "Cypax'' old PC"';

  RsHelpRECENTD3D =
    'Most recent used Direct 3D application (or ??? if not applicable).' + LineEnding +
    'Example: "pinball.exe"';

  RsHelpRECENTDD =
    'Most recent used Direct 3D application (or ??? if not applicable).' + LineEnding +
    'Example: "solitaire.exe"';

  RsHelpREGKEY_ =
    'Content of a Windows registry key.' + LineEnding +
    'Must include a path\key to a textual (REG_SZ) registry key in HKEY_LOCAL_MACHINE (see code example).';

  RsHelpRES =
    'Screen resolution of main monitor.' + LineEnding +
    'Example: "1920x1080"';

  RsHelpSLOTINFO =
    'Information on a mainboard slot.' + LineEnding +
    'Must include a slot number 0..n (see code example).' + LineEnding +
    'Example: "PCIEX16_1"';

  RsHelpSLOTTYPE =
    'Type of a mainboard slot.' + LineEnding +
    'Must include a slot number 0..n (see code example).' + LineEnding +
    'Example: "PCI Express"';

  RsHelpSLOTUSAGE =
    'Usage of a mainboard slot.' + LineEnding +
    'Must include a slot number 0..n (see code example).' + LineEnding +
    'Example: "In use"';

  RsHelpSS =
    'Current second.' + LineEnding +
    'Example: "07"';

  RsHelpTIME =
    'Current time in local time format.' + LineEnding +
    'Example: "12:34:45"';

  RsHelpTIMEZONE =
    'Name of the current time zone, translated to the system language.' + LineEnding +
    'Example: "Mitteleuropäische Zeit"';

  RsHelpTOTALDRIVE =
    'Total disk space in GB.' + LineEnding +
    'Must include a drive letter (see code example).';

  RsHelpUPTIME =
    'Uptime of the computer .' + LineEnding +
    'Example: "03:13:45"';

  RsHelpUSERNAME =
    'Name of the user who started VFD-Studio.' + LineEnding +
    'Example: "Cypax"';

  RsHelpVERSION =
    'Prints the version of VFD-Studio.' + LineEnding +
    'Example: "2.0.0.0"';

  RsHelpWALENGTH =
    'Duration of the current Winamp song.' + LineEnding +
    'Example: "3:15"';

  RsHelpWAPOS =
    'Play position of the current Winamp song.' + LineEnding +
    'Example: "0:56"';

  RsHelpWATITLE =
    'Title of the current Winamp song.' + LineEnding +
    'Example: "Ayla - Into the Light"';

  RsHelpWAVERSION =
    'Version of Winamp.' + LineEnding +
    'Example: "5.03"';

  RsHelpYEAR =
    'Current year.' + LineEnding +
    'Example: "2024"';

  RsHelpOHM =
    'Sensor value from Open Hardware Monitor (OHM).' + LineEnding +
    'OHM must be running to obtain updated values.' + LineEnding +
    'Param1:' + LineEnding + '  Component - must be the exact name as shown in OHM or the correct identifier such as "/intelcpu/0"' + LineEnding +
    'Param2:' + LineEnding + '  Sensor type (e.g. "Temperature", "Clock", ...)' + LineEnding +
    'Param3:' + LineEnding + '  Sensor name - must be the exact name as shown in OHM (e.g. "CPU Core #2)';

const

  // COMMANDS
  Commands: array of TExpression = (
    (Expr: 'INCLUDE';
      Help: RsHelpINCLUDE;
      Example: ';insert the content of other file:' +
                LineEnding +
                'INCLUDE Lists\MyList.vfdlst'),
    (Expr: 'NEWSCREEN';
      Help: RsHelpNEWSCREEN;
      Example: ';Show only when Winamp runs' +
                LineEnding +
                'NEWSCREEN REQUIREWINAMP' + LineEnding +
                'PLAINTEXT ''Song:'' 0 0' + LineEnding +
                'PLAINTEXT ''$WATITLE$'' 1 1' + LineEnding +
                'SCREENTIME 60' + LineEnding +
                'ENDSCREEN'),
    (Expr: 'ENDSCREEN';
      Help: RsHelpENDSCREEN;
      Example: ';Click on NEWSCREEN/ENDSCREEN' + LineEnding +
                'NEWSCREEN' + LineEnding +
                'PLAINTEXT ''Hello'' 0 0' + LineEnding +
                'SCREENTIME 20' + LineEnding +
                'ENDSCREEN'),
    (Expr: 'CLEARSCREEN';
      Help: RsHelpCLEARSCREEN;
      Example: ';Clear all screen content' + LineEnding +

      'CLEARSCREEN'),
    (Expr: 'SCREENTIME';
      Help: RsHelpSCREENTIME;
      Example: ';Show this for 20s' + LineEnding +
                'NEWSCREEN' + LineEnding +
                'PLAINTEXT ''Hello'' 0 0' + LineEnding +
                'SCREENTIME 20' + LineEnding +
                'ENDSCREEN'),
    (Expr: 'STOP';
      Help: RsHelpSTOP;
      Example: 'NEWSCREEN' + LineEnding +
                'PLAINTEXT ''We''''ll stay here.'' 0 0' + LineEnding +
                'SCREENTIME 20' + LineEnding +
                'STOP' + LineEnding +
                'ENDSCREEN'),
    (Expr: 'ORMODE';
      Help: RsHelpORMODE;
      Example: ';OR-combination' + LineEnding +
                'ORMODE' + LineEnding +
                'PLAINTEXT ''Hello'' 0 0' + LineEnding +
                'BITMAP ''mybitmap.bmp'' 0 0'),
    (Expr: 'XORMODE';
      Help: RsHelpXORMODE;
      Example: ';XOR-combination (default)' + LineEnding +
                'XORMODE' + LineEnding +
                'PLAINTEXT ''Hello'' 0 0' + LineEnding +
                'BITMAP ''mybitmap.bmp'' 0 0'),
    (Expr: 'ANDMODE';
      Help: RsHelpANDMODE;
      Example: ';AND-combination' + LineEnding +
                'ANDMODE' + LineEnding +
                'PLAINTEXT ''Hello'' 0 0' + LineEnding +
                'BITMAP ''mybitmap.bmp'' 0 0'),
    (Expr: 'LIGHT';
      Help: RsHelpLIGHT;
      Example: ';full brightness' + LineEnding +
                'LIGHT 100' + LineEnding +
                ';50% brightness' + LineEnding +
                'LIGHT 50'),
    (Expr: 'PLAINTEXT';
      Help: RsHelpPLAINTEXT;
      Example: 'PLAINTEXT ''It''''s me!'' 01 02' + LineEnding +
               'PLAINTEXT ''$TIME$'' 01 03'),
    (Expr: 'TEXTOUT';
      Help: RsHelpTEXTOUT;
      Example: ';"Hello" written in Arial font:' + LineEnding +
                'TEXTOUT ''Hello'' 2 25 12 ''Arial''' + LineEnding +
                'TEXTOUT ''$TIME$'' 2 5 12 ''Calibri'''),
    (Expr: 'BITMAP';
      Help: RsHelpBITMAP;
      Example: ';relative path:' + LineEnding +
                'BITMAP ''Bitmaps\VFDStudio128x17.bmp'' 0 16' + LineEnding +
                ';absolute path:' + LineEnding +
                'BITMAP ''c:\mybitmap.bmp'' 12 34'),
    (Expr: 'ANIMATE';
      Help: RsHelpANIMATE;
      Example: ';New frame each 500ms:' + LineEnding +
                'ANIMATE ''ani\frog.bmp'' 500 12 4 64' + LineEnding +
                ';Play a single sequence:' + LineEnding +
                'ANIMATE ''ani\frog.bmp'' 500 80 4 64 FALSE'),
    (Expr: 'PIXEL';
      Help: RsHelpPIXEL;
      Example: ';Pixel @ x1, y2 on' + LineEnding +
                'PIXEL 1 2' + LineEnding +
                ';Pixel @ x10, y20 off' + LineEnding +
                'PIXEL 10 20 1'),
    (Expr: 'LINE';
      Help: RsHelpLINE;
      Example: ';diagonal line:' + LineEnding +
                'LINE 0 0 127 63' + LineEnding +
                ';horz. inverted line:' + LineEnding +
                'LINE 0 32 127 32 TRUE'),
    (Expr: 'FRAME';
      Help: RsHelpFRAME;
      Example: ';big frame with a' + LineEnding +
                ';smaller frame in it:' + LineEnding +
                'FRAME 0 0 127 63' + LineEnding +
                'FRAME 10 10 117 53'),
    (Expr: 'NOISE';
      Help: RsHelpNOISE;
      Example: ';200 pixels on' + LineEnding +
                'NOISE 200' + LineEnding +
                ';100 pixels off' + LineEnding +
                'NOISE 100 1'),
    (Expr: 'CLOCK';
      Help: RsHelpCLOCK;
      Example: ';The time in Kairo (+2h)' + LineEnding +
                'BITMAP ''bmp\clock.bmp'' 64 0' + LineEnding +
                'CLOCK 120 96 32 14 20 22'),
    (Expr: 'DRIVEUSAGE';
      Help: RsHelpDRIVEUSAGE;
      Example: ';drive usage of C:\' + LineEnding +
                'PLAINTEXT ''C: $NAMEDRIVEC$'' 0 6' + LineEnding +
                'DRIVEUSAGE ''C'' 0 7 10'),
    (Expr: 'RAMMONITOR';
      Help: RsHelpRAMMONITOR;
      Example: 'LINE 0 48 125 48' + LineEnding +
                'LINE 0 56 125 56' + LineEnding +
                'RAMMONITOR 2 7'),
    (Expr: 'CPUMONITOR';
      Help: RsHelpCPUMONITOR;
      Example: 'LINE 0 48 125 48' + LineEnding +
                'LINE 0 56 125 56' + LineEnding +
                'CPUMONITOR 2 7'),
    (Expr: 'WAPLAYBAR';
      Help: RsHelpWAPLAYBAR;
      Example: 'WAPLAYBAR 0 0 60 127 1' + LineEnding),

    (Expr: 'MATRIX';
      Help: RsHelpMATRIX;
      Example: 'NEWSCREEN' + LineEnding +
                'MATRIX 100' + LineEnding +
                'SCREENTIME 90' + LineEnding +
                'ENDSCREEN'),
    (Expr: 'COMMENT';
      Help: RsHelpCOMMENT;
      Example: ';This is a comment' + LineEnding +
               LineEnding +
               ';This cmd won''t be processed:' + LineEnding +
               ';CLEARSCREEN')
     );

  // KEYWORDS
  Keywords: array of TExpression = (
    (Expr: ':';
      Help: RsHelpColon;
      Example: ';Time with blinking ":"' + LineEnding +
               'PLAINTEXT ''$HH$$:$$MM$'' 1 1'),
    (Expr: 'AVERAGECPU';
      Help: RsHelpAVERAGECPU;
      Example: 'PLAINTEXT ''Avg. CPU: $AVERAGECPU$%'' 1 1'),
    (Expr: 'BIOSDATE';
      Help: RsHelpBIOSDATE;
      Example: 'PLAINTEXT ''BIOS is from'' 0 0' + LineEnding +
               'PLAINTEXT ''$BIOSDATE$'' 1 1'),
    (Expr: 'BIOSVENDOR';
      Help: RsHelpBIOSVENDOR;
      Example: 'PLAINTEXT ''BIOS vendor'' 0 0' + LineEnding +
               'PLAINTEXT ''$BIOSVENDOR$'' 1 1'),
    (Expr: 'BIOSVERSION';
      Help: RsHelpBIOSVERSION;
      Example: 'PLAINTEXT ''BIOS version'' 0 0' + LineEnding +
               'PLAINTEXT ''$BIOSVERSION$'' 1 1'),
    (Expr: 'BOARDPRODUCT';
      Help: RsHelpBOARDPRODUCT;
      Example: 'PLAINTEXT ''$BOARDPRODUCT$'' 1 1'),
    (Expr: 'BOARDVENDOR';
      Help: RsHelpBOARDVENDOR;
      Example: 'PLAINTEXT ''$BOARDVENDOR$'' 1 1'),
    (Expr: 'BOARDVERSION';
      Help: RsHelpBOARDVERSION;
      Example: 'PLAINTEXT ''$BOARDVERSION$'' 1 1'),
    (Expr: 'COLORS';
      Help: RsHelpCOLORS;
      Example: 'PLAINTEXT ''$COLORS$'' 1 1'),
    (Expr: 'CPUCORES';
      Help: RsHelpCPUCORES;
      Example: 'PLAINTEXT ''CPU has $CPUCORES$ cores'' 1 1'),
    (Expr: 'CPUFAMILY';
      Help: RsHelpCPUFAMILY;
      Example: 'PLAINTEXT ''CPU family'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUFAMILY$'' 1 1'),
    (Expr: 'CPUIDENT';
      Help: RsHelpCPUIDENT;
      Example: 'PLAINTEXT ''CPU identifier'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUIDENT$'' 1 1'),
    (Expr: 'CPUMAX';
      Help: RsHelpCPUMAX;
      Example: 'PLAINTEXT ''Max. CPU freq'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUMAX$ MHz'' 1 1'),
    (Expr: 'CPUNAME';
      Help: RsHelpCPUNAME;
      Example: 'PLAINTEXT ''CPU name'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUNAME$'' 1 1'),
    (Expr: 'CPUSOCKET';
      Help: RsHelpCPUSOCKET;
      Example: 'PLAINTEXT ''CPU socket'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUSOCKET$'' 1 1'),
    (Expr: 'CPUSPEED';
      Help: RsHelpCPUSPEED;
      Example: 'PLAINTEXT ''CPU freq'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUSPEED$ MHz'' 1 1'),
    (Expr: 'CPUUSAGE';
      Help: RsHelpCPUUSAGE;
      Example: 'PLAINTEXT ''CPU load'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUUSAGE$%'' 1 1'),
    (Expr: 'CPUVENDOR';
      Help: RsHelpCPUVENDOR;
      Example: 'PLAINTEXT ''CPU vendor'' 0 0' + LineEnding +
               'PLAINTEXT ''$CPUVENDOR$'' 1 1'),
    (Expr: 'DATE';
      Help: RsHelpDATE;
      Example: 'PLAINTEXT ''Today is'' 0 0' + LineEnding +
               'PLAINTEXT ''$DATE$'' 1 1'),
    (Expr: 'DAY';
      Help: RsHelpDAY;
      Example: 'PLAINTEXT ''It''''s the $DAY$. of the month'' 1 1'),
    (Expr: 'DOW';
      Help: RsHelpDOW;
      Example: 'PLAINTEXT ''Today is $DOW$'' 1 1'),
    (Expr: 'FREEDRIVE';
      Help: RsHelpFREEDRIVE;
      Example: ';Drive C:\ free space' + LineEnding +
               'PLAINTEXT ''$FREEDRIVEC$ GB'' 1 1'),
    (Expr: 'FREEMEM';
      Help: RsHelpFREEMEM;
      Example: ';Free memory' + LineEnding +
               'PLAINTEXT ''$FREEMEM$ MB'' 1 1'),
    (Expr: 'FREQ';
      Help: RsHelpFREQ;
      Example: 'PLAINTEXT ''Monitor: $FREQ$'' 1 1'),
    (Expr: 'HH';
      Help: RsHelpHH;
      Example: 'PLAINTEXT ''Time: $HH$:$MM$:$SS$'' 1 1'),
    (Expr: 'IP';
      Help: RsHelpIP;
      Example: 'PLAINTEXT ''My IP: $IP$'' 1 1'),
    (Expr: 'MEMORY';
      Help: RsHelpMEMORY;
      Example: ';Total memory' + LineEnding +
               'PLAINTEXT ''$MEMORY$ MB'' 1 1'),
    (Expr: 'MEMUSAGE';
      Help: RsHelpMEMUSAGE;
      Example: 'PLAINTEXT ''Memory load: $MEMUSAGE$%'' 1 1'),
    (Expr: 'MM';
      Help: RsHelpMM;
      Example: 'PLAINTEXT ''Time: $HH$:$MM$:$SS$'' 1 1'),
    (Expr: 'MON';
      Help: RsHelpMON;
      Example: 'PLAINTEXT ''It''''s the $MON$. month'' 1 1'),
    (Expr: 'NAMEDRIVE';
      Help: RsHelpNAMEDRIVE;
      Example: ';Drive C:\ name' + LineEnding +
               'PLAINTEXT ''$NAMEDRIVEC$'' 1 1'),
    (Expr: 'OS';
      Help: RsHelpOS;
      Example: 'PLAINTEXT ''OS: $OS$'' 1 1'),
    (Expr: 'OSNAME';
      Help: RsHelpOSNAME;
      Example: 'PLAINTEXT ''OS: $OSNAME$'' 1 1'),
    (Expr: 'PCNAME';
      Help: RsHelpPCNAME;
      Example: 'PLAINTEXT ''Computer: $PCNAME$'' 1 1'),
    (Expr: 'RECENTD3D';
      Help: RsHelpRECENTD3D;
      Example: 'PLAINTEXT ''Recent D3D:'' 0 0' + LineEnding +
               'PLAINTEXT ''$RECENTD3D$'' 1 1'),
    (Expr: 'RECENTDD';
      Help: RsHelpRECENTDD;
      Example: 'PLAINTEXT ''Recent DD:'' 0 0' + LineEnding +
               'PLAINTEXT ''$RECENTDD$'' 1 1'),
    (Expr: 'REGKEY_';
      Help: RsHelpREGKEY_;
      Example: ';should be something' + LineEnding +
               ';like "AT/AT compatible"' + LineEnding +
               'PLAINTEXT ''$REGKEY_HARDWARE\DESCRIPTION\System/Identifier$'' 1 1'),
    (Expr: 'RES';
      Help: RsHelpRES;
      Example: 'PLAINTEXT ''Monitor: $RES$'' 1 1'),
    (Expr: 'SLOTINFO';
      Help: RsHelpSLOTINFO;
      Example: 'PLAINTEXT ''1. Slot: $SLOTINFO0$'' 1 1'),
    (Expr: 'SLOTTYPE';
      Help: RsHelpSLOTTYPE;
      Example: 'PLAINTEXT ''1. Slot: $SLOTTYPE0$'' 1 1'),
    (Expr: 'SLOTUSAGE';
      Help: RsHelpSLOTUSAGE;
      Example: 'PLAINTEXT ''1. Slot: $SLOTUSAGE0$'' 1 1'),
    (Expr: 'SS';
      Help: RsHelpSS;
      Example: 'PLAINTEXT ''Time: $HH$:$MM$:$SS$'' 1 1'),
    (Expr: 'TIME';
      Help: RsHelpTIME;
      Example: 'PLAINTEXT ''Time: $TIME$'' 1 1'),
    (Expr: 'TIMEZONE';
      Help: RsHelpTIMEZONE;
      Example: 'PLAINTEXT ''Timezone:'' 0 0' + LineEnding +
                'PLAINTEXT ''$TIMEZONE$'' 1 1'),
    (Expr: 'TOTALDRIVE';
      Help: RsHelpTOTALDRIVE;
      Example: ';Drive C:\ total space' + LineEnding +
               'PLAINTEXT ''$TOTALDRIVEC$ GB'' 1 1'),
    (Expr: 'UPTIME';
      Help: RsHelpUPTIME;
      Example: 'PLAINTEXT ''Uptime:'' 0 0' + LineEnding +
                'PLAINTEXT ''$UPTIME$'' 1 1'),
    (Expr: 'USERNAME';
      Help: RsHelpUSERNAME;
      Example: 'PLAINTEXT ''This is $USERNAME$!'' 1 1'),
    (Expr: 'VERSION';
      Help: RsHelpVERSION;
      Example: 'PLAINTEXT ''VFD-Studio version:'' 0 0' + LineEnding +
                'PLAINTEXT ''$VERSION$'' 1 1'),
    (Expr: 'WALENGTH';
      Help: RsHelpWALENGTH;
      Example: 'PLAINTEXT ''Winamp $WAVERSION$'' 0 0' + LineEnding +
               'PLAINTEXT ''$WATITLE$'' 1 1' + LineEnding +
               'PLAINTEXT ''Pos $WAPOS$ / $WALENGTH$'' 1 2'),
    (Expr: 'WAPOS';
      Help: RsHelpWAPOS;
      Example: 'PLAINTEXT ''Winamp $WAVERSION$'' 0 0' + LineEnding +
               'PLAINTEXT ''$WATITLE$'' 1 1' + LineEnding +
               'PLAINTEXT ''Pos $WAPOS$ / $WALENGTH$'' 1 2'),
    (Expr: 'WATITLE';
      Help: RsHelpWATITLE;
      Example: 'PLAINTEXT ''Winamp $WAVERSION$'' 0 0' + LineEnding +
               'PLAINTEXT ''$WATITLE$'' 1 1' + LineEnding +
               'PLAINTEXT ''Pos $WAPOS$ / $WALENGTH$'' 1 2'),
    (Expr: 'WAVERSION';
      Help: RsHelpWAVERSION;
      Example: 'PLAINTEXT ''Winamp $WAVERSION$'' 0 0' + LineEnding +
               'PLAINTEXT ''$WATITLE$'' 1 1' + LineEnding +
               'PLAINTEXT ''Pos $WAPOS$ / $WALENGTH$'' 1 2'),
    (Expr: 'YEAR';
      Help: RsHelpYEAR;
      Example: 'PLAINTEXT ''It''''s the year $YEAR$!'' 1 1'),
    (Expr: 'OHM';
      Help: RsHelpOHM;
      Example: 'PLAINTEXT ''CPU Temp: $OHM|/intelcpu/0|Temperature|CPU Package$'' 1 1')
    );

