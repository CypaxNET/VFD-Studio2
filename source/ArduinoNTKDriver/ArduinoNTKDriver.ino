/*!
 * @brief         Arduino driver for Noritake GU 300/800 VFD series
 * 
 * @details       This sketch is intended for the Arduino Nano and belongs to 
 *                the VFD-Studio 2 project:
 *                https://github.com/CypaxNET/VFD-Studio2/
 * 
 *    IMPORTANT!
 *    1. First select the correct board in the Arduino IDE:
 *       Menu > Tools > Board > Arduino AVR Boards > Arduino Nano
 *    2. Then select the port
 *    3. Then compile (Ctrl + R)
 *    4. Then upload (Ctrl + U)
 *
 *    Sketch function:
 *    Receives command strings in ASCII format via serial interface and forwards them 
 *    accordingly to a Noritake Itron GU-800 or GU-300 series VFD.
 *    Uses software-controlled flow control on the serial interface. Baud rate 115200.
 *
 *    Controller pins:
 *    Uses PB0..PB4 and PD5..PD7 as ouputs for the display data pins.
 *    Uses PD2..PD4 as control as outputs for the display control pins /RD, C/D and /WR.
 *    Uses not connected analog inputs A0 and A1 to initialize the randomizer.
 *    The built-in LED (PB5) lights up when the serial receive buffer of the Arduino becomes full.
 *    
 *    Circuit:
 *    | Display pin (function) | Arduino Nano pin (function) |
 *    | ---------------------- | --------------------------- |
 *    | 1  (Data7)             | D7  (PD7)                   |
 *    | 3  (Data6)             | D6  (PD6)                   |
 *    | 5  (Data5)             | D5  (PD5)                   |
 *    | 7  (Data4)             | D12 (PB4)                   |
 *    | 9  (Data3)             | D11 (PB3)                   |
 *    | 11 (Data2)             | D10 (PB2)                   |
 *    | 13 (Data1)             | D9  (PB1)                   |
 *    | 15 (Data0)             | D8  (PB0)                   |
 *    | 17 (/WR)               | D4  (PD4)                   |
 *    | 19 (C/D)               | D3  (PD3)                   |
 *    | 21 (/RD)               | D2  (PD2)                   |
 *    | 23 (/CS)               | GND                         |
 *    | 2..24                  | GND                         |
 * 
 * 
 * @copyright     Created February 2024 by Cypax, https://cypax.net
*/

#ifndef ARDUINO_AVR_NANO
  #error Select the Arduino Nano board in the Arduino IDE
#endif

/******************************************************************************/
/* Library includes                                                           */
/******************************************************************************/
#include <Arduino.h>

/******************************************************************************/
/* Own includes                                                               */
/******************************************************************************/
#include "ntk_commands.h" // include display commands

/******************************************************************************/
/* Constants and #defines                                                     */
/******************************************************************************/

/* ******** Serial interface stuff ******** */

// This program uses software controlled flow control for serial communication 
// using XON/XOFF characters:
#define XON  0x11 // ASCII value of XON character
#define XOFF 0x13 // ASCII value of XOFF character

// Number of serial bytes receivable in the buffer before XOFF is sent. 
// Actually the buffer size is 64 bytes by default in Arduino, but it's not 
// wrong to raise the flag a little bit earlier:
#define XOFF_THRESHOLD 16  



/* ******** Communication stuff ******** */

const char kIdStr[] =      "Arduino driver for Noritake GU 300/800 VFD series";
const char kVersionStr[] = "v1.0.0.0";
const char kHelpStr[] =    "I = identify driver\n"\
                           "V = get software version\n"\
                           "3 w h = initialize GU-300 series mode with w h = display resolution in hexadecimal\n"\
                           "8 w h = initialize GU-800 series mode with w h = display resolution in hexadecimal\n"\
                           "R = reinitialize GU-800 display\n"\
                           "X = clear GU-300 display\n"\
                           "Cxy = send command with xy = hex code\n"\
                           "Dxy = send data with xy = hex code\n"\
                           "? = this help text";
const char kErrorStr[] =   "ERR";



/* ******** GPIO stuff ******** */

// Unfortunately we don't have a full port available on the Arduino Nano for the 
// data pins, so we must split it to port B and D. We'll use bit masks to address
// port B and port D directly:
#define MASK_PORT_B B00011111
#define MASK_PORT_D B11100000

//Pins for display control lines:
#define PIN_RD 2  // connect RD (pin 21) of the display with PD2 (pin D2) of the Arduino
#define PIN_CD 3  // connect CD (pin 19) of the display with PD3 (pin D3) of the Arduino
#define PIN_WR 4  // connect WR (pin 17) of the display with PD4 (pin D4) of the Arduino
                  // also connect CSS (pin 23) of the display with GND

/******************************************************************************/
/* Global variables                                                           */
/******************************************************************************/

// default display resolution, can be set with the '3' and '8' commands
uint16_t u16DspWidth = 128;
uint16_t u16DspHeight = 64; // must be divisible by 8!

/******************************************************************************/
/* Declaration of own functions                                               */
/******************************************************************************/

/* primary functionality */
void processCommand(String command);

/* low level display control functions */
void sendCmd(unsigned char cmd);
void sendData(unsigned char dat);

/* display stuff */
void initDisplay800(uint16_t width, uint16_t height);
void initDisplay300(uint16_t width, uint16_t height);
void clearScreen300(unsigned char screen);

/* helpers */
void splitString(String input, char separator, String* parts, int maxParts);
void sendInvalidInputResponse(String command);

/******************************************************************************/
/* Arduino sketch functions                                                   */
/******************************************************************************/

/*
 * @brief        Setup code to be run once on power-on or after reset.
 * 
 * @description  Initializes GPIOs and the serial interface.
 */
void setup()
{
  // to avoid problems with the display controller the very first thing to do 
  // is to activate pullups on the IOs which leat to the controll pins
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  

  // configure outputs
  DDRB |= B00011111; // set PB0..PB4 which are D0..D4 to output
  DDRD |= B11100000; // set PD5..PD7 which are D5..D7 to output
  DDRD |= B00011100; // set PD2..PD4 which are /RD, C/D and /WR to output

  // set outputs initial state
  PORTB |= B00011111; // set D0..D4 to HIGH
  PORTD |= B11100000; // set D5..D7 to HIGH
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  

  pinMode(LED_BUILTIN, OUTPUT); // built-in LED will be used to indicate serial flow control

  Serial.begin(115200);
  // identify self
  Serial.print(kIdStr);
  Serial.print(' ');
  Serial.println(kVersionStr);

  // initialize the randomizer using floating (unconnected) analog inputs
  randomSeed(analogRead(0) + analogRead(1));
}

/*
 * @brief        Code to be run repeatedly.
 * @description  Reads lines of incomming serial data and passes them to processCommand().
 *               Manages software flow control.
 */
void loop()
{
  if(Serial.available() > 0)
  {
    if(Serial.available() > XOFF_THRESHOLD)
    {
      digitalWrite(LED_BUILTIN, HIGH); // LED is on when the Arduino needs some time to process the serial data
      Serial.write((byte)XOFF);
    }
    
    String input = Serial.readStringUntil('\n');
    processCommand(input);
    
    digitalWrite(LED_BUILTIN, LOW);
    Serial.write((byte)XON); // ready to receive more serial data
  }
}


/******************************************************************************/
/* Own functions                                                              */
/******************************************************************************/

/*
 * @brief        Processes a command received via serial interface and initiates 
 *               appropriate actions.
 * @description  Communication via serial interface is ASCII-based. Although 
 *               this makes the data load longer, it makes debugging much
 *               easier and enables software flow control.
 *               Some commands have parameters. Command parameters are separated 
 *               from the command and from each other by single spaces, except 
 *               for the 'C' and 'D' commands as these shall be as compact as 
 *               possible.
 *               In order to generally keep commands short, numbers are passed 
 *               in hexadecimal form - e.g. "8 80 40" inits a GU-800 with 128x64
 *               pixels.
 *               Commands are not case-sensitive.
 */
void processCommand(String command)
{
  command.toUpperCase();

  if (command.length() >= 1)
  {  
    char action = command.charAt(0);

    switch (action)
    {
      case '3':
      case '8':
        // display (re)initialization
        {
          String tokens[3] = {"", "", ""}; // should be 3 parts: [8] [WIDTH] [HEIGHT]
          char* endptr[2];
          uint16_t width = u16DspWidth; 
          uint16_t height = u16DspHeight;
          splitString(command, ' ', tokens, 3);            
          width = strtoul(tokens[1].c_str(), &endptr[0], 16);
          height =    strtoul(tokens[2].c_str(), &endptr[1], 16);
          if (('\0' == *endptr[0]) && ('\0' == *endptr[1]))
          {
            if ('3' == action)
            {
              initDisplay300(width, height);
            }
            else
            {
              initDisplay800(width, height);
            }
          }
          else
          {          
            sendInvalidInputResponse(command);
          }
        }
        break;        
      
      case 'R':
        // display reinitialization
        initDisplay800(u16DspWidth, u16DspHeight);
        break;
      
      case 'X':
        // clear a layer of the display
        if ('0' == command.charAt(1))
        {
          clearScreen300(0);
        } else {
          clearScreen300(1);
        }
        break;
      
      case 'I':
        // return identification
        Serial.println(kIdStr);
        break;
      
      case 'V':
        // return version
        Serial.println(kVersionStr);
        break;
      
      case 'C':
        // it's a command to be forwarded to the VFD
        
        // check plausibility
        if (
             (command.length() >= 3)
             && 
             ( ((command.charAt(1) >='0') && (command.charAt(1) <='9')) || ((command.charAt(1) >='A') && (command.charAt(1) <='F')) )
             &&
             ( ((command.charAt(2) >='0') && (command.charAt(2) <='9')) || ((command.charAt(2) >='A') && (command.charAt(2) <='F')) )
           )
         {
          unsigned char cmd = (unsigned char)strtoul(command.substring(1,3).c_str(), NULL, 16);
          sendCmd(cmd);
        } else {
          sendInvalidInputResponse(command);
        }
        break;
      
      case 'D':
        // it's data to be forwarded to the VFD
        
        // check plausibility
        if (
             (command.length() >= 3)
             &&
             ( ((command.charAt(1) >='0') && (command.charAt(1) <='9')) || ((command.charAt(1) >='A') && (command.charAt(1) <='F')) )
             &&
             ( ((command.charAt(2) >='0') && (command.charAt(2) <='9')) || ((command.charAt(2) >='A') && (command.charAt(2) <='F')) )
           )
        {
          unsigned char dat = strtoul(command.substring(1,3).c_str(), NULL, 16);
          sendData(dat);
        } else {
          sendInvalidInputResponse(command);
        }
        break;
      
      case '?':
        Serial.println(kHelpStr);
        break;
        
      default:
        Serial.print(kErrorStr);
        Serial.print(" unknown cmd '");
        Serial.print(command);
        Serial.println("'");
        break;
    }
  } else {
    Serial.print(kErrorStr);
    Serial.print(" incorrect cmd length (");
    Serial.print(command.length());
    Serial.print(" bytes) ");
    Serial.println(command);
  }
}

/*
 * @brief Low-level function to send a command byte to the display.
 */
void sendCmd(unsigned char cmd)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to C
  digitalWrite(PIN_CD, HIGH);

  // set command
  PORTB &= (~MASK_PORT_B);
  PORTB |= (MASK_PORT_B & cmd);
  PORTD &= (~MASK_PORT_D);
  PORTD |= (MASK_PORT_D & cmd);

  asm("nop");
  
  // Pulse /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");
  digitalWrite(PIN_WR, HIGH);
}

/*
 * @brief Low-level function to send a data byte to the display.
 */
void sendData(unsigned char dat)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to D
  digitalWrite(PIN_CD, LOW);

  // set data
  PORTB &= (~MASK_PORT_B);
  PORTB |= (MASK_PORT_B & dat);
  PORTD &= (~MASK_PORT_D);
  PORTD |= (MASK_PORT_D & dat);

  asm("nop");
  
  // Pulse /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");
  digitalWrite(PIN_WR, HIGH); 
}


/*
 * @brief        Initializes a GU-800 series VF-display.
 * @description  Since display initialization of the GU-300 and GU-800 series 
 *               VFDs involves either many commands to be sent or a specific 
 *               timing, this function exists to do it from here, independently
 *               from any serial connection limitations.
 *               This function will draw also a random pattern on the display
 *               so we can observe if the display is working at all.
 */
 void initDisplay800(uint16_t width, uint16_t height)
{
  u16DspWidth = width;
  u16DspHeight = height;
    
  Serial.println("800 mode");
    
  sendCmd(VFD_800_DSP_CLEAR);
  delay(2);
  // initialize display areas
  for (unsigned char n=0; n < (u16DspHeight / 8); n++)
  {
    sendCmd(VFD_800_AREA_SET);
    sendCmd(n); // index of area
    sendData(0xFF);
  }

  sendCmd(VFD_800_DSP_ONOFF | VFD_800_DSP_ONOFF_L0 | VFD_800_DSP_ONOFF_L1);  // both layers/screens on
  
  sendCmd(VFD_800_BRIGHTNESS); // full brightness

  // create random pixel pattern on screen0
  for (unsigned char y=0; y < (u16DspHeight / 8); y++) // for each row
  {
    sendCmd(VFD_800_SET_X);
    sendCmd(0x00); // xpos = 0
    sendCmd(VFD_800_SET_Y);
    sendCmd(y);    // ypos = y
    sendCmd(VFD_800_ADRMODE | VFD_800_ADRMODE_INCX); // auto inc x
    
    for(unsigned char x=0; x < u16DspWidth; x++) // for each x position
    {
      sendData(1<<random(8)); // randomly one pixel with a block of eight is on
    }
  }
}

/*
 * @brief        Initializes a GU-300 series VF-display.
 * @description  Since display initialization of the GU-300 and GU-800 series 
 *               VFDs involves either many commands to be sent or a specific 
 *               timing, this function exists to do it from here, independently
 *               from any serial connection limitations.
 *               This function will draw also a random pattern on the display
 *               so we can observe if the display is working at all.
 */
void initDisplay300(uint16_t width, uint16_t height)
{
  u16DspWidth = width;
  u16DspHeight = height;
   
  Serial.println("300 mode");

  uint16_t screen0_addr = 0x0000;
  uint16_t screen1_addr = u16DspHeight / 8 * u16DspWidth; // e.g. 256x64 => 2048 (0x0800)
  uint16_t screen_size = screen1_addr; // note: the start address screen1_addr is also the size of a screen in bytes

  // screen0 starts at screen0_addr
  sendCmd(VFD_300_SET_LOWER_ADDR_1);
  sendData(screen0_addr & 0x00FF);
  sendCmd(VFD_300_SET_UPPER_ADDR_1);
  sendData(screen0_addr >> 8);
  // screen1 starts at 0x0800
  sendCmd(VFD_300_SET_LOWER_ADDR_2);
  sendData(screen1_addr & 0x00FF);
  sendCmd(VFD_300_SET_UPPER_ADDR_2);
  sendData(screen1_addr >> 8);

  sendCmd(VFD_300_CURSOR_HOLD);
  sendCmd(VFD_300_SET_LUMINANCE);

  sendCmd(0x00); // both screens off

  sendCmd(VFD_300_SET_CURSOR_LOW);
  sendData(0x00);
  sendCmd(VFD_300_SET_CURSOR_HIGH);
  sendData(0x00);
  
  sendCmd(VFD_300_CURSOR_INCR);

  sendCmd(VFD_300_DATA_WRITE);

  // create random pixel pattern on screen0
  for(int i = 0; i < screen_size; i++)
  {
    sendData(1<<random(8)); // randomly one pixel with a block of eight is on
  }
  
  sendCmd(VFD_300_CURSOR_HOLD);

  delay(1);

  sendCmd(VFD_300_SET_CURSOR_LOW);
  sendData(screen1_addr & 0x00FF);
  sendCmd(VFD_300_SET_CURSOR_HIGH);
  sendData(screen1_addr >> 8);

  sendCmd(VFD_300_CURSOR_INCR);
  sendCmd(VFD_300_DATA_WRITE);
  
  // fill screen1 RAM with 0-bytes
  for(int i = 0; i < screen_size; i++)
  {
    sendData(0);
  }

  sendCmd(VFD_300_CURSOR_HOLD);
  
  sendCmd(VFD_300_SCREEN1_CHARACTER); // screen1 = text
  sendCmd(VFD_300_OR_DISPLAY); // OR screens

  sendCmd(0x03); // both screens on
}

/*
 * @brief        Clears a layer (screen) of a GU-300 display.
 * @description  Cleaning the screen on the 300 series VFD requires a lot of 
 *               instructions to be sent.
 *               To avoid sending all those via the serial interface, this 
 *               function exists to do it from here directly on the Arduino.
 */
void clearScreen300(unsigned char screen)
{
  uint16_t screen0_addr = 0x0000;
  uint16_t screen1_addr = u16DspHeight / 8 * u16DspWidth; // e.g. 256x64 => 2048 (0x0800)
  uint16_t screen_size = screen1_addr; // note: the start address screen1_addr is also the size of a screen in bytes
  
  if (0 == screen)
  {
    // start at screen0_addr
    sendCmd(VFD_300_SET_CURSOR_LOW);
    sendData(screen0_addr & 0xFF);
    sendCmd(VFD_300_SET_CURSOR_HIGH);
    sendData(screen0_addr >> 8);
  } else {
    // start at screen1_addr
    sendCmd(VFD_300_SET_CURSOR_LOW);
    sendData(screen1_addr & 0xFF);
    sendCmd(VFD_300_SET_CURSOR_HIGH);
    sendData(screen1_addr >> 8);
  }
  
  sendCmd(VFD_300_CURSOR_INCR);
  sendCmd(VFD_300_DATA_WRITE);

  // fill the screen RAM with 0-bytes
  for(int i = 0; i < screen_size; i++)
  {
    sendData(0);
  }
  
  sendCmd(VFD_300_CURSOR_HOLD);  

  Serial.print("Clearscreen ");
  Serial.println(screen);  
}

/* 
 *  @brief        Helper function to split a string into parts, using a separator character.
 *  @description  Apparently, Arduino doesn't provide a library function for this.
 */
void splitString(String input, char separator, String* parts, int maxParts)
{
  int index = 0;
  int lastIndex = 0;
  int partCount = 0;

  while (index < input.length() && partCount < maxParts - 1)
  {
    if (input[index] == separator)
    {
      parts[partCount++] = input.substring(lastIndex, index);
      lastIndex = index + 1;
    }
    index++;
  }

  if (lastIndex < input.length() && partCount < maxParts)
  {
    parts[partCount++] = input.substring(lastIndex);
  }

  while (partCount < maxParts)
  {
    parts[partCount++] = "";
  }
}

/* 
 *  @brief        Helper function to send a "invalid input" response.
 *  @description  Since this is used multiple times we have an own function for it.
 */
void sendInvalidInputResponse(String command)
{
  Serial.print(kErrorStr);
  Serial.print(" invalid input (");
  Serial.print(command);
  Serial.println(")");
}

/**** Last line of source code                                             ****/
