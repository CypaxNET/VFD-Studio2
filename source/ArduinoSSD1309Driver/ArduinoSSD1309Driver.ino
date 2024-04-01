/*!
 * @brief         Arduino driver for U8g2 compatible displays
 * 
 * @details       This sketch belongs to the VFD-Studio 2 project:
 *                https://github.com/CypaxNET/VFD-Studio2/
 *                The specific code in this sketch is made for a
 *                LOLIN(WEMOS) D1 mini Pro Arduino board and a 
 *                DIYMORE SSD1309 128X64 OLED display.
 *                For another display you need to change the 
 *                u8g2-constructor in the global variables section.
 *                For another Arduino board you need to select the 
 *                correct board in the Arduino IDE.
 *                Also make sure to connect the display to the Arduino
 *                properly.
 *                Visit https://github.com/CypaxNET/VFD-Studio2/ to 
 *                learn how to adapt this sketch to another display
 *                or Arduino board.
 * 
 *    IMPORTANT!
 *    1. First select the correct board in the Arduino IDE:
 *       Menu > Tools > Board > ESP8266 Boards (2.7.4) > LOLIN(WEMOS) D1 mini Pro
 *    2. Then select the port
 *    3. Then compile (Ctrl + R)
 *    4. Then upload (Ctrl + U)
 *
 *    Sketch function:
 *    Receives command strings in ASCII format via serial interface and forwards them 
 *    accordingly to an U8g2 compatible display.
 *    Uses software-controlled flow control on the serial interface. Baud rate 115200.
 *
 *    Controller pins:
 *    Uses D1, D2 and D8 as ouputs for the display.
 *    Uses SCLK and MOSI as outputs for hardware SPI interface.
 *    Uses not connected analog inputs A0 to initialize the randomizer.
 *    The built-in LED (D3) lights up when the serial receive buffer of the Arduino becomes full.
 *    
 *    Circuit:
 *    | Display pin (function) | Arduino Nano pin (function) |
 *    | ---------------------- | --------------------------- |
 *    | 1 (GND)                | GND                         |
 *    | 2 (3.3V)               |                             |
 *    | 3 (SCL)                | D5 (SCLK)                   |
 *    | 4 (SDA)                | D7 (MOSI)                   |
 *    | 5 (RES)                | D1 (GPIO5)                  |
 *    | 6 (DC)                 | D2 (GPIO4)                  |
 *    | 7 (CS)                 | D8 (GPIO15)                 |
 *    Note that this display requires an own 3.3V supply. The 3.3V on the Arduino 
 *    board cannot power the display as it draws too much current.
 *    But you can supply an additonal 3.3V regulator for the display from the 5V 
 *    pin of the Arduino board.
 * 
 * @copyright     Created February 2024 by Cypax, https://cypax.net
*/

/******************************************************************************/
/* Library includes                                                           */
/******************************************************************************/
#include <Arduino.h>
#include <U8g2lib.h>
#ifdef U8X8_HAVE_HW_SPI
#include <SPI.h>
#endif
#ifdef U8X8_HAVE_HW_I2C
#include <Wire.h>
#endif

/******************************************************************************/
/* Own includes                                                               */
/******************************************************************************/
#include "glyph.h"  // character set

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
#define XOFF_THRESHOLD 48  


/* ******** Communication stuff ******** */

const char kIdStr[] =      "Arduino driver for U8g2 compatible displays";
const char kVersionStr[] = "v1.0.0.0";
const char kHelpStr[] =    "I = identify driver\n"\
                           "V = get software version\n"\
                           "R = (re)initialize display\n"\
                           "X = clear display\n"\
                           "U = update display content\n"\
                           "T v = set contrast / brightness\n"\
                           "B p x y = draw vertical block of pixels\n"\
                           "L l x y = draw letter\n"\
                           "SP x y = draw pixel\n"\
                           "CP x y = clear pixel\n"\
                           "SL x0 y0 x0 y0 = draw line\n"\
                           "CL x0 y0 x0 y0 = clear line\n"\
                           "SF x0 y0 x0 y0 = draw frame\n"\
                           "CF x0 y0 x0 y0 = clear frame\n"\
                           "SB x0 y0 x0 y0 = draw filled box\n"\
                           "CB x0 y0 x0 y0 = clear box\n"\
                           "? = this help text\n"\
                           "!!All parameters must be in hex!!";
const char kErrorStr[] =   "ERR";


/******************************************************************************/
/* Global variables                                                           */
/******************************************************************************/

// A display constructor using software SPI:
//U8G2_SSD1309_128X64_NONAME0_F_4W_SW_SPI u8g2(U8G2_R0, /* clock=*/ 14, /* data=*/ 13, /* cs=*/ 15, /* dc=*/ 4, /* reset=*/ 5);  
// A display constructor using hardware SPI:
U8G2_SSD1309_128X64_NONAME0_F_4W_HW_SPI u8g2(U8G2_R0, /* cs=*/ 15, /* dc=*/ 4, /* reset=*/ 5);  

/******************************************************************************/
/* Declaration of own functions                                               */
/******************************************************************************/

/* primary functionality */
void processCommand(String command);


/* display stuff */
void initializeDisplay();
void drawPixelBlock(unsigned char pixels, int x, int y);
void drawCharacter(unsigned char c, int col, int row);

/* helpers */
void splitString(String input, char separator, String* parts, int maxParts);
void sendInvalidInputResponse(String command);


/******************************************************************************/
/* Arduino sketch functions                                                   */
/******************************************************************************/

/*
 * @brief        Setup code to be run once on power-on or after reset.
 * 
 * @description  Initializes the serial interface and the u8g2 display.
 */
void setup()
{
  pinMode(LED_BUILTIN, OUTPUT); // built-in LED will be used to indicate serial flow control
  digitalWrite(LED_BUILTIN, HIGH); // on the D1 mini PRO the LED is low active

  Serial.begin(115200);
  Serial.println(" ");
  // identify self
  Serial.print(kIdStr);
  Serial.print(' ');
  Serial.println(kVersionStr);

  // initialize the display
  u8g2.begin();
  u8g2.clearBuffer(); // clear the internal memory
  initializeDisplay();
  
  // initialize the randomizer using a floating (unconnected) analog input
  randomSeed(analogRead(0));	
}

/*
 * @brief        Code to be run repeatedly.
 * @description  Reads lines of incomming serial data and passes them to processCommand().
 *               Manages software flow control.
 */
void loop()
{
  yield(); // tell the watchdog we are still alive

  if(Serial.available() > 0)
  {
    if(Serial.available() > XOFF_THRESHOLD)
    {
      digitalWrite(LED_BUILTIN, LOW); // LED is on when the Arduino needs some time to process the serial data
      Serial.write((byte)XOFF);
    }
    
    String input = Serial.readStringUntil('\n');
    digitalWrite(LED_BUILTIN, LOW); // LED is on when the Arduino needs some time to process the serial data
    Serial.write((byte)XOFF);
    processCommand(input);
    
    digitalWrite(LED_BUILTIN, HIGH);
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
 *               from the command and from each other by single spaces.
 *               In order to generally keep commands short, numbers are passed 
 *               in hexadecimal form.
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
      case 'R':
        // display reinitialization
        initializeDisplay();
        break;
      
      case 'X':
        // clear display
        u8g2.clear();
        break;
      
      case 'U':
        // update display from buffer
        u8g2.sendBuffer();
        break;
      
      case 'I':
        // return identification
        Serial.println(kIdStr);
        break;
      
      case 'V':
        // return version
        Serial.println(kVersionStr);
        break;
      
      case 'T':
        // set display brightness or contrast
        {
          String tokens[4] = {"", ""}; // should be 2 parts: [T] [contrast]
          char* endptr[1];
          unsigned char contrast = 0;
          splitString(command, ' ', tokens, 2);            
          contrast = strtoul(tokens[1].c_str(), &endptr[0], 16);
          if ('\0' == *endptr[0])
          {
            u8g2.setContrast(contrast);
          } else {
            sendInvalidInputResponse(command);
          }
        }
        break;        
      
      case 'L':
        // draw a letter
        {
          String tokens[4] = {"", "", "", ""}; // should be 4 parts: [L] [Letter] [X] [Y]
          char* endptr[3];
          unsigned char letter = 0;
          unsigned char col = 0;
          unsigned char row = 0;
          splitString(command, ' ', tokens, 4);            
          letter = strtol(tokens[1].c_str(), &endptr[0], 16);
          col =    strtol(tokens[2].c_str(), &endptr[1], 16);
          row =    strtol(tokens[3].c_str(), &endptr[2], 16);
          if (('\0' == *endptr[0]) && ('\0' == *endptr[1]) && ('\0' == *endptr[2]))
          {
            drawCharacter(letter, col, row);
          } else {          
            sendInvalidInputResponse(command);
          }
        }
        break;
      
      case 'C':
      case 'S':
        // clear (C) or set (S) pixels
        {
          char gfxFunction = command.charAt(1);
          switch (gfxFunction)
          {
            case 'P':
              // set/clear pixel        
              {
                String tokens[3] = {"", "", ""}; // should be 3 parts: [CP|SP] [X] [Y]
                char* endptr[2];
                unsigned char x = 0;
                unsigned char y = 0;
                splitString(command, ' ', tokens, 3);            
                x = strtol(tokens[1].c_str(), &endptr[0], 16);
                y = strtol(tokens[2].c_str(), &endptr[1], 16);
                if ( ('\0' == *endptr[0]) && ('\0' == *endptr[1]))
                {
                  // set or clear?
                  if ('S' == action)
                  {
                    u8g2.setDrawColor(1);
                  } else {
                    u8g2.setDrawColor(0);
                  }              
                  u8g2.drawPixel(x, y);
                } else {
                  sendInvalidInputResponse(command);
                }
              }
              break;
            case 'L': // set/clear line
            case 'F': // set/clear frame
            case 'B': // set/clear box
              {
                String tokens[5] = {"", "", "", "", ""}; // should be 5 parts: [CP|SP] [X0] [Y0] [X1] [Y1]
                char* endptr[4];
                unsigned char x0 = 0;
                unsigned char y0 = 0;
                unsigned char x1 = 0;
                unsigned char y1 = 0;
                splitString(command, ' ', tokens, 5);         
                x0 = strtol(tokens[1].c_str(), &endptr[0], 16);
                y0 = strtol(tokens[2].c_str(), &endptr[1], 16);
                x1 = strtol(tokens[3].c_str(), &endptr[2], 16);
                y1 = strtol(tokens[4].c_str(), &endptr[3], 16);
                if (('\0' == *endptr[0]) && ('\0' == *endptr[1]) && ('\0' == *endptr[2]) && ('\0' == *endptr[3]))
                {
                  // set or clear?
                  if ('S' == action)
                  {
                    u8g2.setDrawColor(1);
                  } else {
                    u8g2.setDrawColor(0);
                  }
                  if ('L' == gfxFunction)
                    u8g2.drawLine(x0, y0, x1, y1);
                  if ('F' == gfxFunction)
                    u8g2.drawFrame(x0, y0, x1-x0, y1-y0);
                  if ('B' == gfxFunction)
                    u8g2.drawBox(x0, y0, x1-x0, y1-y0);
                } else {
                  sendInvalidInputResponse(command);
                }
              }
              break;
            default:
            Serial.print(kErrorStr);
            Serial.print(" unknown cmd '");
            Serial.print(command);
            Serial.println("'");
            break;          
          } // switch(gfxFunction)
          
        }
        break;        
      
      case 'B':
        // it's a vertical block of 8 pixels
        {
          String tokens[4] = {"", "", "", ""}; // should be 4 parts: [B] [Pixels] [X] [Y]
          char* endptr[3];
          unsigned char pixels = 0;
          unsigned char x = 0;
          unsigned char y = 0;
          splitString(command, ' ', tokens, 4);            
          pixels = strtoul(tokens[1].c_str(), &endptr[0], 16);
          x =      strtol(tokens[2].c_str(), &endptr[1], 16);
          y =      strtol(tokens[3].c_str(), &endptr[2], 16);
          if (('\0' == *endptr[0]) && ('\0' == *endptr[1]) && ('\0' == *endptr[2]))
          {
            drawPixelBlock(pixels, x, y);
          } else {
            sendInvalidInputResponse(command);
          }
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
 * @brief        Initializes the display.
 * @description  This function will draw a random pattern on the display
 *               so we can observe if the display is working at all.
 */
void initializeDisplay()
{
  u8g2.clear();
  for (unsigned char y = 0; y < (u8g2.getDisplayHeight()/8); y++) // for each row
  {
    for(unsigned char x = 0; x < u8g2.getDisplayWidth(); x++) // for each x position
    {
      u8g2.drawPixel(x, y * 8 + random(8));
    }
  }  
  u8g2.sendBuffer(); // transfer internal memory to the display  
}

/*
 * @brief        Draws a vertical 1x8 block of pixels.
 * @description  This function is used when displaying bitmap data sent from VFD-Studio 2.
 */
void drawPixelBlock(unsigned char pixels, int x, int y)
{
  for(unsigned char i=0; i < 8; i++) // for each pixel in the block
  {
    unsigned char mask = 1 << i;
    if (mask & pixels)
    {
      u8g2.setDrawColor(1);
    } 
    else
    {
      u8g2.setDrawColor(0);
    }
    u8g2.drawPixel(x, y + i);
  } 
}

/*
 * @brief        Draws character on the display.
 * @description  Uses the charmap in glyph.h instead of one of the u8g2 fonts beacuse
 *               we want the VFD-Studio 2 preview display to match with with this one.
 */
void drawCharacter(unsigned char c, int col, int row)
{
  for (unsigned char cCol=0; cCol<GLYPH_W; cCol++)
  {
    unsigned char pixels = charMap8x6[c][cCol];
    drawPixelBlock(pixels, col * (GLYPH_W + GLYPH_GAP) + cCol, row * GLYPH_H);  
  }
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
 *  
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
