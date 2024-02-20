/*
 * IMPORTANT!
 * 1. Select the correct board in the Arduino IDE first:
 *    Menu > Tools > Board > ESP8266 Boards (2.7.4) > LOLIN(WEMOS) D1 mini Pro
 * 2. Then select the port.
 * 3. Then compile (Ctrl + R)
 * 4. Then upload (Ctrl + U)
 */

/*
#ifndef ARDUINO_ESP8266_WEMOS_D1MINI_PRO
  #error Select the LOLIN(WEMOS) D1 mini Pro board in the Arduino IDE
#endif
*/

#include <Arduino.h>
#include <U8g2lib.h>

#include "glyph.h"

const char idStr[] = "Arduino driver for U8g2 compatible displays";
const char verStr[] = "v0.0.0.0";
const char helpStr[] = "I = identify driver\nV = Get SW version\nCxy = send command with xy = hex code\nDxy = send data with xy = hex code\n? = this help text";
const char errorStr[] = "ERR";

// this program uses software controlled flow control for serial communication using XON/XOFF characters
const byte XON = 0x11;  // ASCII value of XON
const byte XOFF = 0x13; // ASCII value of XOFF


#ifdef U8X8_HAVE_HW_SPI
#include <SPI.h>
#endif
#ifdef U8X8_HAVE_HW_I2C
#include <Wire.h>
#endif



//U8G2_SSD1309_128X64_NONAME0_F_4W_SW_SPI u8g2(U8G2_R0, /* clock=*/ 14, /* data=*/ 13, /* cs=*/ 15, /* dc=*/ 4, /* reset=*/ 5);  
U8G2_SSD1309_128X64_NONAME0_F_4W_HW_SPI u8g2(U8G2_R0, /* cs=*/ 15, /* dc=*/ 4, /* reset=*/ 5);  


void initializeDisplay()
{
  u8g2.clear();
  for (unsigned char y=0; y < (u8g2.getDisplayHeight()/8); y++) // for each row
  {
    for(unsigned char x=0; x < u8g2.getDisplayWidth(); x++) // for each x position
    {
      u8g2.drawPixel(x, y * 8 + random(8));
    }
  }  
  u8g2.sendBuffer(); // transfer internal memory to the display  
}

void drawPixelBlock(unsigned char pixels, int x, int y)
{
  for(unsigned char i=0; i < 8; i++) // for each pixel in the block
  {
    unsigned char mask = 1 << i;
    if (mask & pixels)
    {
      u8g2.setDrawColor(1);
    } else {
      u8g2.setDrawColor(0);
    }
    u8g2.drawPixel(x, y + i);
  } 
}

void drawCharacter(unsigned char c, int col, int row)
{
  for (unsigned char cCol=0; cCol<GLYPH_W; cCol++)
  {
    unsigned char pixels = charMap8x6[c][cCol];
    drawPixelBlock(pixels, col * (GLYPH_W + GLYPH_GAP) + cCol, row * GLYPH_H);  
  }
}

void sendInvalidInputResponse(String command)
{
  Serial.print(errorStr);
  Serial.print(" invalid input (");
  Serial.print(command);
  Serial.println(")");
}

void splitString(String input, char separator, String* parts, int maxParts) {
  int index = 0;
  int lastIndex = 0;
  int partCount = 0;

  while (index < input.length() && partCount < maxParts - 1) {
    if (input[index] == separator) {
      parts[partCount++] = input.substring(lastIndex, index);
      lastIndex = index + 1;
    }
    index++;
  }

  if (lastIndex < input.length() && partCount < maxParts) {
    parts[partCount++] = input.substring(lastIndex);
  }

  while (partCount < maxParts) {
    parts[partCount++] = "";
  }
}

void processCommand(String command)
{
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
        Serial.println(idStr);
        break;
      case 'V':
        // return version
        Serial.println(verStr);
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
            Serial.print(errorStr);
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
        Serial.println(helpStr);
        break;        
      default:
        Serial.print(errorStr);
        Serial.print(" unknown cmd '");
        Serial.print(command);
        Serial.println("'");
        break;
    }
  } else {
    Serial.print(errorStr);
    Serial.print(" incorrect cmd length (");
    Serial.print(command.length());
    Serial.print(" bytes) ");
    Serial.println(command);
  }
}


void setup() 
{
  pinMode(LED_BUILTIN, OUTPUT);
  digitalWrite(LED_BUILTIN, HIGH);

  Serial.begin(115200);
  Serial.println(" ");
  //Serial.begin(230400);


  u8g2.begin();

  //u8g2.setFont(u8g2_font_ncenB08_tr); // choose a suitable font

  u8g2.clearBuffer();          // clear the internal memory
  initializeDisplay();


  Serial.print(idStr);
  Serial.print(' ');
  Serial.println(verStr);

  randomSeed(analogRead(0));	
}

void loop() 
{
  yield(); // tell the watchdog we are still alive

  if(Serial.available() > 0)
  {
    if(Serial.available() > 48)
    {
      digitalWrite(LED_BUILTIN, LOW); // LED is on when the Arduino needs some time to process the serial data
      Serial.write(XOFF);
    }
    
    String input = Serial.readStringUntil('\n');
    input.toUpperCase();
    processCommand(input);
    
    digitalWrite(LED_BUILTIN, HIGH);
    Serial.write(XON);
  }

}
