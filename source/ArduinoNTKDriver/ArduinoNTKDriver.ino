/*
 * IMPORTANT!
 * 1. Select the correct board in the Arduino IDE first:
 *    Menu > Tools > Board > Arduino AVR Boards > Arduino Nano
 * 2. Then select the port.
 * 3. Then compile (Ctrl + R)
 * 4. Then upload (Ctrl + U)
 */

#ifndef ARDUINO_AVR_NANO
  #error Select the Arduino Nano board in the Arduino IDE
#endif


const char idStr[] = "Arduino driver for Noritake GU 300/800 VFD series";
const char verStr[] = "v0.2.0.0";
const char helpStr[] = "I = identify driver\nV = Get SW version\n3 = initialize 300 series mode\n8 = initialize 800 series mode\nCxy = send command with xy = hex code\nDxy = send data with xy = hex code\n? = this help text";
const char errorStr[] = "ERR";

// unfortunately we don't have a full port available on the Arduino Nano for the data pins, so we must split it to port B and D:
const unsigned char MASK_PORTB = B00011111;
const unsigned char MASK_PORTD = B11100000; 

// this program uses software controlled flow control for serial communication using XON/XOFF characters
const byte XON = 0x11;  // ASCII value of XON
const byte XOFF = 0x13; // ASCII value of XOFF

// pinout configuration:
#define PIN_RD 2  // connect RD (pin 21) of the display with PD2 (pin D2) of the Arduino
#define PIN_CD 3  // connect CD (pin 19) of the display with PD3 (pin D3) of the Arduino
#define PIN_WR 4  // connect WR (pin 17) of the display with PD4 (pin D4) of the Arduino
                  // also connect CSS (pin 23) of the display with GND

// 800 series commands
#define VFD_800_DSP_CLEAR     0x5F
#define VFD_800_AREA_SET      0x62
#define VFD_800_DSP_ONOFF     0x20
#define VFD_800_DSP_ONOFF_L0  0x04
#define VFD_800_DSP_ONOFF_L1  0x08
#define VFD_800_BRIGHTNESS    0x40
#define VFD_800_SET_X         0x64
#define VFD_800_SET_Y         0x60
#define VFD_800_ADRMODE       0x80
#define VFD_800_ADRMODE_INCX  0x04
#define VFD_800_ADRMODE_INCY  0x02

// 300 series commands
#define VFD_300_SET_LOWER_ADDR_1 0x0A
#define VFD_300_SET_UPPER_ADDR_1 0x0B
#define VFD_300_SET_LOWER_ADDR_2 0x0C
#define VFD_300_SET_UPPER_ADDR_2 0x0D

#define VFD_300_CURSOR_INCR 0x04
#define VFD_300_CURSOR_HOLD 0x05

#define VFD_300_SCREEN2_CHARACTER 0x06
#define VFD_300_SCREEN2_GRAPHICS  0x07

#define VFD_300_DATA_WRITE 0x08
#define VFD_300_DATA_READ  0x09

#define VFD_300_SET_CURSOR_LOW  0x0E
#define VFD_300_SET_CURSOR_HIGH 0x0F

#define VFD_300_OR_DISPLAY  0x10
#define VFD_300_XOR_DISPLAY 0x11
#define VFD_300_AND_DISPLAY 0x12

#define VFD_300_SET_LUMINANCE 0x18 // full brightness

void sendCmd(unsigned char cmd)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to C
  digitalWrite(PIN_CD, HIGH);

  // set command
  PORTB &= (~MASK_PORTB);
  PORTB |= (MASK_PORTB & cmd);
  PORTD &= (~MASK_PORTD);
  PORTD |= (MASK_PORTD & cmd);

  asm("nop");

  
  // Pulse /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");
  digitalWrite(PIN_WR, HIGH);
}


void sendData(unsigned char dat)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to D
  digitalWrite(PIN_CD, LOW);

  // set data
  PORTB &= (~MASK_PORTB);
  PORTB |= (MASK_PORTB & dat);
  PORTD &= (~MASK_PORTD);
  PORTD |= (MASK_PORTD & dat);

  asm("nop");
  
  // Pulse /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");
  digitalWrite(PIN_WR, HIGH);
  
}


void initDisplay800()
{
  sendCmd(VFD_800_DSP_CLEAR);
  delay(2);
  // initialize display areas
  for (unsigned char n=0; n < 8; n++)
  {
    sendCmd(VFD_800_AREA_SET);
    sendCmd(n); // index of area
    sendData(0xFF);
  }

  sendCmd(VFD_800_DSP_ONOFF | VFD_800_DSP_ONOFF_L0 | VFD_800_DSP_ONOFF_L1);  // both layers on
  
  sendCmd(VFD_800_BRIGHTNESS); // full brightness

  for (unsigned char y=0; y < 8; y++) // for each row (0..7)
  {
    sendCmd(VFD_800_SET_X);
    sendCmd(0x00); // xpos = 0
    sendCmd(VFD_800_SET_Y);
    sendCmd(y);    // ypos = y
    sendCmd(VFD_800_ADRMODE | VFD_800_ADRMODE_INCX); // auto inc x
    
    for(unsigned char x=0; x < 128; x++) // for each x position (0..127)
    {
      sendData(1<<random(8));
    }
  }

}

void initDisplay300()
{
  Serial.println("300 mode");

  // screen0 starts at 0x0000
  sendCmd(VFD_300_SET_LOWER_ADDR_1);
  sendData(0x00);
  sendCmd(VFD_300_SET_UPPER_ADDR_1);
  sendData(0x00);
  // screen1 starts at 0x0800
  sendCmd(VFD_300_SET_LOWER_ADDR_2);
  sendData(0x00);
  sendCmd(VFD_300_SET_UPPER_ADDR_2);
  sendData(0x08);

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
  for(int i=0; i < 0x800; i++) {
    sendData(1<<random(8));
  }
  
  sendCmd(VFD_300_CURSOR_HOLD);

  delay(1);

  sendCmd(VFD_300_SET_CURSOR_LOW);
  sendData(0x00);
  sendCmd(VFD_300_SET_CURSOR_HIGH);
  sendData(0x08);

  sendCmd(VFD_300_CURSOR_INCR);
  sendCmd(VFD_300_DATA_WRITE);
  
  // clear screen1
  for(int i=0; i<0x800; i++) {
    sendData(0);
  }

  sendCmd(VFD_300_CURSOR_HOLD);
  
  sendCmd(VFD_300_SCREEN2_CHARACTER); // screen 2 = text
  sendCmd(VFD_300_OR_DISPLAY); // OR screens

  sendCmd(0x03); // both screens on
}


void processCommand(String command)
{
  if (command.length() >= 1 && command.length() <= 3)
  {  
    char action = command.charAt(0);

    switch (action)
    {
      case '3':
        // display reinitialization
        initDisplay300();
        break;
      case '8':
        // display reinitialization
        initDisplay800();
        break;
      case 'r':
      case 'R':
        // display reinitialization
        initDisplay800();
        break;
      case 'x':
      case 'X':
        // clear display
        if ('0' == command.charAt(1))
        {
          clearScreen300(0);
        } else {
          clearScreen300(1);
        }
        break;
      case 'i':
      case 'I':
        // return identification
        Serial.println(idStr);
        break;
      case 'v':
      case 'V':
        // return version
        Serial.println(verStr);
        break;
      case 'c':
      case 'C':
        // it's a command
        // check received data for plausibility
        if (
             (command.length() >= 3)
             && 
             ( ((command.charAt(1) >='0') && (command.charAt(1) <='9')) || ((command.charAt(1) >='A') && (command.charAt(1) <='F')) || ((command.charAt(1) >='a') && (command.charAt(1) <='f')) )
             &&
             ( ((command.charAt(2) >='0') && (command.charAt(2) <='9')) || ((command.charAt(2) >='A') && (command.charAt(2) <='F')) || ((command.charAt(2) >='a') && (command.charAt(2) <='f')) )
           )
         {
          unsigned char cmd = (unsigned char)strtoul(command.substring(1,3).c_str(), NULL, 16);
          sendCmd(cmd);
        } else {
          Serial.print(errorStr);
          Serial.print(" invalid input (");
          Serial.print(command);
          Serial.println(")");
        }
        break;
      case 'd':
      case 'D':
        // it's data
        // check received data for plausibility
        if (
             (command.length() >= 3)
             &&
             ( ((command.charAt(1) >='0') && (command.charAt(1) <='9')) || ((command.charAt(1) >='A') && (command.charAt(1) <='F')) || ((command.charAt(1) >='a') && (command.charAt(1) <='f')) )
             &&
             ( ((command.charAt(2) >='0') && (command.charAt(2) <='9')) || ((command.charAt(2) >='A') && (command.charAt(2) <='F')) || ((command.charAt(2) >='a') && (command.charAt(2) <='f')) )
           )
        {
          unsigned char dat = strtoul(command.substring(1,3).c_str(), NULL, 16);
          sendData(dat);
        } else {
          Serial.print(errorStr);
          Serial.print(" invalid input (");
          Serial.print(command);
          Serial.println(")");
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

// Cleaning the screen on the 300 series VFD requires a lot of instructions to be sent.
// To avoid, sending all those via the serial interface, this function exists to do it from the Arduino.
void clearScreen300(unsigned char screen)
{
  if (0 == screen)
  {
    // start at 0x0000
    sendCmd(VFD_300_SET_CURSOR_LOW);
    sendData(0x00);
    sendCmd(VFD_300_SET_CURSOR_HIGH);
    sendData(0x00);
  } else {
    // start at 0x0800
    sendCmd(VFD_300_SET_CURSOR_LOW);
    sendData(0x00);
    sendCmd(VFD_300_SET_CURSOR_HIGH);
    sendData(0x08);
  }
  
  sendCmd(VFD_300_CURSOR_INCR);
  sendCmd(VFD_300_DATA_WRITE);

  for(int i=0; i<0x800; i++) {
    sendData(0);
  }
  
  sendCmd(VFD_300_CURSOR_HOLD);  

  Serial.print("Clearscreen ");
  Serial.println(screen);  
}


void setup()
{
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  
  
  DDRB |= B00011111; // set PB0..PB4 which are D0..D4 to output
  DDRD |= B11100000; // set PD5..PD7 which are D5..D7 to output
  DDRD |= B00011100; // set PD2..PD4 which are /RD, C/D and /WR to output

  PORTB |= B00011111; // set D0..D4 to HIGH
  PORTD |= B11100000; // set D5..D7 to HIGH
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  

  pinMode(LED_BUILTIN, OUTPUT);

  /*
  DDRB |= B00100000; // set PB5 (built-in LED) to output
  digitalWrite(13, LOW); // built-in LED off
  */
  
  // put your setup code here, to run once:
  Serial.begin(115200);
  //Serial.begin(230400);
  Serial.print(idStr);
  Serial.print(' ');
  Serial.println(verStr);

  randomSeed(analogRead(0)+analogRead(1));

}


void loop()
{
  // put your main code here, to run repeatedly:
  
  if(Serial.available() > 0)
  {
    if(Serial.available() > 16)
    {
      digitalWrite(LED_BUILTIN, HIGH); // LED is on when the Arduino needs some time to process the serial data
      Serial.write(XOFF);
    }
    
    String input = Serial.readStringUntil('\n');
    processCommand(input);
    
    digitalWrite(LED_BUILTIN, LOW);
    Serial.write(XON);
    
  }
}
