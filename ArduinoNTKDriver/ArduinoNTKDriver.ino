#define SER_BUFFERSIZE 1024
#define MAX_CMD_LEN 4

const char idStr[] = "Arduino Display Driver";
const char verStr[] = "v0.1.0.0";
const char helpStr[] = "V = Get SW version\nCxy = send command with xy = hex code\nDxy = send data with xy = hex code\n? = this help text";
const char errorStr[] = "ERR";


const unsigned char BIT_CD = 0x08;
const unsigned char BIT_RD = 0x04;
const unsigned char BIT_WR = 0x10; 


const unsigned char MASK_PORTB = B00011111;
const unsigned char MASK_PORTD = B11100000; 

const byte XON = 0x11;  // ASCII-Wert für XON
const byte XOFF = 0x13; // ASCII-Wert für XOFF

char aBuffer[SER_BUFFERSIZE];
int iBufferIdx;

#define PIN_CD  3
#define PIN_RD  2
#define PIN_WR  4



#define VFD_CMD_SET_LOWER_ADDR_1 0x0A
#define VFD_CMD_SET_UPPER_ADDR_1 0x0B
#define VFD_CMD_SET_LOWER_ADDR_2 0x0C
#define VFD_CMD_SET_UPPER_ADDR_2 0x0D

#define VFD_CMD_CURSOR_INCR 0x04
#define VFD_CMD_CURSOR_HOLD 0x05

#define VFD_CMD_SCREEN2_CHARACTER 0x06
#define VFD_CMD_SCREEN2_GRAPHICS  0x07

#define VFD_CMD_DATA_WRITE 0x08
#define VFD_CMD_DATA_READ  0x09

#define VFD_CMD_SET_CURSOR_LOW  0x0E
#define VFD_CMD_SET_CURSOR_HIGH 0x0F

#define VFD_CMD_OR_DISPLAY  0x10
#define VFD_CMD_XOR_DISPLAY 0x11
#define VFD_CMD_AND_DISPLAY 0x12

#define VFD_CMD_SET_LUMINANCE 0x18

#if 0
void sendCmd(unsigned char cmd)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to C
  digitalWrite(PIN_CD, HIGH);
  // make sure /WR is set
  digitalWrite(PIN_WR, HIGH);
  
  asm("nop");

  // set command
  PORTB &= (~MASK_PORTB);
  PORTB |= (MASK_PORTB & cmd);
  PORTD &= (~MASK_PORTD);
  PORTD |= (MASK_PORTD & cmd);
  
  // set /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");

  // reset /WR
  digitalWrite(PIN_WR, HIGH);
  asm("nop");

  // set direction to input
  digitalWrite(PIN_RD, LOW);  


  if((cmd & 0xF2) == 0x52)
  { // appears to be a clear display command
    delay(2);  // give the display some time to process that
  }

  //delay(2);

  /*
  Serial.print('c');
  if (cmd <= 0x0F) Serial.print("0");
  Serial.println(cmd, HEX);
  */

}


void sendData(unsigned char dat)
{
  // set direction to output
  digitalWrite(PIN_RD, HIGH);
  // set C/D to D
  digitalWrite(PIN_CD, LOW);
  // make sure /WR is set
  digitalWrite(PIN_WR, HIGH);
  asm("nop");

  // set command
  PORTB &= (~MASK_PORTB);
  PORTB |= (MASK_PORTB & dat);
  PORTD &= (~MASK_PORTD);
  PORTD |= (MASK_PORTD & dat);

  // set /WR
  digitalWrite(PIN_WR, LOW);
  asm("nop");

  // reset /WR
  digitalWrite(PIN_WR, HIGH);
  asm("nop");

  // set direction to input
  digitalWrite(PIN_RD, LOW);  

  //delay(2);
  /*
  Serial.print('d');
  if (dat <= 0x0F) Serial.print("0");
  Serial.println(dat, HEX);
  */
}
#endif

void sendCmd2(unsigned char cmd)
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



#if 0
  // set data bits to 0xFD
  PORTB |= (MASK_PORTB & 0xFD);
  PORTD |= (MASK_PORTD & 0xFD); 

  // set data bits to 0x00
  PORTB &= (~MASK_PORTB);
  PORTD &= (~MASK_PORTD);
#endif
}


void sendData2(unsigned char dat)
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



#if 0
  // set data bits to 0xFD
  PORTB |= (MASK_PORTB & 0xFD);
  PORTD |= (MASK_PORTD & 0xFD); 

  // set data bits to 0x00
  PORTB &= (~MASK_PORTB);
  PORTD &= (~MASK_PORTD);
#endif
  
}


void initDisplay800()
{

  sendCmd2(0x5F);
  delay(2);
  for (unsigned char n=0; n < 8; n++)
  {
    sendCmd2(0x62);
    sendCmd2(n);
    sendData2(0xFF);
  }

  sendCmd2(0x2C);  // both layers on
  sendCmd2(0x40);  // gfx mode

  sendCmd2(0x40); // full brightness



  for (unsigned char y=0; y<8; y++)
  {
    sendCmd2(0x64);
    sendCmd2(0x00); // xpos 0
    sendCmd2(0x60);
    sendCmd2(y);    // ypos 0
    sendCmd2(0x84); // auto inc x
    for(unsigned char x=0; x<128; x++)
    {
      sendData2(1<<random(8));
    }
  }

}



void initDisplay300()
{
  Serial.println("300 mode");

  sendCmd2(0x0A);
  sendData2(0x00);
  sendCmd2(0x0B);
  sendData2(0x00);
  sendCmd2(0x0C);
  sendData2(0x00);
  sendCmd2(0x0D);
  sendData2(0x08);

  sendCmd2(0x05);

  sendCmd2(0x18);

  sendCmd2(0x00);

  sendCmd2(0x0E);
  sendData2(0x00);
  sendCmd2(0x0F);
  sendData2(0x00);
  
  sendCmd2(0x04);

  sendCmd2(0x08);


  for(int i=0; i<0x800; i++) {
    //sendData2(0);
    sendData2(1<<random(8));
  }

  /*
  for(int i=0; i<0x400; i++) {
    sendData2(0);
    //sendData2(1<<random(8));
  }
  */

  
  sendCmd2(0x05);

  delay(1);

  sendCmd2(0x0E);
  sendData2(0x00);
  sendCmd2(0x0F);
  sendData2(0x08);

  sendCmd2(0x04);
  sendCmd2(0x08);
  
  for(int i=0; i<0x800; i++) {
    //sendData2(random(0x30, 0x3A));
    //sendData2(0x35);
    sendData2(0);
    //sendData2(random(1)<<random(8));
    //sendData2(0xFF);
    //asm("nop");
  }

  sendCmd2(0x05);
  
  sendCmd2(0x06); // screen 2 = text
  //sendCmd2(0x07); // screen 2 = graphics
  sendCmd2(0x10); // OR screens

  sendCmd2(0x03); // which screens on? 1..3

}



void setup()
{
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  
  
  DDRB |= B00011111; // set PB0..PB4 which are D0..D4 to output
  DDRD |= B11100000; // set PD5..PD7 which are D5..D7 to output
  DDRD |= B00011100; // set PD2..PD4 which are /RD, C/D and /WR to output

  // alt PORTD |= BIT_WR;
  // neu:
  PORTB |= B00011111; // set D0..D4 to HIGH
  PORTD |= B11100000; // set D5..D7 to HIGH
  PORTD |= B00011100; // set /RD, C/D and /WR to HIGH  

  DDRB |= B00100000; // set PB5 (built-in LED) to output
  digitalWrite(13, LOW); // built-in LED off
  
  for (iBufferIdx = 0; iBufferIdx < SER_BUFFERSIZE; iBufferIdx++)
  {
    aBuffer[iBufferIdx] = 0;
  }
  iBufferIdx = 0;

 
  // put your setup code here, to run once:
  Serial.begin(115200);
  //Serial.begin(230400);
  Serial.println(verStr);

  randomSeed(analogRead(0)+analogRead(1));

  //initDisplay300();
}

void clearScreen300(unsigned char layer)
{
  if (0 == layer)
  {
    sendCmd2(0x0E);
    sendData2(0x00);
    sendCmd2(0x0F);
    sendData2(0x00);
  } else {
    sendCmd2(0x0E);
    sendData2(0x00);
    sendCmd2(0x0F);
    sendData2(0x08);
  }
  
  sendCmd2(0x04);
  sendCmd2(0x08);

  for(int i=0; i<0x800; i++) {
    sendData2(0);
  }
  
  sendCmd2(0x05);  

  Serial.print("Clearscreen ");
  Serial.println(layer);  
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
          sendCmd2(cmd);
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
          sendData2(dat);
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


void loop()
{
  // put your main code here, to run repeatedly:
  
  if(Serial.available() > 0)
  {
    if(Serial.available() > 16)
    {
      Serial.write(XOFF);
    }
    
    String input = Serial.readStringUntil('\n');
    processCommand(input);

    Serial.write(XON);
    
  }
}
