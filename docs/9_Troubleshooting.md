# Troubleshooting

Can't get the display running or having issues with the VFD-Studio 2 application? Let's see what might help...

## Display

### The display doesn't show anything

* Push the reset button on the Arduino compatible board. If the display shows a random pixel pattern, the problem resides within the connection to the PC or within VFD-Studio 2 but not within the display/Arduino combination.

* Make sure the display is powered. Measure the supply voltages using a voltage meter. Some 3.3V displays draw too much current to be powered from the Arduino; in that case it will require a dedicated power source.

* Have you checked the wiring?

* Make sure you have selected the correct constructor matching the respective display controller, resolution, interface and variant. Maybe try an alternative constructor variant.

* Make sure the correct display type and interface is selected in VFD-Studio 2.

* Make sure that the Arduino microcontroller is working correctly:
  
  1. Unplug the adapter board from the display.
  2. Open the Arduino IDE.
  3. Compile and upload the *Blink* example. If this works, the Arduino is not the problem.
  4. Upload the Arduino program again and observe if there is any error message.

* Check if we can communicate with the firmware on the Arduino:
  
  1. Close VFD-Studio 2
  2. Open the Arduino IDE
  3. Select Menu > Tools > Serial Monitor
  4. Configure baud rate 115200
  5. Send "i" (without quotation marks)
  6. The Arduino should respond with something like "Arduino driver for U8g2 compatible displays v1.0.0.0" or "Arduino driver for Noritake GU 300/800 VFD series v1.0.0.0"

If none of that helps, ask the following questions:

* Did this display work before? What has changed since?
* Did this Arduino board work before? Did it work while connected to the display too? What did change since?
* Are we 100% sure the wiring is correct?

### The Arduino board doesn't connect with the computer

* Open the Windows device manager and open the *Ports (COM & LPT)* section. Unplug and plug in the USB cable. Observe if a new interface shows up.
  
  If the interface shows up with an error indicator, you might have to change the driver.

### The Arduino IDE doesn't upload the firmware

* Make sure you have selected the correct port:
  
  Select Menu > Tools > Port > the port your Arduino compatible board connects to.

## VFD-Studio 2 application

### VFD-Studio 2 doesn't react anymore / crashes

* VFD-Studio 2 doesn't like it at all when the USB cable to the display is unplugged. Avoid this.

* Make sure all paths to bitmaps / animations in the List File are correct.

* Check the log file for further information.

### VFD-Studio 2 runs but doesn't update the display

* Make sure the [correct serial port is selected](./2_Setup.md#interface) in VFD-Studio 2.

* Depending on the settings, VFD-Studio 2 might suspend animations while the CPU load is high.
