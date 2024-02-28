# Noritake Itron VFDs

First of all, for those who wonder:
A [*VFD*](https://en.wikipedia.org/wiki/Vacuum_fluorescent_display) is a kind of display you've probably seen before on HiFi devices or supermarket checkouts. Here's an example from a cassette player:

![Closeup photo from the front of a hifi cassette deck with its glowing display.](./vfd_example2.jpg)

Most of these displays can only show certain symbols or digits. However, there are also graphics-capable VFDs. Much less common and much more expensive, but they make very fascinating gadgets for your PC to show pictures, animations or various system information using VFD-Studio 2.

## What you'll need

**Materials:**

* A Noritake Itron 800 or 300 series VFD
  * Unfortunately those VFDs are as pricey as rare. The best approach is to look on eBay for cheap used ones.
  * VFD-Studio 2 is tested with a GU128x64-800B display (800A should do too) and a GU256x64-372 display, which has the same datasheet as the GU256X64F-9900.
* An [Arduino Nano board](https://docs.arduino.cc/hardware/nano/), incl. USB cable
* A 5V power supply for the display
  * A VFD requires a lot of power. A 256X64 display may take more than 2A, which is far more than a typical USB port could deliver.
* A 2x13 pin female connector, 2.54mm pitch, 90° angulated
* Two 1x15 male connectors, 2.54mm pitch
* Two 1x15 female connectors, 2.54mm pitch \[optional, if you want the Arduino Nano to be removable\]
* A prototype PCB
* Some wire and solder

<img src="./images/photo-what-youll-need-noritake.jpg" title="Needed materials" alt="Photo showing the parts you will need." width="662">

> [!CAUTION]
> Be super careful when handling the VFD! Do not damage the fragile glass evacuation tube on the side or you risk expensive hardware ending up as useless trash just because of a small mishap.
> <img src="./images/photo-glass-tube.jpg" title="Protect this at all costs!" alt="Photo showing the fragile glass evacuation tube of a VFD." width="400">



**Tools:**

* A soldering iron with a fine tip

* The Arduino IDE
  
  * Download from [arduino.cc](https://www.arduino.cc/en/software)
  
  * I have to admit that personally I still mess around with version 1.8 of the Arduino IDE. But there should be no reason why it wouldn't work with newer versions too.



**Skills:**

* Reading/understanding schematics
* Some soldering work (no SMD parts though)
* Actually no programming skills. But you'll have to compile and upload a program to the Arduino Nano. So it would be quite beneficial if this was not your very first project with an Arduino.



## Instructions

### Adapter board

The VF-display cannot just be plugged into a computer so we need to build an adapter board with the Arduino.

#### Soldering the connectors

1. Start by soldering the 1x15 male connectors to the bottom side of the Arduino Nano. The PCB might be a good help to keep the connectors aligned. Do not solder to the PCB though. 
   
   <img src="./images/photo-soldering-arduino-pins.jpg" title="Soldering the Arduino pins" alt="Photo showing the pins of the Arduino being soldered." width="662">

2. Solder the 90° 2x13 female connector to the PCB.

3. Solder the two 1x15 female connectors to the PCB. The Arduino might be a good help to keep the connectors aligned:
   
   <img src="./images/photo-1x15-connectors.jpg" title="Use the Arduino to fix the female connectors prior soldering" alt="Photo showing how the Arduino is used to keep the female connectors aligned." width="662">

4. This is how it should look now:
   
   <img src="./images/photo-soldered-connectors.jpg" title="PCB with soldered connectors" alt="Photo showing the connectors soldered to the adapter board with the Arduino unplugged." width="662">

#### Wiring

Now to the "fun" part.

Turn the adapter board bottom side up. What we want to achieve is something like this:
<img src="./images/photo-adapter-board-bottomside.jpg" title="Bottomside of the adapter board with the pins labeled" alt="Photo showing the adapter board bottom side with labeled pins." width="300">

1. Start by connecting the display pins 2..24 to ground of the Arduino.

2. Connect the data pins of the display 1..15 with the Arduino.

3. Connect the control pins 17 (*/WR*), 19 (*C/D*) and 21 (*/RD*) with the Arduino.

4. Connect the display pin 23 (*/CS*) with ground (a drop of solder connects it to the opposite pin 24).

Follow the instructions in the image. **The image shows the adapter board from the bottom side.** Click the image to enlarge it:
<img src="./images/wiring-notitake-nano-adapter.png" title="Step by step instruction for wiring" alt="Photo showing the adapter board bottom side with labeled pins." width="800">