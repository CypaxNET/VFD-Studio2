# Noritake Itron VFDs

First of all, for those who wonder:
A [*VFD*](https://en.wikipedia.org/wiki/Vacuum_fluorescent_display) is a kind of display you've probably seen before on HiFi devices or supermarket checkouts. Here's an example from a cassette player:

![Closeup photo from the front of a hifi cassette deck with its glowing display.](./vfd_example2.jpg)

Most of these displays can only show certain symbols or digits. However, there are also graphics-capable VFDs. Much less common and much more expensive, but they make very fascinating gadgets for your PC to show pictures, animations or various system information using VFD-Studio.

## What you'll need

**Materials:**

* A Noritake Itron 800 or 300 series VFD
  * Unfortunately those VFDs are as pricey as rare. The best approach is to look on eBay for cheap used ones.
  * VFD-Studio 2 is tested with a GU128x64-800B display (800 or 800A should do too) and a GU256x64-372 display, which has the same datasheet as the GU256X64F-9900.
* An Arduino Nano board, incl. USB cable
* A 2x13 pin female connector, 2.54mm pitch
* Two 1x15 male connectors, 2.54mm pitch
* Two 1x15 female connectors, 2.54mm pitch \[optional, if you want the Arduino Nano to be removable\]
* A prototype PCB
* Some wire and solder

**Tools:**

* A soldering iron with a fine tip.

* The Arduino IDE
  
  * Download from [arduino.cc](https://www.arduino.cc/en/software)
  
  * I have to admit that personally I still mess around with version 1.8 of the Arduino IDE. But there should be no reason why it wouldn't work with version 2.x too.

* A Windows computer, with USB. Apparently.

**Skills:**

* Reading/understanding schematics
* Some soldering work (no SMD parts though)
* Actually no programming skills. But you'll have to compile and upload a program to the Arduino Nano. So it would be quite beneficial if this was not your very first project with an Arduino.

## Instructions

### Adapter board