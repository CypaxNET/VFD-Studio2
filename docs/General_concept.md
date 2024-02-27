# General concept

VFD-Studio 2 is a Windows application intended to show various text, graphics and system information on a monochrome graphics display.

Since these displays usually do not have any interface suitable to be connected directly to a computer, an Arduino microcontroller will serve as the driver and connect to the computer via USB serial interface.

The VFD-Studio 2 application reads and processes text based files (*List files*) which contain *commands* - e.g. print some text, show an image, etc.

It is neither possible nor desirable to show all information on a small display at the same time. That's why list files are structured by *Screens* which are shown in sequential order. When the last screen was shown, the list begins anew.
