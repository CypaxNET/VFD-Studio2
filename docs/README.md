# Installation

[Installation via installer](./1_Installation.md)

[Portable version](./1_Portable_version.md)

# Setup

[Application setup](./2_Setup.md)

# Displays

[Noritake Itron VFDs](./3_VF-Displays.md)

[Other displays](./3_LCD_and_OLED_displays.md)

# Usage

[Application usage](./4_Application_usage.md)

[List Editor](./4_List_Editor_usage.md)

[Special: Multiple displays](./Multiple_displays.md)

[Troubleshooting](./9_Troubleshooting.md)

# FAQ

Q: Which displays can I control with VFD-Studio?

A: It works with

- any display supported by the [U8glib library](https://github.com/olikraus/u8g2) - no matter if LCD, OLED, etc. See [here](https://github.com/olikraus/u8g2/wiki/u8g2setupcpp) for a complete list of supported display controllers.
- [Noritake Itron 800 VFD series](https://www.noritake-elec.com/products/vfd-display-module/dot-matrix-graphic-display/gu-800-series) (e.g. GU128x64-800B)
- Noritake Itron 300 VFD series (e.g. GU256x64-372 aka GU256X64F-9900)

Q: Why doesn't VFD-Studio 2 support the [Noritake Itron 7000 series](https://www.noritake-elec.com/products/vfd-display-module/dot-matrix-graphic-display/gu-7000-series)? They even come with a serial interface!

A: Those are nice. But I have none. Buy me one and I'll make an update of VFD-Studio 2 :smile:

Q: Why VFD-Studio **2** - where is version 1?

A: [Here](https://cypax.net/projects/vfdstudio/) it is. VFD-Studio 1 was a program I wrote back in 2004 as a student project. It could only control one single type of display and used direct write access to hardware registers of the printer interface. These printer interfaces no longer exist on today's computers. But I had still the display. That's why I decided to revive the whole thing in a version 2 and support some more displays.
