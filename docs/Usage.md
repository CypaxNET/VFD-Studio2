# Usage

## General concept

VFD-Studio 2 is a Windows application intended to show various text, graphics and system information on a monochrome graphics display.

Since these displays usually do not have any interface suitable to be connected directly to a computer, an <mark>Arduino microcontroller</mark> will serve as the driver and connect to the computer via USB serial interface.

The VFD-Studio 2 application reads and processes text based files (*List files*) which contain *commands* - e.g. print some text, show an image, etc.

It is neither possible nor desirable to show all information on a small display at the same time. That's why list files are structured by *Screens* which are shown in sequential order. When the last screen was shown, the list begins anew.

## Main window

![Screenshot of the main window showing the controls.](main_window.png)

## Preview Display

A prominent part of the main window is filled out with a *Preview Display*, which imitates what is currently being shown on the physical display.

The preview display adapts in size and color to the [display settings](./Setup.md#Display settings) from the settings window.

## List control buttons

In the middle of the application you'll find buttons

* to to load a list file, 
* reload the current list again, 
* stop at the current Screen,
* skip to the next Screen.

Clicking the stop button toggles between staying at the current Screen and continuing to the next Screen once its remaining display time has elapsed.
Right below the Preview Display the remaining time of the currently displayed Screen is shown. 

## Taskbar icon

VFD-Studio 2 will be shown as an icon in the Windows task bar. Hovering the mouse cursor above the item will display the selected display type and interface: 
![Screenshot of the Windows task bar showing the VFD-Studio icon with a hint "U8G2@COM9", which informas about selectzd display and interface.](screenshot_icon.png)
By clicking on the icon, a menu will open from where the *main window* of VFD-Studio 2 can be opened.

The other menu options are used to control the list.
