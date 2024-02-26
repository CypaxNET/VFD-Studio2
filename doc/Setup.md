# Setup

The settings window can be opened anytime via the "*Config*" button from the applications main window:

![settings_window.png](E:\Projekte\VFD-Studio2\git\doc\settings_window.png)

## Application settings

### Start minimized

You can select whether or not VFD-Studio 2 shall start minimized. If so, it will be hidden and only be visible as an icon in the system tray of Windows:

![system_tray.png](E:\Projekte\VFD-Studio2\git\doc\system_tray.png)

### Icon

The icon can be selected in the settings window, which is useful when running <mark>multiple instances of VFD-Studio 2 at a time</mark>.

### Language

Klick on any language button to change the application language of VFD-Studio.

Note: Some dialogs (e.g. when opening files) might still use the respective system language as configured in Windows.

## Display selection

Changing display settings requires a restart of VFD-Studio 2 to take effect. The application will ask you to restart automatically if any changes in this section have been made.

Make sure the display settings match the display connected to the computer and that no other applications such as the <mark>Arduino IDE</mark> are blocking the interface.

### Display type

Select the type of display connected to the computer from the provided list. 
If using an u8g2 compatible display, VFD-Studio doesn't need to know the specific type of display since the <mark>Arduino display driver</mark> will take care of the details.

### Interface

Select the interface your display is connected to from the provided list. If unsure which interface is correct, close VFD-Studio, open the Windows device manager and observe which interface disappears/appears when unplugging the cable from the computer.

### Baud rate

Usually the predefined baud rate of 115200 is correct and should not be changed. If you are using <mark>custom Arduino firmware</mark> you might adapt it to the respective baud rate here.

### Resolution

VFD-Studio 2 cannot guess the specific display resolution (in pixels) of your display, so you'll have to configure it here. In fact, if using the <mark>VF-displays</mark>, the application will send the display resolution to the <mark>Arduino display driver</mark> during initialization.

Note: The **vertical resolution must be divisible by 8** for VFD-Studio 2 to work properly!

## Options

### Display brightness

Lists may include special <mark>commands to adjust the display brightness</mark>. In the options you can decide whether these commands shall be processed or if the display shall run on a fixed value of brightness. Changing this setting will take effect on the next brightness instruction or next new <mark>Screen</mark>.

### Clear display on exit

You can decide whether to clear the display content when VFD-Studio 2 closes or leave it as it is.

### Color of preview display

The main window of VFD-Studio features a preview display. In the settings you can adjust the color of the preview display to match the color of the physical display.

Note: This settings takes effect as soon as something is updated on the preview display. If you slide the settings window away from the main window, you might see the effect immediately.

### Play animations only when the CPU is idle

Playing animations costs some CPU processing time which might be unwanted while playing games or performing other tasks that require a lot of CPU.

In this setting you can instruct VFD-Studio 2 to play animations only when the average CPU usage falls below an idle threshold.
