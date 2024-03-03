# Multiple displays

## How it works

You can control as many displays connected to the computer with VFD-Studio 2 as you like, where each display is controlled by a dedicate instance of VFDStudio2.exe.

> [!TIP]
> The recommended method of handling multiple displays is to have multiple installations of VFD-Studio 2 in separate directories.

Now, simply starting VFDStudio2.exe two times would not work because VFD-Studio 2 uses a settings file to store the type of display and its interface settings.
The most obvious method to have two instances running in parallel would be to install VFD-Studio 2 in two separate directories.
If this is not possible for you for some reason, there is an alternative option:

### Multiple settings files

1. Go to the VFD-Studio 2 installation directory.

2. Make a copy of vfdstudio.ini and name it "theotherdisplay.ini" or whatever you like.

3. Make a link to VFDStudio2.exe and name it "The other display" or whatever you like.

4. Right-click the link in the Windows Explorer and select *Properties* from the context menu.

5. In the target field, add ` theotherdisplay.ini` (note that there is a leading space) to the path:
   
   ![screenshot_linkparam.png](./images/screenshot_linkparam.png)

6. Close any running VFD-Studio 2 application.

7. Start VFD-Studio 2 via the new link.

8. Open the [settings window](./Setup.md) and configure your other display.

9. Close VFD-Studio 2.

You now have settings files (\*.ini) for two displays in the installation directory. You can start VFDStudio2.exe to control you first display, which will use the default *vfdstudio.ini* file, and another instance of VFD-Studio 2 via the link to control your other display, which will use the *theotherdisplay.ini* settings file.

## Synchronization

When using multiple displays, you can synchronize them with one instance of VFDStudio2.exe being the *synchronization sender* and another instance of VFDStudio2.exe being a *synchronization receiver*.
The synchronization sender sends a message to any synchronization receivers whenever it switches to another Screen or reaching the start of the List.
Synchronization receivers will ignore any SCREENTIME commands and instead will wait for a message from the synchronization sender before switching to the next Screen.

Since synchronization this is an advanced feature and still experimental, there is no configuration item in the settings window. So you need to edit the settings files (\*.ini) of the VFDStudio2.exe instances:

1. Close any running VFD-Studio 2 application.

2. Open the settings file (\*.ini) of the VFD-Studio 2 application which shall be the *synchronization sender* in Windows Notepad or any other text editor.

3. In the `[APPLICATION]` section change the line `Sync=0` to `Sync=1`.

4. Open the settings file (*.ini) of the VFD-Studio 2 application which shall be the *synchronization receiver* in Windows Notepad or any other text editor.

5. In the `[APPLICATION]` section change the line `Sync=0` to `Sync=2`.

6. Start the VFD-Studio 2 applications.

The displays will be synchronized at the latest when the synchronization sender starts scrolling back to the start of its List. To enforce this, you can click on the Reload button there.
