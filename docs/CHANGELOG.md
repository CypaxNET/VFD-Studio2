# Change log

## 2.0.1.0

- Features:
  
  - Can now retrieve information from [Open Hardware Monitor](https://openhardwaremonitor.org/) such as CPU or GPU temperatures (e.g. `PLAINTEXT '$OHM|/nvidiagpu/0|Temperature|GPU Core$ %  ' 13 03`).

- Bugfixes:
  
  - Some ESP8266-based Arduino controller and similar devices would enter programming mode when the serial port was opened. A new Serial Port class now prevents this.
  
  - Some various minor improvements and bugfixes.

## 2.0.0.0

Initial version of VFD-Studio 2.
