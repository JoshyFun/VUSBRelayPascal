# VUSBRelayPascal
Pascal (fpc) library to manage V-USB relays

 --------------------------------------
  USBRelayLIB
 --------------------------------------

 This library simplifies the handling of V-USB [1] relays identified
 by the system as USB HID devices. This relays are cheap and easy to
 find in many online shops. They can be used, in example to power off
 your router when internet connectivity is drop, and repower it on
 after some time.

 It supports as USB API interface:
  -Native Windows (Shoud work from XP to 10, 32 and 64 bits)
  -libusb-1.0 (Linux 32 and 64 bits, and Windows 32 bits)
  -libusb-0.1 (Linux 32 and 64 bits)

 Library defaults to:
  -Unix: libusb-1.0
  -Windows: Windows

  The use of libusb-1.0 can be forced by define "USBHID_LIBUSB10".
  "USBHID_QUIET" supresses any console output.

 libusb-1.0 and Windows supports dynamic library loading using the define "USBHID_DYNAMIC".

 License:
  LGLP [2] - The same as Free Pascal compiler and Lazarus LCL.

 Inspired by:
  The project at https://github.com/pavel-a/usb-relay-hid

 Copyright:
  José Mejuto (joshyfun at gmail.com)

 References:
  [1] https://www.obdev.at/products/vusb/index.html
  [2] https://www.gnu.org/licenses/lgpl-3.0.txt

