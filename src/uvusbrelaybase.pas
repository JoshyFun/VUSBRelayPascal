unit uvusbrelaybase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{.$DEFINE USBHID_QUIET}

const
  USB_CFG_VENDOR_ID =      $16c0;
  USB_CFG_DEVICE_ID =      $05DF;
  USB_RELAY_VENDOR_NAME =  'www.dcttech.com';
  USB_RELAY_NAME_PREF =    'USBRelay';  // USBRelay1, USBRelay2...8
  USB_RELAY_ID_STR_LEN =   5;

  USBHID_OK =             0;   // no error
  USBHID_ERR_ACCESS =     1;   // access or permissions error
  USBHID_ERR_IO =         2;   // I/O error
  USBHID_ERR_NOTFOUND =   3;   // device not found
  USBHID_ERR_BAD_ARG =    20;  // invalid parameter
  USBHID_ERR_INTERNAL =   23;  // not implemented, not supported...
  USBHID_ERR_IO_USB =     24;  // I/O error at usb layer
  USBHID_ERR_IO_HID =     25;  // I/O error at hid layer
  USBHID_ERR_UNKNOWN =    -1;  // whatever

type

  TUSBDEVHANDLE = record
    TheDev: Pointer;
    TheDescriptorPointer: Pointer;
    case integer of
      0: (TheHandle: SizeUint);
      1: (TheHandlePointer: Pointer);
  end;
  USBDEVHANDLE=^TUSBDEVHANDLE;
  THIDGUID = TGUID;

  USBHIDEnumFunc=function(const aUSBDevHandle: USBDEVHANDLE; const Context: Pointer): integer of object;

  TUSBDeviceIDCallback=procedure (const aVendorID: WORD; const aProductID: WORD; out aVendorName: string; out aProductName: string) of object;

resourcestring
  USBHID_MSG_DEVICE_FOUND_MASK='Device Found: vid=%d pid=%d ver=%4.4X';
  USBHID_MSG_CALLING_ENUMERATE='Calling enumerate callback.';
  USBHID_MSG_WIN_HIDPATH='HID PATH found %s';
  USBHID_MSG_SET_REPORT='SET report message: %s';
  USBHID_MSG_GET_REPORT='GET report message: %s';
  USBHID_ERROR_OPEN_FAIL='Failed to open handle for device. Check your access privileges.';
  USBHID_ERROR_ACCESS_DENIED='Access denied to USB device. Check your access privileges.';
  USBHID_ERROR_READING_VENDOR='Error reading vendor string.';
  USBHID_ERROR_READING_PRODUCT='Error reading product string.';
  USBHID_ERROR_READING_DESCRIPTOR='Error reading descriptor.';
  USBHID_ERROR_GET_REPORT='GET Report failed.';
  USBHID_ERROR_SET_REPORT='SET Report failed.';
  USBHID_ERROR_GETPROCADDRESS='Entry point for one or more functions failed (GetProcAddress).';

var
  // This variable could be set at runtime to retrieve
  // information about discovered devices by the USB enumerators.
  gUSBDeviceIDInfo: TUSBDeviceIDCallback;
  gUSBHID_Quiet: Boolean=false;


  procedure DEBUG_OUTPUT(const aMessage: String);
  procedure DEBUG_USBIDS(const aVendorID, aProductID, aVersion: WORD);

implementation

procedure DEBUG_OUTPUT(const aMessage: String); inline;
begin
  {$IFNDEF USBHID_QUIET}
  if IsConsole and not gUSBHID_Quiet then begin
    writeln(aMessage);
  end;
  {$ENDIF}
end;

procedure DEBUG_USBIDS(const aVendorID, aProductID, aVersion: WORD);
var
  lVendorName: string;
  lProductName: string;
begin
  if Assigned(gUSBDeviceIDInfo) then begin
    gUSBDeviceIDInfo(aVendorID,aProductID,lVendorName,lProductName);
    DEBUG_OUTPUT(format('%4.4X-%4.4X %s %2.2X.%2.2X (%s)',[aVendorID,aProductID,lProductName,aVersion shr 8, aVersion and $FF,lVendorName]));
  end else begin
    DEBUG_OUTPUT(format(USBHID_MSG_DEVICE_FOUND_MASK,[aVendorID,aProductID,aVersion]));
  end;

end;


end.

