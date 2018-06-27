unit libusb;

(*
  Got from hansiglaser/pas-libusb
  https://github.com/hansiglaser/pas-libusb
  libusb-0.1
  Some modifications by José Mejuto (joshyfun@gmail.com)
*)

Interface

{.$DEFINE USBHID_DYNAMIC}

{$Macro ON}

{$IFDEF WINDOWS}
  {$DEFINE MACRO_CALLING_CONV:=stdcall}
{$ELSE}
  {$DEFINE MACRO_CALLING_CONV:=cdecl}
{$ENDIF}

const
  {$IFDEF WINDOWS}
  LIBUSB_LIB='libusb.dll';
  LIBUSB_LIB_DYN='libusb.dll';
  {$ELSE}
  LIBUSB_LIB='usb';
  {$IFNDEF LIBUSB_LIB_DYN}
  LIBUSB_LIB_DYN='libusb-0.1.so.4';
  {$ENDIF}
  {$ENDIF}

{
  Automatically converted by H2Pas 0.99.15 from usb.h
  The following command line parameters were used:
    -d
    -l
    usb
    -o
    libusb.pp
    -p
    -v
    usb.h
}

{ global Linux constants }

const
   PATH_MAX = 4096;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{
   USB spec information

   This is all stuff grabbed from various USB specs and is pretty much
   not subject to change
}
{
   Device and/or Interface Class codes
}
{ for DeviceClass  }

const
   USB_CLASS_PER_INTERFACE = 0;
   USB_CLASS_AUDIO = 1;
   USB_CLASS_COMM = 2;
   USB_CLASS_HID = 3;
   USB_CLASS_PRINTER = 7;
   USB_CLASS_MASS_STORAGE = 8;
   USB_CLASS_HUB = 9;
   USB_CLASS_DATA = 10;
   USB_CLASS_VENDOR_SPEC = $ff;
{
   Descriptor types
}
   USB_DT_DEVICE = $01;
   USB_DT_CONFIG = $02;
   USB_DT_STRING = $03;
   USB_DT_INTERFACE = $04;
   USB_DT_ENDPOINT = $05;
   USB_DT_HID = $21;
   USB_DT_REPORT = $22;
   USB_DT_PHYSICAL = $23;
   USB_DT_HUB = $29;
{
   Descriptor sizes per descriptor type
}
   USB_DT_DEVICE_SIZE = 18;
   USB_DT_CONFIG_SIZE = 9;
   USB_DT_INTERFACE_SIZE = 9;
   USB_DT_ENDPOINT_SIZE = 7;
{ Audio extension  }
   USB_DT_ENDPOINT_AUDIO_SIZE = 9;
   USB_DT_HUB_NONVAR_SIZE = 7;
{ All standard descriptors have these 2 fields in common  }

type
   PUSBDescriptorHeader = ^USBDescriptorHeader;
   USBDescriptorHeader = packed record
        bLength : Byte;
        bDescriptorType : Byte;
     end;

{ String descriptor  }
   PUSBStringDescriptor = ^USBStringDescriptor;
   USBStringDescriptor = packed record
        bLength : Byte;
        bDescriptorType : Byte;
        wData : array[0..0] of WideChar;
     end;

{ HID descriptor  }
   Pusb_hid_descriptor = ^usb_hid_descriptor;
   usb_hid_descriptor = packed record
        bLength : Byte;
        bDescriptorType : Byte;
        bcdHID : Word;
        bCountryCode : Byte;
        bNumDescriptors : Byte;
        bReportDescriptorType : Byte;
        wDescriptorLength : Word;
     end;

{ Endpoint descriptor  }

const
   USB_MAXENDPOINTS = 32;
{ Extra descriptors  }

type
   PUSBEndpointDescriptor = ^USBEndpointDescriptor;
   USBEndpointDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bEndpointAddress : Byte;
        bmAttributes : Byte;
        wMaxPacketSize : Word;
        bInterval : Byte;
        bRefresh : Byte;
        bSynchAddress : Byte;
        extra : Pbyte;
        extralen : Longint;
     end;
   PUSBEndpointDescriptorArray = ^USBEndpointDescriptorArray;
   USBEndpointDescriptorArray = array[0..0] of USBEndpointDescriptor;

{ in bEndpointAddress  }

const
   USB_ENDPOINT_ADDRESS_MASK = $0f;
   USB_ENDPOINT_DIR_MASK = $80;
{ in bmAttributes  }
   USB_ENDPOINT_TYPE_MASK = $03;
   USB_ENDPOINT_TYPE_CONTROL = 0;
   USB_ENDPOINT_TYPE_ISOCHRONOUS = 1;
   USB_ENDPOINT_TYPE_BULK = 2;
   USB_ENDPOINT_TYPE_INTERRUPT = 3;
{ Interface descriptor  }
   USB_MAXINTERFACES = 32;
{ Extra descriptors  }

type
   PUSBInterfaceDescriptor = ^USBInterfaceDescriptor;
   USBInterfaceDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bInterfaceNumber : Byte;
        bAlternateSetting : Byte;
        bNumEndpoints : Byte;
        bInterfaceClass : Byte;
        bInterfaceSubClass : Byte;
        bInterfaceProtocol : Byte;
        iInterface : Byte;
        endpoint : PUSBEndpointDescriptorArray;
        extra : Pbyte;
        extralen : Longint;
     end;
   PUSBInterfaceDescriptorArray = ^USBInterfaceDescriptorArray;
   USBInterfaceDescriptorArray = array[0..0] of USBInterfaceDescriptor;


{ Hard limit  }

const
   USB_MAXALTSETTING = 128;

type
   PUSB_Interface = ^USB_Interface;
   USB_Interface = record
        altsetting : PUSBInterfaceDescriptorArray;
        num_altsetting : Longint;
     end;
   PUSB_InterfaceArray = ^USB_InterfaceArray;
   USB_InterfaceArray = array[0..0] of USB_Interface;

{ Configuration descriptor information..  }

const
   USB_MAXCONFIG = 8;
{ Extra descriptors  }

type
   PUSBConfigDescriptor = ^USBConfigDescriptor;
   USBConfigDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        wTotalLength : Word;
        bNumInterfaces : Byte;
        bConfigurationValue : Byte;
        iConfiguration : Byte;
        bmAttributes : Byte;
        MaxPower : Byte;
        TheInterface : PUSB_InterfaceArray;
        extra : Pbyte;
        extralen : Longint;
     end;

{ Device descriptor  }
   PUSBDeviceDescriptor = ^USBDeviceDescriptor;
   USBDeviceDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bcdUSB : Word;
        bDeviceClass : Byte;
        bDeviceSubClass : Byte;
        bDeviceProtocol : Byte;
        bMaxPacketSize0 : Byte;
        idVendor : Word;
        idProduct : Word;
        bcdDevice : Word;
        iManufacturer : Byte;
        iProduct : Byte;
        iSerialNumber : Byte;
        bNumConfigurations : Byte;
     end;

{
   Standard requests
}

const
   USB_REQ_GET_STATUS = $00;
   USB_REQ_CLEAR_FEATURE = $01;
{ 0x02 is reserved  }
   USB_REQ_SET_FEATURE = $03;
{ 0x04 is reserved  }
   USB_REQ_SET_ADDRESS = $05;
   USB_REQ_GET_DESCRIPTOR = $06;
   USB_REQ_SET_DESCRIPTOR = $07;
   USB_REQ_GET_CONFIGURATION = $08;
   USB_REQ_SET_CONFIGURATION = $09;
   USB_REQ_GET_INTERFACE = $0A;
   USB_REQ_SET_INTERFACE = $0B;
   USB_REQ_SYNCH_FRAME = $0C;
   USB_TYPE_STANDARD = $00 shl 5;
   USB_TYPE_CLASS = $01 shl 5;
   USB_TYPE_VENDOR = $02 shl 5;
   USB_TYPE_RESERVED = $03 shl 5;
   USB_RECIP_DEVICE = $00;
   USB_RECIP_INTERFACE = $01;
   USB_RECIP_ENDPOINT = $02;
   USB_RECIP_OTHER = $03;

  USB_REQ_HID_GET_REPORT   = $01;
  USB_REQ_HID_GET_IDLE     = $02;
  USB_REQ_HID_GET_PROTOCOL = $03;
  USB_REQ_HID_SET_REPORT   = $09;
  USB_REQ_HID_SET_IDLE     = $0A;
  USB_REQ_HID_SET_PROTOCOL = $0B;

  HID_REPORT_TYPE_INPUT    = $01;
  HID_REPORT_TYPE_OUTPUT   = $02;
  HID_REPORT_TYPE_FEATURE  = $03;

  {
   Various libusb API related stuff
  }
   USB_ENDPOINT_IN = $80;
   USB_ENDPOINT_OUT = $00;
{ Error codes  }
   USB_ERROR_BEGIN = 500000;

var //errno : LongInt; cvar; external; { from libc }
    usb_error_errno : LongInt; cvar; external; { from libusb }
    usb_error_type  : LongInt; cvar; external; { from libusb }
    usb_error_str   : PChar;   cvar; external; { from libusb }

type PUSBBus = ^USBBus;

   PUSBDevice = ^USBDevice;
   USBDevice = record
        next : PUSBDevice;
        prev : PUSBDevice;
        filename : array[0..(PATH_MAX + 1)-1] of Char;
        bus : PUSBBus;
        descriptor : USBDeviceDescriptor;
        config : PUSBConfigDescriptor;
        dev : pointer;
     end;

   { PUSBBus = ^USBBus;}
   USBBus = record
        next : PUSBBus;
        prev : PUSBBus;
        dirname : array[0..(PATH_MAX + 1)-1] of Char;
        devices : PUSBDevice;
     end;
   PUSBDevHandle = ^TUSBDevHandle;
   TUSBDevHandle = record
     end;

{ Variables  }

var USBBusses : PUSBBus; cvar; external;

{ function prototypes  }

{$IFNDEF USBHID_DYNAMIC}
function  usb_open(dev:PUSBDevice):PUSBDevHandle; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_close(dev:PUSBDevHandle):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_get_string(dev:PUSBDevHandle; Index:Longint; langid:Longint; var Buf; buflen:Longint) : Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_get_string_simple(dev:PUSBDevHandle; index:Longint; var Buf; buflen:Longint) : Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_bulk_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_bulk_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_interrupt_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_interrupt_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_control_msg(dev:PUSBDevHandle; requestType:Longint; request:Longint; value:Longint; index:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_set_configuration(dev:PUSBDevHandle; configuration:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_claim_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_release_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_set_altinterface(dev:PUSBDevHandle; alternate:Longint):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_resetep(dev:PUSBDevHandle; ep:dword):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_clear_halt(dev:PUSBDevHandle; ep:dword):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_reset(dev:PUSBDevHandle):Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_strerror:PChar; MACRO_CALLING_CONV; external LIBUSB_LIB;
procedure usb_init; MACRO_CALLING_CONV; external LIBUSB_LIB;
procedure usb_set_debug(level:Longint); MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_find_busses:Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_find_devices:Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_device(Dev:PUSBDevHandle):PUSBDevice; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_get_busses: PUSBBus; MACRO_CALLING_CONV; external LIBUSB_LIB;
function  usb_get_driver_np(dev:PUSBDevHandle; TheInterface:Longint; Var name; namelen:LongInt) : Longint; MACRO_CALLING_CONV; external LIBUSB_LIB;
{$ELSE}
function  usb_open(dev:PUSBDevice):PUSBDevHandle;
function  usb_close(dev:PUSBDevHandle):Longint;
function  usb_get_string(dev:PUSBDevHandle; Index:Longint; langid:Longint; var Buf; buflen:Longint) : Longint;
function  usb_get_string_simple(dev:PUSBDevHandle; index:Longint; var Buf; buflen:Longint) : Longint;
function  usb_bulk_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
function  usb_bulk_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
function  usb_interrupt_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
function  usb_interrupt_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
function  usb_control_msg(dev:PUSBDevHandle; requestType:Longint; request:Longint; value:Longint; index:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
function  usb_set_configuration(dev:PUSBDevHandle; configuration:Longint):Longint;
function  usb_claim_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint;
function  usb_release_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint;
function  usb_set_altinterface(dev:PUSBDevHandle; alternate:Longint):Longint;
function  usb_resetep(dev:PUSBDevHandle; ep:dword):Longint;
function  usb_clear_halt(dev:PUSBDevHandle; ep:dword):Longint;
function  usb_reset(dev:PUSBDevHandle):Longint;
function  usb_strerror:PChar;
procedure usb_init;
procedure usb_set_debug(level:Longint);
function  usb_find_busses:Longint;
function  usb_find_devices:Longint;
function  usb_device(Dev:PUSBDevHandle):PUSBDevice;
function  usb_get_busses: PUSBBus;
function  usb_get_driver_np(dev:PUSBDevHandle; TheInterface:Longint; Var name; namelen:LongInt) : Longint;
{$ENDIF}

Implementation

uses uvusbrelaybase;

{$IFDEF USBHID_DYNAMIC}

type
  Tusb_open=function (dev:PUSBDevice):PUSBDevHandle; MACRO_CALLING_CONV;
  Tusb_close=function (dev:PUSBDevHandle):Longint; MACRO_CALLING_CONV;
  Tusb_get_string=function (dev:PUSBDevHandle; Index:Longint; langid:Longint; var Buf; buflen:Longint) : Longint; MACRO_CALLING_CONV;
  Tusb_get_string_simple=function (dev:PUSBDevHandle; index:Longint; var Buf; buflen:Longint) : Longint; MACRO_CALLING_CONV;
  Tusb_bulk_write=function (dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_bulk_read=function (dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_interrupt_write=function (dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_interrupt_read=function (dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_control_msg=function (dev:PUSBDevHandle; requestType:Longint; request:Longint; value:Longint; index:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_set_configuration=function (dev:PUSBDevHandle; configuration:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_claim_interface=function (dev:PUSBDevHandle; TheInterface:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_release_interface=function (dev:PUSBDevHandle; TheInterface:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_set_altinterface=function (dev:PUSBDevHandle; alternate:Longint):Longint; MACRO_CALLING_CONV;
  Tusb_resetep=function (dev:PUSBDevHandle; ep:dword):Longint; MACRO_CALLING_CONV;
  Tusb_clear_halt=function (dev:PUSBDevHandle; ep:dword):Longint; MACRO_CALLING_CONV;
  Tusb_reset=function (dev:PUSBDevHandle):Longint; MACRO_CALLING_CONV;
  Tusb_strerror=function :PChar; MACRO_CALLING_CONV;
  Tusb_init=procedure ; MACRO_CALLING_CONV;
  Tusb_set_debug=procedure (level:Longint); MACRO_CALLING_CONV;
  Tusb_find_busses=function :Longint; MACRO_CALLING_CONV;
  Tusb_find_devices=function :Longint; MACRO_CALLING_CONV;
  Tusb_device=function (Dev:PUSBDevHandle):PUSBDevice; MACRO_CALLING_CONV;
  Tusb_get_busses=function : PUSBBus; MACRO_CALLING_CONV;
  Tusb_get_driver_np=function (dev:PUSBDevHandle; TheInterface:Longint; Var name; namelen:LongInt) : Longint; MACRO_CALLING_CONV;

  TDLLlibusb01EntriesRecord = record
    usb_open: Tusb_open;
    usb_close: Tusb_close;
    usb_get_string: Tusb_get_string;
    usb_get_string_simple: Tusb_get_string_simple;
    usb_bulk_write: Tusb_bulk_write;
    usb_bulk_read: Tusb_bulk_read;
    usb_interrupt_write: Tusb_interrupt_write;
    usb_interrupt_read: Tusb_interrupt_read;
    usb_control_msg: Tusb_control_msg;
    usb_set_configuration: Tusb_set_configuration;
    usb_claim_interface: Tusb_claim_interface;
    usb_release_interface: Tusb_release_interface;
    usb_set_altinterface: Tusb_set_altinterface;
    usb_resetep: Tusb_resetep;
    usb_clear_halt: Tusb_clear_halt;
    usb_reset: Tusb_reset;
    usb_strerror: Tusb_strerror;
    usb_init: Tusb_init;
    usb_set_debug: Tusb_set_debug;
    usb_find_busses: Tusb_find_busses;
    usb_find_devices: Tusb_find_devices;
    usb_device: Tusb_device;
    usb_get_busses: Tusb_get_busses;
    usb_get_driver_np: Tusb_get_driver_np;
  end;

var
  DLLlibusb01Handle: TLibHandle;
  DLLInitializedFlag: Boolean=false;
  DLLInitializedSuccess: Boolean=false;
  DLLlibusb01Functions: TDLLlibusb01EntriesRecord;

  function InitializeLibraries: Boolean; forward;

function  usb_open(dev:PUSBDevice):PUSBDevHandle;
begin
  Result:=DLLlibusb01Functions.usb_open(dev);
end;

function  usb_close(dev:PUSBDevHandle):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_close(dev);
end;

function  usb_get_string(dev:PUSBDevHandle; Index:Longint; langid:Longint; var Buf; buflen:Longint) : Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_get_string(dev,Index,langid,Buf,buflen);
end;

function  usb_get_string_simple(dev:PUSBDevHandle; index:Longint; var Buf; buflen:Longint) : Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_get_string_simple(dev,index,Buf,buflen);
end;

function  usb_bulk_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_bulk_write(dev,ep,bytes,size,timeout);
end;

function  usb_bulk_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_bulk_read(dev,ep,bytes,size,timeout);
end;

function  usb_interrupt_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_interrupt_write(dev,ep,bytes,size,timeout);
end;

function  usb_interrupt_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_interrupt_read(dev,ep,bytes,size,timeout);
end;

function  usb_control_msg(dev:PUSBDevHandle; requestType:Longint; request:Longint; value:Longint; index:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_control_msg(dev,requestType,request,value,index,bytes,size,timeout);
end;

function  usb_set_configuration(dev:PUSBDevHandle; configuration:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_set_configuration(dev,configuration);
end;

function  usb_claim_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_claim_interface(dev,TheInterface);
end;

function  usb_release_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_release_interface(dev,TheInterface);
end;

function  usb_set_altinterface(dev:PUSBDevHandle; alternate:Longint):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_set_altinterface(dev,alternate);
end;

function  usb_resetep(dev:PUSBDevHandle; ep:dword):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_resetep(dev,ep);
end;

function  usb_clear_halt(dev:PUSBDevHandle; ep:dword):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_clear_halt(dev,ep);
end;

function  usb_reset(dev:PUSBDevHandle):Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_reset(dev);
end;

function  usb_strerror:PChar;
begin
  if not InitializeLibraries then exit(nil);
  Result:=DLLlibusb01Functions.usb_strerror();
end;

procedure usb_init;
begin
  if not InitializeLibraries then exit;
  DLLlibusb01Functions.usb_init;
end;

procedure usb_set_debug(level:Longint);
begin
  if not InitializeLibraries then exit;
  DLLlibusb01Functions.usb_set_debug(level);
end;

function  usb_find_busses:Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_find_busses();
end;

function  usb_find_devices:Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_find_devices();
end;

function  usb_device(Dev:PUSBDevHandle):PUSBDevice;
begin
  if not InitializeLibraries then exit(nil);
  Result:=DLLlibusb01Functions.usb_device(Dev);
end;

function  usb_get_busses: PUSBBus;
begin
  if not InitializeLibraries then exit(nil);
  Result:=DLLlibusb01Functions.usb_get_busses();
end;

function  usb_get_driver_np(dev:PUSBDevHandle; TheInterface:Longint; Var name; namelen:LongInt) : Longint;
begin
  if not InitializeLibraries then exit(USB_ERROR_BEGIN);
  Result:=DLLlibusb01Functions.usb_get_driver_np(dev,TheInterface,name,namelen);
end;

function InitializeLibraries: Boolean;
var
  lSuccessFunctions: Boolean=true;
  function CheckFunctionsRecord(p: Pointer; aEntries: integer): Boolean;
  var
    j: integer;
    pProc: Pointer;
    lAssigned: Boolean=true;
    pp: PPointer;
  begin
    pp:=p;
    for j := 0 to Pred(aEntries) do begin
      pProc:=pp^;
      if not Assigned(pProc) then begin
        lAssigned:=false;
        break;
      end;
      inc(pp);
    end;
    Result:=lAssigned;
  end;
begin
  if DLLInitializedFlag then exit(DLLInitializedSuccess);
  DLLlibusb01Functions:=Default(TDLLlibusb01EntriesRecord);
  DLLInitializedFlag:=true;
  DLLlibusb01Handle:=LoadLibrary(LIBUSB_LIB_DYN);
  if DLLlibusb01Handle<>0 then begin
    with DLLlibusb01Functions do begin
      Pointer(usb_open):=GetProcAddress(DLLlibusb01Handle,'usb_open');
      Pointer(usb_close):=GetProcAddress(DLLlibusb01Handle,'usb_close');
      Pointer(usb_get_string):=GetProcAddress(DLLlibusb01Handle,'usb_get_string');
      Pointer(usb_get_string_simple):=GetProcAddress(DLLlibusb01Handle,'usb_get_string_simple');
      Pointer(usb_bulk_write):=GetProcAddress(DLLlibusb01Handle,'usb_bulk_write');
      Pointer(usb_bulk_read):=GetProcAddress(DLLlibusb01Handle,'usb_bulk_read');
      Pointer(usb_interrupt_write):=GetProcAddress(DLLlibusb01Handle,'usb_interrupt_write');
      Pointer(usb_interrupt_read):=GetProcAddress(DLLlibusb01Handle,'usb_interrupt_read');
      Pointer(usb_control_msg):=GetProcAddress(DLLlibusb01Handle,'usb_control_msg');
      Pointer(usb_set_configuration):=GetProcAddress(DLLlibusb01Handle,'usb_set_configuration');
      Pointer(usb_claim_interface):=GetProcAddress(DLLlibusb01Handle,'usb_claim_interface');
      Pointer(usb_release_interface):=GetProcAddress(DLLlibusb01Handle,'usb_release_interface');
      Pointer(usb_set_altinterface):=GetProcAddress(DLLlibusb01Handle,'usb_set_altinterface');
      Pointer(usb_resetep):=GetProcAddress(DLLlibusb01Handle,'usb_resetep');
      Pointer(usb_clear_halt):=GetProcAddress(DLLlibusb01Handle,'usb_clear_halt');
      Pointer(usb_reset):=GetProcAddress(DLLlibusb01Handle,'usb_reset');
      Pointer(usb_strerror):=GetProcAddress(DLLlibusb01Handle,'usb_strerror');
      Pointer(usb_init):=GetProcAddress(DLLlibusb01Handle,'usb_init');
      Pointer(usb_set_debug):=GetProcAddress(DLLlibusb01Handle,'usb_set_debug');
      Pointer(usb_find_busses):=GetProcAddress(DLLlibusb01Handle,'usb_find_busses');
      Pointer(usb_find_devices):=GetProcAddress(DLLlibusb01Handle,'usb_find_devices');
      Pointer(usb_device):=GetProcAddress(DLLlibusb01Handle,'usb_device');
      Pointer(usb_get_busses):=GetProcAddress(DLLlibusb01Handle,'usb_get_busses');
      Pointer(usb_get_driver_np):=GetProcAddress(DLLlibusb01Handle,'usb_get_driver_np');
    end;
    if lSuccessFunctions then begin
      lSuccessFunctions:=CheckFunctionsRecord(@DLLlibusb01Functions,sizeof(DLLlibusb01Functions) div sizeof(Pointer));
    end;
  end else begin
    lSuccessFunctions:=false;
    DEBUG_OUTPUT('Library load error "'+LIBUSB_LIB_DYN+'"');
  end;
  if not lSuccessFunctions then begin
    DEBUG_OUTPUT(USBHID_ERROR_GETPROCADDRESS);
  end;
  DLLInitializedSuccess:=lSuccessFunctions;
  Result:=DLLInitializedSuccess;
end;

initialization
//  DEBUG_OUTPUT('libusb-0.1-dynamic');

finalization
  if DLLInitializedFlag then begin
    if (DLLlibusb01Handle<>0) then FreeLibrary(DLLlibusb01Handle);
  end;


{$ELSE}
initialization
//  DEBUG_OUTPUT('libusb-0.1-static');
{$ENDIF}

End.
