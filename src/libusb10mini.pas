unit libusb10mini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{.$DEFINE USBHID_DYNAMIC}

{$Macro ON}

{$IFDEF WINDOWS}
  {$DEFINE MACRO_CALLING_CONV:=stdcall}
{$ELSE}
  {$DEFINE MACRO_CALLING_CONV:=cdecl}
{$ENDIF}

const
  {$IFDEF WINDOWS}
    {$IFDEF WIN64}
      LIBUSB_LIB='libusb-1.0_x64.dll';
      LIBUSB_LIB_DYN='libusb-1.0_x64.dll';
    {$ELSE}
      LIBUSB_LIB='libusb-1.0_x86.dll';
      LIBUSB_LIB_DYN='libusb-1.0_x86.dll';
    {$ENDIF}
  {$ELSE}
  LIBUSB_LIB='usb-1.0';
  {$IFNDEF LIBUSB_LIB_DYN}
  LIBUSB_LIB_DYN='libusb-1.0.so.0';
  {$ENDIF}
  {$ENDIF}

{$PackRecords C}

type
  LIBUSB_RET_CODE=type Int32;

const
  //** Success (no error) */
  LIBUSB_SUCCESS: LIBUSB_RET_CODE = 0;

  //** Input/output error */
  LIBUSB_ERROR_IO: LIBUSB_RET_CODE  = -1;

  //** Invalid parameter */
  LIBUSB_ERROR_INVALID_PARAM: LIBUSB_RET_CODE = -2;

  //** Access denied (insufficient permissions) */
  LIBUSB_ERROR_ACCESS: LIBUSB_RET_CODE = -3;

  //** No such device (it may have been disconnected) */
  LIBUSB_ERROR_NO_DEVICE: LIBUSB_RET_CODE = -4;

  //** Entity not found */
  LIBUSB_ERROR_NOT_FOUND: LIBUSB_RET_CODE = -5;

  //** Resource busy */
  LIBUSB_ERROR_BUSY: LIBUSB_RET_CODE = -6;

  //** Operation timed out */
  LIBUSB_ERROR_TIMEOUT: LIBUSB_RET_CODE = -7;

  //** Overflow */
  LIBUSB_ERROR_OVERFLOW: LIBUSB_RET_CODE = -8;

  //** Pipe error */
  LIBUSB_ERROR_PIPE: LIBUSB_RET_CODE = -9;

  //** System call interrupted (perhaps due to signal) */
  LIBUSB_ERROR_INTERRUPTED: LIBUSB_RET_CODE = -10;

  //** Insufficient memory */
  LIBUSB_ERROR_NO_MEM: LIBUSB_RET_CODE = -11;

  //** Operation not supported or unimplemented on this platform */
  LIBUSB_ERROR_NOT_SUPPORTED: LIBUSB_RET_CODE = -12;

  //* NB! Remember to update libusb_error_name()
  //   when adding new error codes here. */

  //** Other error */
  LIBUSB_ERROR_OTHER: LIBUSB_RET_CODE = -99;

  //enum libusb_request_type
	//** Standard */
	LIBUSB_REQUEST_TYPE_STANDARD = ($00 shl 5);

	//** Class */
	LIBUSB_REQUEST_TYPE_CLASS = ($01 shl 5);

	//** Vendor */
	LIBUSB_REQUEST_TYPE_VENDOR = ($02 shl 5);

	//** Reserved */
	LIBUSB_REQUEST_TYPE_RESERVED = ($03 shl 5);

  //enum libusb_request_recipient
	//** Device */
	LIBUSB_RECIPIENT_DEVICE = $00;

  //** Interface */
  LIBUSB_RECIPIENT_INTERFACE = $01;

  //** Endpoint */
  LIBUSB_RECIPIENT_ENDPOINT = $02;

  //** Other */
  LIBUSB_RECIPIENT_OTHER = $03;

  //enum libusb_endpoint_direction
	//** In: device-to-host */
  LIBUSB_ENDPOINT_IN = $80;

  //** Out: host-to-device */
  LIBUSB_ENDPOINT_OUT = $00;

  // enum libusb_descriptor_type
  //** Device descriptor. See libusb_device_descriptor. */
  LIBUSB_DT_DEVICE = $01;

  //** Configuration descriptor. See libusb_config_descriptor. */
  LIBUSB_DT_CONFIG = $02;

  //** String descriptor */
  LIBUSB_DT_STRING = $03;

  //** Interface descriptor. See libusb_interface_descriptor. */
  LIBUSB_DT_INTERFACE = $04;

  //** Endpoint descriptor. See libusb_endpoint_descriptor. */
  LIBUSB_DT_ENDPOINT = $05;

  //** HID descriptor */
  LIBUSB_DT_HID = $21;

  //** HID report descriptor */
  LIBUSB_DT_REPORT = $22;

  //** Physical descriptor */
  LIBUSB_DT_PHYSICAL = $23;

  //** Hub descriptor */
  LIBUSB_DT_HUB = $29;

  // enum libusb_standard_request
  //** Request status of the specific recipient */
  LIBUSB_REQUEST_GET_STATUS = $00;

  //** Clear or disable a specific feature */
  LIBUSB_REQUEST_CLEAR_FEATURE = $01;

  //* 0x02 is reserved */

  //** Set or enable a specific feature */
  LIBUSB_REQUEST_SET_FEATURE = $03;

  //* 0x04 is reserved */

  //** Set device address for all future accesses */
  LIBUSB_REQUEST_SET_ADDRESS = $05;

  //** Get the specified descriptor */
  LIBUSB_REQUEST_GET_DESCRIPTOR = $06;

  //** Used to update existing descriptors or add new descriptors */
  LIBUSB_REQUEST_SET_DESCRIPTOR = $07;

  //** Get the current device configuration value */
  LIBUSB_REQUEST_GET_CONFIGURATION = $08;

  //** Set device configuration */
  LIBUSB_REQUEST_SET_CONFIGURATION = $09;

  //** Return the selected alternate setting for the specified interface */
  LIBUSB_REQUEST_GET_INTERFACE = $0A;

  //** Select an alternate interface for the specified interface */
  LIBUSB_REQUEST_SET_INTERFACE = $0B;

  //** Set then report an endpoint's synchronization frame */
  LIBUSB_REQUEST_SYNCH_FRAME = $0C;


type

  OpaquePointer=type Pointer;
  libusb_device=type OpaquePointer;
  Plibusb_device=^libusb_device;
  PPlibusb_device=^Plibusb_device;
  PPPlibusb_device=^PPlibusb_device;
  Plibusb_device_list=PPPlibusb_device;
  libusb_context=type OpaquePointer;
  Plibusb_context=^libusb_context;
  PPlibusb_context=^Plibusb_context;
  libusb_device_handle=type Pointer;
  Plibusb_device_handle=^libusb_device_handle;

  libusb_device_descriptor = record
  	//** Size of this descriptor (in bytes) */
  	bLength: UInt8;

  	//** Descriptor type. Will have value
  	// * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE LIBUSB_DT_DEVICE in this
  	// * context. */
  	bDescriptorType: UInt8;

  	//** USB specification release number in binary-coded decimal. A value of
  	// * 0x0200 indicates USB 2.0, 0x0110 indicates USB 1.1, etc. */
  	bcdUSB: UInt16;

  	//** USB-IF class code for the device. See \ref libusb_class_code. */
  	bDeviceClass: UInt8;

  	//** USB-IF subclass code for the device, qualified by the bDeviceClass
  	// * value */
  	bDeviceSubClass: UInt8;

  	//** USB-IF protocol code for the device, qualified by the bDeviceClass and
  	// * bDeviceSubClass values */
  	bDeviceProtocol: UInt8;

  	//** Maximum packet size for endpoint 0 */
  	bMaxPacketSize0: UInt8;

  	//** USB-IF vendor ID */
  	idVendor: UInt16;

  	//** USB-IF product ID */
  	idProduct: UInt16;

  	//** Device release number in binary-coded decimal */
  	bcdDevice: UInt16;

  	//** Index of string descriptor describing manufacturer */
  	iManufacturer: UInt8;

  	//** Index of string descriptor describing product */
  	iProduct: UInt8;

  	//** Index of string descriptor containing device serial number */
  	iSerialNumber: UInt8;

  	//** Number of possible configurations */
  	bNumConfigurations: UInt8;
  end;
  Plibusb_device_descriptor = ^libusb_device_descriptor;

{$IFNDEF USBHID_DYNAMIC}
  function  libusb_init(ctx: PPlibusb_context): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;
  procedure libusb_exit(ctx: Plibusb_context); MACRO_CALLING_CONV; external LIBUSB_LIB;
  function  libusb_get_device_list(ctx: Plibusb_context;	var list: PPlibusb_device): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;
  procedure libusb_free_device_list(list: PPlibusb_device; unref_devices: integer); MACRO_CALLING_CONV; external LIBUSB_LIB;
  function  libusb_get_device_descriptor(var dev: libusb_device; var desc: libusb_device_descriptor): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;
  function  libusb_open(var dev: libusb_device; var handle: libusb_device_handle): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;
  procedure libusb_close(dev_handle: libusb_device_handle); MACRO_CALLING_CONV; external LIBUSB_LIB;

  function  libusb_control_transfer(dev_handle: libusb_device_handle;
                                  	request_type: UInt8; bRequest: UInt8;
                                    wValue: UInt16; wIndex: UInt16;
                                    data: pchar; wLength: UInt16;
                                    timeout: SizeUInt): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;

  function  libusb_get_string_descriptor_ascii(dev: libusb_device_handle; desc_index: UInt8; data: PBYTE; length: Int32): LIBUSB_RET_CODE; MACRO_CALLING_CONV; external LIBUSB_LIB;
{$ELSE}
  function  libusb_init(ctx: PPlibusb_context): LIBUSB_RET_CODE;
  procedure libusb_exit(ctx: Plibusb_context);
  function  libusb_get_device_list(ctx: Plibusb_context;	var list: PPlibusb_device): LIBUSB_RET_CODE;
  procedure libusb_free_device_list(list: PPlibusb_device; unref_devices: integer);
  function  libusb_get_device_descriptor(var dev: libusb_device; var desc: libusb_device_descriptor): LIBUSB_RET_CODE;
  function  libusb_open(var dev: libusb_device; var handle: libusb_device_handle): LIBUSB_RET_CODE;
  procedure libusb_close(dev_handle: libusb_device_handle);

  function  libusb_control_transfer(dev_handle: libusb_device_handle;
                                	  request_type: UInt8; bRequest: UInt8;
                                    wValue: UInt16; wIndex: UInt16;
                                    data: pchar; wLength: UInt16;
                                    timeout: SizeUInt): LIBUSB_RET_CODE;

  function  libusb_get_string_descriptor_ascii(dev: libusb_device_handle; desc_index: UInt8; data: PBYTE; length: Int32): LIBUSB_RET_CODE;
{$ENDIF}

implementation

uses uvusbrelaybase;

{$IFDEF USBHID_DYNAMIC}

type
  Tlibusb_init=function  (ctx: PPlibusb_context): LIBUSB_RET_CODE; MACRO_CALLING_CONV;
  Tlibusb_exit=procedure (ctx: Plibusb_context); MACRO_CALLING_CONV;
  Tlibusb_get_device_list=function  (ctx: Plibusb_context;	var list: PPlibusb_device): LIBUSB_RET_CODE; MACRO_CALLING_CONV;
  Tlibusb_free_device_list=procedure (list: PPlibusb_device; unref_devices: integer); MACRO_CALLING_CONV;
  Tlibusb_get_device_descriptor=function  (var dev: libusb_device; var desc: libusb_device_descriptor): LIBUSB_RET_CODE; MACRO_CALLING_CONV;
  Tlibusb_open=function  (var dev: libusb_device; var handle: libusb_device_handle): LIBUSB_RET_CODE; MACRO_CALLING_CONV;
  Tlibusb_close=procedure (dev_handle: libusb_device_handle); MACRO_CALLING_CONV;

  Tlibusb_control_transfer=function  (dev_handle: libusb_device_handle;
                                	  request_type: UInt8; bRequest: UInt8;
                                    wValue: UInt16; wIndex: UInt16;
                                    data: pchar; wLength: UInt16;
                                    timeout: SizeUInt): LIBUSB_RET_CODE; MACRO_CALLING_CONV;

  Tlibusb_get_string_descriptor_ascii=function  (dev: libusb_device_handle; desc_index: UInt8; data: PBYTE; length: Int32): LIBUSB_RET_CODE; MACRO_CALLING_CONV;

  TDLLlibusb10EntriesRecord=record
    libusb_init: Tlibusb_init;
    libusb_exit: Tlibusb_exit;
    libusb_get_device_list: Tlibusb_get_device_list;
    libusb_free_device_list: Tlibusb_free_device_list;
    libusb_get_device_descriptor: Tlibusb_get_device_descriptor;
    libusb_open: Tlibusb_open;
    libusb_close: Tlibusb_close;
    libusb_control_transfer: Tlibusb_control_transfer;
    libusb_get_string_descriptor_ascii: Tlibusb_get_string_descriptor_ascii;
  end;

var
  DLLlibusb10Handle: TLibHandle;
  DLLInitializedFlag: Boolean=false;
  DLLInitializedSuccess: Boolean=false;
  DLLlibusb10Functions: TDLLlibusb10EntriesRecord;

  function InitializeLibraries: Boolean; forward;

function libusb_init(ctx: PPlibusb_context): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_init(ctx);
end;

procedure libusb_exit(ctx: Plibusb_context);
begin
  if not InitializeLibraries then exit;
  DLLlibusb10Functions.libusb_exit(ctx);
end;

function libusb_get_device_list(ctx: Plibusb_context; var list: PPlibusb_device): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_get_device_list(ctx,list);
end;

procedure libusb_free_device_list(list: PPlibusb_device; unref_devices: integer);
begin
  if not InitializeLibraries then exit;
  DLLlibusb10Functions.libusb_free_device_list(list,unref_devices);
end;

function libusb_get_device_descriptor(var dev: libusb_device; var desc: libusb_device_descriptor): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_get_device_descriptor(dev,desc);
end;

function libusb_open(var dev: libusb_device; var handle: libusb_device_handle): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_open(dev,handle);
end;

procedure libusb_close(dev_handle: libusb_device_handle);
begin
  if not InitializeLibraries then exit;
  DLLlibusb10Functions.libusb_close(dev_handle);
end;

function libusb_control_transfer(dev_handle: libusb_device_handle; request_type: UInt8; bRequest: UInt8; wValue: UInt16; wIndex: UInt16; data: pchar; wLength: UInt16; timeout: SizeUInt
  ): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_control_transfer(dev_handle,request_type,bRequest,wValue,wIndex,data,wLength,timeout);
end;

function libusb_get_string_descriptor_ascii(dev: libusb_device_handle; desc_index: UInt8; data: PBYTE; length: Int32): LIBUSB_RET_CODE;
begin
  if not InitializeLibraries then exit(LIBUSB_ERROR_OTHER);
  Result:=DLLlibusb10Functions.libusb_get_string_descriptor_ascii(dev,desc_index,data,length);
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
  DLLlibusb10Functions:=Default(TDLLlibusb10EntriesRecord);
  DLLInitializedFlag:=true;
  DLLlibusb10Handle:=LoadLibrary(LIBUSB_LIB);
  if DLLlibusb10Handle<>0 then begin
    with DLLlibusb10Functions do begin
      Pointer(libusb_init):=GetProcAddress(DLLlibusb10Handle,'libusb_init');
      Pointer(libusb_exit):=GetProcAddress(DLLlibusb10Handle,'libusb_exit');
      Pointer(libusb_get_device_list):=GetProcAddress(DLLlibusb10Handle,'libusb_get_device_list');
      Pointer(libusb_free_device_list):=GetProcAddress(DLLlibusb10Handle,'libusb_free_device_list');
      Pointer(libusb_get_device_descriptor):=GetProcAddress(DLLlibusb10Handle,'libusb_get_device_descriptor');
      Pointer(libusb_open):=GetProcAddress(DLLlibusb10Handle,'libusb_open');
      Pointer(libusb_close):=GetProcAddress(DLLlibusb10Handle,'libusb_close');
      Pointer(libusb_control_transfer):=GetProcAddress(DLLlibusb10Handle,'libusb_control_transfer');
      Pointer(libusb_get_string_descriptor_ascii):=GetProcAddress(DLLlibusb10Handle,'libusb_get_string_descriptor_ascii');
    end;
    if lSuccessFunctions then begin
      lSuccessFunctions:=CheckFunctionsRecord(@DLLlibusb10Functions,sizeof(DLLlibusb10Functions) div sizeof(Pointer));
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
//  DEBUG_OUTPUT('libusb-1.0-dynamic');

finalization
  if DLLInitializedFlag then begin
    if (DLLlibusb10Handle<>0) then FreeLibrary(DLLlibusb10Handle);
  end;

{$ELSE}
initialization
//  DEBUG_OUTPUT('libusb-1.0-static');

{$ENDIF}

end.

