unit hid_libusb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvusbrelaybase, libusb;

function  usbhidEnumDevices(aVendor: Integer; aProduct: integer; aContext: Pointer; aEnumFunc: usbhidEnumFunc): integer;
procedure usbhidCloseDevice(usbh: USBDEVHANDLE);
function  usbhidGetVendorString(usbh: USBDEVHANDLE; out aString: string): integer;
function  usbhidGetProductString(usbh: USBDEVHANDLE; out aString: string): integer;
function  usbhidSetReport(usbh: USBDEVHANDLE; Buffer: PChar; len: integer): integer;
function  usbhidGetReport(usbh: USBDEVHANDLE; ReportNumber: integer; Buffer: pchar; var len: integer): integer;

implementation

const
  USBRQ_HID_GET_REPORT =         $01;
  USBRQ_HID_SET_REPORT =         $09;
  USB_HID_REPORT_TYPE_FEATURE =  $03;

  A_REPORT_REQUEST_TIMEOUT =     5000; // Milliseconds

var
  mUSBInitialized: Boolean=false;

function  usbhidEnumDevices(aVendor: Integer; aProduct: integer; aContext: Pointer; aEnumFunc: usbhidEnumFunc): integer;
var
  lBus: PUSBBus;
  lDevice: PUSBDevice;
  lHandle: PUSBDevHandle=nil;
  lErrorCode: Integer = USBHID_ERR_NOTFOUND;
  lUSBDEVHandle: USBDEVHANDLE;
begin
  if not mUSBInitialized then begin
    usb_init;
    mUSBInitialized := true;
  end;
  usb_find_busses;
  usb_find_devices;
  lBus := usb_get_busses;
  while lBus<>nil do
  begin
    lDevice := lBus^.devices;
    while Assigned(lDevice) do begin
      DEBUG_USBIDS(lDevice^.descriptor.idVendor,lDevice^.descriptor.idProduct,lDevice^.descriptor.bcdDevice);
      if (lDevice^.descriptor.idVendor = aVendor) and  (lDevice^.descriptor.idProduct = aProduct) then begin
        lHandle := usb_open (lDevice);
        if not Assigned(lHandle) then begin
          lErrorCode := USBHID_ERR_ACCESS;
          DEBUG_OUTPUT (USBHID_ERROR_OPEN_FAIL + ' ('+usb_strerror+')');
          lDevice := lDevice^.next;
          continue;
        end;
        lErrorCode := 0;
        new(lUSBDEVHandle);
        lUSBDEVHandle^.TheHandlePointer:=lHandle;
        lUSBDEVHandle^.TheDev:=lDevice;
        DEBUG_OUTPUT(USBHID_MSG_CALLING_ENUMERATE);
        if aEnumFunc (lUSBDEVHandle, aContext) = 0 then begin
          // Stop enumerating
          break;
        end;
        // Handle is keep by the enumFunc and it must close it.
        lHandle := nil;
      end;
      lDevice := lDevice^.next;
    end;
    lBus := lBus^.next;
  end;
  exit (lErrorCode);
end;

procedure usbhidCloseDevice(usbh: USBDEVHANDLE);
begin
  usb_close(usbh^.TheHandlePointer);
  dispose(usbh);
end;

function usbhidGetStringAscii(usbh: USBDEVHANDLE; aIndex: integer; buf: pchar; buflen: integer): integer;
var
  lRetCode: integer;
begin
  lRetCode := usb_get_string_simple(usbh^.TheHandlePointer, aIndex, buf^, buflen);
  if lRetCode>=0 then begin
    Result:=lRetCode;
  end else begin
    if lRetCode=-1 then begin
      DEBUG_OUTPUT(USBHID_ERROR_ACCESS_DENIED);
    end;
    //DEBUG_OUTPUT(format('%s error %s',['usbhidGetStringAscii', usb_strerror()]));
    Result:=-1;
  end;
end;

function usbhidGetVendorString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lRetCode: integer;
begin
  SetLength(aString,128);
  lRetCode := usbhidGetStringAscii(usbh, PUSBDevice(usbh^.TheDev)^.descriptor.iManufacturer, @aString[1], Length(aString));
  if lRetCode<0 then begin
    aString:='';
    DEBUG_OUTPUT (USBHID_ERROR_READING_VENDOR);
    exit (USBHID_ERR_IO);
  end;
  SetLength(aString,lRetCode);
  Result:=USBHID_OK;
end;

function usbhidGetProductString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lRetCode: integer;
begin
  SetLength(aString,128);
  lRetCode := usbhidGetStringAscii(usbh, PUSBDevice(usbh^.TheDev)^.descriptor.iProduct, @aString[1], Length(aString));
  if lRetCode < 0 then begin
    aString:='';
    DEBUG_OUTPUT (USBHID_ERROR_READING_PRODUCT);
    exit (USBHID_ERR_IO);
  end;
  SetLength(aString,lRetCode);
  Result:=USBHID_OK;
end;

function HexbufToStr(aP: pchar; const aLen: integer): string;
var
  j: integer;
begin
  Result:='';
  for j := 0 to Pred(aLen) do
  begin
    Result:=Result+format('%2.2X ',[BYTE(aP^)]);
    inc(aP);
  end;
end;

function usbhidSetReport(usbh: USBDEVHANDLE; Buffer: PChar; len: integer): integer;
var
  BytesReceived: integer;
  lReportNumber: integer=0;
begin
  inc(Buffer);
  dec(len);
  DEBUG_OUTPUT (format(USBHID_MSG_SET_REPORT ,[HexbufToStr(Buffer,Len)]));
  BytesReceived := usb_control_msg(usbh^.TheHandlePointer,
          USB_TYPE_CLASS or USB_RECIP_DEVICE or USB_ENDPOINT_OUT,
          USBRQ_HID_SET_REPORT,
          (USB_HID_REPORT_TYPE_FEATURE shl 8) or lReportNumber,
          0, buffer, Len, A_REPORT_REQUEST_TIMEOUT);
  if bytesReceived<0 then begin
    DEBUG_OUTPUT (USBHID_ERROR_SET_REPORT);
    Result:=USBHID_ERR_IO;
  end else begin
    Result:=USBHID_OK;
  end;
end;

function usbhidGetReport(usbh: USBDEVHANDLE; ReportNumber: integer; Buffer: pchar; var len: integer): integer;
var
  BytesReceived: integer;
  lReportNumber: integer=0;
begin
  BytesReceived := usb_control_msg(usbh^.TheHandlePointer,
          USB_TYPE_CLASS or USB_RECIP_DEVICE or USB_ENDPOINT_IN,
          USBRQ_HID_GET_REPORT,
          (USB_HID_REPORT_TYPE_FEATURE shl 8) or lReportNumber,
          0, buffer, Len, A_REPORT_REQUEST_TIMEOUT);
  if BytesReceived < 0 then begin
    DEBUG_OUTPUT (USBHID_ERROR_GET_REPORT);
    len:=0;
    Result:=USBHID_ERR_IO;
  end else begin
    move(Buffer^,(Buffer+1)^,8);
    PBYTE(Buffer)^:=lReportNumber;
    DEBUG_OUTPUT (format(USBHID_MSG_GET_REPORT ,[HexbufToStr(Buffer,Len)]));
    len:=BytesReceived;
    Result:=USBHID_OK;
  end;
end;

end.

