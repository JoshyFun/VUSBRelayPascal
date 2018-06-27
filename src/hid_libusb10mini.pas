unit hid_libusb10mini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvusbrelaybase, libusb10mini;

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
  mListOfUSBDevices: PPlibusb_device=nil;

function IsLibUSBSuccess(const aRetCode: LIBUSB_RET_CODE): Boolean; overload;
begin
  if aRetCode<0 then begin
    Result:=false;
  end else begin
    Result:=true;
  end;
end;

function IsLibUSBSuccess(const aRetCode: LIBUSB_RET_CODE; out aStoreRetCode: LIBUSB_RET_CODE): Boolean; overload;
begin
  aStoreRetCode:=aRetCode;
  if aRetCode<0 then begin
    Result:=false;
  end else begin
    Result:=true;
  end;
end;

procedure FinalizeUSBDeviceList;
begin
  if Assigned(mListOfUSBDevices) then begin
    libusb_free_device_list(mListOfUSBDevices,1);
    mListOfUSBDevices:=nil;
  end;
end;

function usbhidEnumDevices(aVendor: Integer; aProduct: integer; aContext: Pointer; aEnumFunc: usbhidEnumFunc): integer;
var
  lRetCode: LIBUSB_RET_CODE;
  lNumberOfDevices, j: integer;
  lDescriptor: libusb_device_descriptor;
  lDescriptorStore: Plibusb_device_descriptor;
  lHandle: USBDEVHANDLE;
  lHandlelibusb: libusb_device_handle;
begin
  FinalizeUSBDeviceList; //Safeguard
  Result:=USBHID_OK;
  if not IsLibUSBSuccess(libusb_init(nil),lRetCode) then begin
    exit(USBHID_ERR_ACCESS);
  end;
  if not IsLibUSBSuccess(libusb_get_device_list(nil,mListOfUSBDevices),lRetCode) then begin
    exit(USBHID_ERR_UNKNOWN);
  end;
  lNumberOfDevices:=lRetCode;

  for j := 0 to Pred(lNumberOfDevices) do begin
    lDescriptor:=Default(libusb_device_descriptor);
    if not IsLibUSBSuccess(libusb_get_device_descriptor(mListOfUSBDevices[j]^,lDescriptor),lRetCode) then begin
      DEBUG_OUTPUT(USBHID_ERROR_READING_DESCRIPTOR+ ' Index: '+inttostr(j));
    end else begin
      DEBUG_USBIDS(lDescriptor.idVendor,lDescriptor.idProduct,lDescriptor.bcdDevice);
      if (lDescriptor.idVendor=aVendor) and (lDescriptor.idProduct=aProduct) then begin
        lHandlelibusb:=nil;
        if IsLibUSBSuccess(libusb_open(mListOfUSBDevices[j]^, lHandlelibusb),lRetCode) then begin
          new(lHandle);
          lHandle^.TheDev:=mListOfUSBDevices[j]^;
          lHandle^.TheHandlePointer:=lHandlelibusb;
          new(lDescriptorStore);
          move(lDescriptor,lDescriptorStore^,sizeof(lDescriptor));
          lHandle^.TheDescriptorPointer:=lDescriptorStore;
          DEBUG_OUTPUT(USBHID_MSG_CALLING_ENUMERATE);
          if aEnumFunc(lHandle,aContext)=0 then begin
            // Stop enumerating
            break;
          end;
          // Handle is keep by the enumFunc and it must close it.
        end else begin
          DEBUG_OUTPUT(USBHID_ERROR_OPEN_FAIL);
        end;
      end;
    end;
  end;
end;

procedure usbhidCloseDevice(usbh: USBDEVHANDLE);
begin
  libusb_close(usbh^.TheHandlePointer);
  Dispose(Plibusb_device_descriptor(usbh^.TheDescriptorPointer));
  Dispose(usbh);
end;

function usbhidGetVendorString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lRetCode: LIBUSB_RET_CODE;
begin
  SetLength(aString,128);
  if not IsLibUSBSuccess(libusb_get_string_descriptor_ascii(usbh^.TheHandlePointer,
                        Plibusb_device_descriptor(usbh^.TheDescriptorPointer)^.iManufacturer,
                        @aString[1], Length(aString)), lRetCode) then begin
    aString:='';
    DEBUG_OUTPUT (USBHID_ERROR_READING_VENDOR);
    Result:=USBHID_ERR_IO;
  end else begin
    SetLength(aString,lRetCode);
    Result:=USBHID_OK;
  end;
end;

function usbhidGetProductString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lRetCode: LIBUSB_RET_CODE;
begin
  SetLength(aString,128);
  if not IsLibUSBSuccess(libusb_get_string_descriptor_ascii(usbh^.TheHandlePointer,
                        Plibusb_device_descriptor(usbh^.TheDescriptorPointer)^.iProduct,
                        @aString[1], Length(aString)),lRetCode) then begin
    aString:='';
    DEBUG_OUTPUT (USBHID_ERROR_READING_PRODUCT);
    Result:=USBHID_ERR_IO;
  end else begin
    SetLength(aString,lRetCode);
    Result:=USBHID_OK;
  end;
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
  BytesReceived: LIBUSB_RET_CODE;
  lReportNumber: integer=0;
begin
  DEBUG_OUTPUT (format(USBHID_MSG_SET_REPORT ,[HexbufToStr(Buffer,Len)]));
  inc(Buffer);
  dec(len);
  if not IsLibUSBSuccess(libusb_control_transfer(usbh^.TheHandlePointer,
          LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_DEVICE or LIBUSB_ENDPOINT_OUT,
          USBRQ_HID_SET_REPORT,
          (USB_HID_REPORT_TYPE_FEATURE shl 8) or lReportNumber,
          0, buffer, Len, A_REPORT_REQUEST_TIMEOUT), BytesReceived) then begin
    DEBUG_OUTPUT (USBHID_ERROR_SET_REPORT);
    Result:=USBHID_ERR_IO;
  end else begin
    Result:=USBHID_OK;
  end;
end;

function usbhidGetReport(usbh: USBDEVHANDLE; ReportNumber: integer; Buffer: pchar; var len: integer): integer;
var
  BytesReceived: LIBUSB_RET_CODE;
begin
  if not IsLibUSBSuccess(libusb_control_transfer(usbh^.TheHandlePointer,
          LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_DEVICE or LIBUSB_ENDPOINT_IN,
          USBRQ_HID_GET_REPORT,
          (USB_HID_REPORT_TYPE_FEATURE shl 8) or ReportNumber,
          0, buffer, Len, A_REPORT_REQUEST_TIMEOUT), BytesReceived) then begin
    DEBUG_OUTPUT (USBHID_ERROR_GET_REPORT);
    len:=0;
    Result:=USBHID_ERR_IO;
  end else begin
    move(Buffer^,(Buffer+1)^,8);
    PBYTE(Buffer)^:=ReportNumber;
    DEBUG_OUTPUT (format(USBHID_MSG_GET_REPORT ,[HexbufToStr(Buffer,Len)]));
    len:=BytesReceived;
    Result:=USBHID_OK;
  end;
end;

finalization
  FinalizeUSBDeviceList;

end.

