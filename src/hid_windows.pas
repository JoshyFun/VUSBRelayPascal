unit hid_windows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvusbrelaybase;

function  usbhidEnumDevices(aVendor: Integer; aProduct: integer; aContext: Pointer; aEnumFunc: usbhidEnumFunc): integer;
procedure usbhidCloseDevice(usbh: USBDEVHANDLE);
function  usbhidGetVendorString(usbh: USBDEVHANDLE; out aString: string): integer;
function  usbhidGetProductString(usbh: USBDEVHANDLE; out aString: string): integer;
function  usbhidSetReport(usbh: USBDEVHANDLE; Buffer: PChar; len: integer): integer;
function  usbhidGetReport(usbh: USBDEVHANDLE; ReportNumber: integer; Buffer: pchar; var len: integer): integer;

implementation

uses
  windows;

const
  WINDOWS_DLLHID_NAME='hid.dll';
  WINDOWS_SETUPAPI_NAME='setupapi.dll';

  DIGCF_DEFAULT =          $00000001;
  DIGCF_PRESENT =          $00000002;
  DIGCF_ALLCLASSES =       $00000004;
  DIGCF_PROFILE =          $00000008;
  DIGCF_DEVICEINTERFACE =  $00000010;

type

{$PackRecords C}

  HDEVINFO = type THANDLE;

  SP_DEVINFO_DATA = record
      cbSize: DWORD;
      ClassGuid: TGUID;
      DevInst: DWORD;    // DEVINST handle
      Reserved: Pointer;
  end;
  PSP_DEVINFO_DATA=^SP_DEVINFO_DATA;

  SP_DEVICE_INTERFACE_DATA = record
      cbSize: DWORD;
      InterfaceClassGuid: TGUID;
      Flags: DWORD;
      Reserved: Pointer;
  end;
  PSP_DEVICE_INTERFACE_DATA= ^SP_DEVICE_INTERFACE_DATA;

  SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
      cbSize: DWORD;
      DevicePath: array [0..0] of WideChar;
  end;
  PSP_DEVICE_INTERFACE_DETAIL_DATA_W = ^SP_DEVICE_INTERFACE_DETAIL_DATA_W;

  HIDD_ATTRIBUTES = record
    Size: DWORD;
    VendorID: WORD;
    ProductID: WORD;
    VersionNumber: WORD;
  end;

  // HID.DLL
  THidD_GetHidGuid = procedure (var HIDGuid: THIDGUID); stdcall;
  THidD_GetAttributes = function (Device: THANDLE; var Attributes: HIDD_ATTRIBUTES): WINBOOL; stdcall;
  THidD_GetManufacturerString = function (Device: THANDLE; Buffer: Pointer; BufferLen: DWORD): WINBOOL; stdcall;
  THidD_GetProductString = function (Device: THANDLE; Buffer: Pointer; BufferLen: DWORD): WINBOOL; stdcall;
  THidD_GetSerialNumberString = function (Device: THANDLE; Buffer: Pointer; BufferLen: DWORD): WINBOOL; stdcall;
  THidD_GetFeature = function (Device: THANDLE; ReportBuffer: Pointer; ReportBufferLen: DWORD): WINBOOL; stdcall;
  THidD_SetFeature = function (Device: THANDLE; ReportBuffer: Pointer; ReportBufferLen: DWORD): WINBOOL; stdcall;
  THidD_GetNumInputBuffers = function (Device: THANDLE; var numBuffers: DWORD): WINBOOL; stdcall;
  THidD_SetNumInputBuffers = function (Device: THANDLE; var numBuffers: DWORD): WINBOOL; stdcall;
  // SETUPAPI.DLL
  TSetupDiGetClassDevs = function (var aGUID: THIDGUID; aEnumerator: PWideChar; hwndParent: SizeInt; aFlags: DWORD): HDEVINFO; stdcall;
  TSetupDiEnumDeviceInterfaces = function (DeviceInfoSet: HDEVINFO; DeviceInfoData: PSP_DEVINFO_DATA; var InterfaceClassGuid: TGUID; MemberIndex: DWORD; var DeviceInterfaceData: SP_DEVICE_INTERFACE_DATA): WINBOOL; stdcall;
  TSetupDiGetDeviceInterfaceDetailW = function (DeviceInfoSet: HDEVINFO; DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA;
                                              DeviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA_W;
                                              DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
                                              DeviceInfoData: PSP_DEVINFO_DATA): WINBOOL; stdcall;
  TSetupDiDestroyDeviceInfoList = function (DeviceInfoSet: HDEVINFO): WINBOOL; stdcall;

type

  TDLLHIDEntriesRecord=record
    HidD_GetHidGuid: THidD_GetHidGuid;
    HidD_GetAttributes: THidD_GetAttributes;
    HidD_GetManufacturerString: THidD_GetManufacturerString;
    HidD_GetProductString: THidD_GetProductString;
    HidD_GetSerialNumberString: THidD_GetSerialNumberString;
    HidD_GetFeature: THidD_GetFeature;
    HidD_SetFeature: THidD_SetFeature;
    HidD_GetNumInputBuffers: THidD_GetNumInputBuffers;
    HidD_SetNumInputBuffers: THidD_SetNumInputBuffers;
  end;

  TDLLSetupAPIEntriesRecord=record
    SetupDiGetClassDevs: TSetupDiGetClassDevs;
    SetupDiEnumDeviceInterfaces: TSetupDiEnumDeviceInterfaces;
    SetupDiGetDeviceInterfaceDetailW: TSetupDiGetDeviceInterfaceDetailW;
    SetupDiDestroyDeviceInfoList: TSetupDiDestroyDeviceInfoList;
  end;

var
  DLLHIDHandle: TLibHandle;
  DLLSetupAPIHandle: TLibHandle;
  DLLInitializedFlag: Boolean=false;
  DLLInitializedSuccess: Boolean=false;
  DLLHIDFunctions: TDLLHIDEntriesRecord;
  DLLSetupAPIFunctions: TDLLSetupAPIEntriesRecord;

function InitializeLibraries: Boolean; forward;

function usbhidEnumDevices(aVendor: Integer; aProduct: integer; aContext: Pointer; aEnumFunc: usbhidEnumFunc): integer;
var
  lGUID: THIDGUID;
  lDeviceInfoList: HDEVINFO;
  lDeviceInfo: SP_DEVICE_INTERFACE_DATA;
  lDeviceDetails: PSP_DEVICE_INTERFACE_DETAIL_DATA_W;
  lDeviceAttributes: HIDD_ATTRIBUTES;
  lHandle: THandle=INVALID_HANDLE_VALUE;
  lMemberIndex: integer;
  lStopEnumeration: Boolean=false;
  lSize: DWORD;
  lUSBDEVHandle: USBDEVHANDLE;
begin
  if not InitializeLibraries then exit(USBHID_ERR_UNKNOWN);
  lGUID:=Default(THIDGUID);
  DLLHIDFunctions.HidD_GetHidGuid(lGUID);
  ldeviceInfoList := DLLSetupAPIFunctions.SetupDiGetClassDevs(lGUID, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if lDeviceInfoList=INVALID_HANDLE_VALUE then begin
    exit(USBHID_ERR_NOTFOUND);
  end;
  lDeviceInfo.cbSize:=SizeOf(lDeviceInfo);
  lMemberIndex:=0;
  while true do begin
    if not DLLSetupAPIFunctions.SetupDiEnumDeviceInterfaces(ldeviceInfoList, nil, lGUID, lMemberIndex, lDeviceInfo) then begin
      // Browse finished
      break;
    end;
    lSize := 0;
    DLLSetupAPIFunctions.SetupDiGetDeviceInterfaceDetailW(ldeviceInfoList, @lDeviceInfo, nil, 0, lSize, nil);
    if lSize>0 then begin
      lDeviceDetails:=GetMem(lSize);
      lDeviceDetails^.cbSize:=sizeof(lDeviceDetails^);
      if DLLSetupAPIFunctions.SetupDiGetDeviceInterfaceDetailW(ldeviceInfoList, @lDeviceInfo, lDeviceDetails, lSize, lSize, nil) then begin
        DEBUG_OUTPUT(format(USBHID_MSG_WIN_HIDPATH,[ansistring(PWideChar(@lDeviceDetails^.DevicePath[0]))]));
        lHandle:=Windows.CreateFileW(@lDeviceDetails^.DevicePath[0],
                  GENERIC_READ or GENERIC_WRITE,
                  FILE_SHARE_READ or FILE_SHARE_WRITE,
                  nil,
                  OPEN_EXISTING,
                  0,
                  HANDLE(0));
        if lHandle=windows.INVALID_HANDLE_VALUE then begin
          DEBUG_OUTPUT(USBHID_ERROR_OPEN_FAIL + ' ('+inttostr(windows.GetLastError())+')');
        end else begin
          lDeviceAttributes.Size := sizeof(lDeviceAttributes);
          if DLLHIDFunctions.HidD_GetAttributes(lHandle, lDeviceAttributes) then begin
            DEBUG_USBIDS(lDeviceAttributes.VendorID,lDeviceAttributes.ProductID,lDeviceAttributes.VersionNumber);
            if (aVendor=lDeviceAttributes.VendorID) and (aProduct=lDeviceAttributes.ProductID) and Assigned(aEnumFunc) then begin
              DEBUG_OUTPUT(USBHID_MSG_CALLING_ENUMERATE);
              new(lUSBDEVHandle);
              lUSBDEVHandle^.TheHandle:=lHandle;
              if aEnumFunc(lUSBDEVHandle,aContext)=0 then begin
                // Stop enumeration
                lStopEnumeration:=true;
              end;
              // Handle is kept by the enumFunc and it must close it.
              lHandle:=INVALID_HANDLE_VALUE;
            end;
          end;
        end;
      end;
    end;
    if Assigned(lDeviceDetails) then begin
      Freemem(lDeviceDetails);
      lDeviceDetails:=nil;
    end;
    if lHandle<>INVALID_HANDLE_VALUE then begin
      Windows.CloseHandle(lHandle);
      lHandle:=INVALID_HANDLE_VALUE;
    end;
    if lStopEnumeration then break;
    inc(lMemberIndex);
  end;
  DLLSetupAPIFunctions.SetupDiDestroyDeviceInfoList(lDeviceInfoList);
  Result:=USBHID_OK;
end;

procedure usbhidCloseDevice(usbh: USBDEVHANDLE);
begin
  Windows.CloseHandle(usbh^.TheHandle);
  Dispose(usbh);
end;

function usbhidGetVendorString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lBuffer: WideString;
begin
  aString:='';
  if not InitializeLibraries then exit(USBHID_ERR_UNKNOWN);
  SetLength(lBuffer,256);
  if not DLLHIDFunctions.HidD_GetManufacturerString(usbh^.TheHandle, @lBuffer[1], ByteLength(lBuffer)) then begin
    DEBUG_OUTPUT (USBHID_ERROR_READING_VENDOR);
    exit (USBHID_ERR_IO_HID);
  end;
  SetLength(lBuffer,StrLen(PWideChar(@lBuffer[1])));
  aString:=string(lBuffer);
  exit (USBHID_OK);
end;

function usbhidGetProductString(usbh: USBDEVHANDLE; out aString: string): integer;
var
  lBuffer: WideString;
begin
  aString:='';
  if not InitializeLibraries then exit(USBHID_ERR_UNKNOWN);
  SetLength(lBuffer,256);
  if not DLLHIDFunctions.HidD_GetProductString(usbh^.TheHandle, @lBuffer[1], ByteLength(lBuffer)) then begin
    DEBUG_OUTPUT (USBHID_ERROR_READING_PRODUCT);
    exit (USBHID_ERR_IO_HID);
  end;
  SetLength(lBuffer,StrLen(PWideChar(@lBuffer[1])));
  aString:=string(lBuffer);
  exit (USBHID_OK);
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
begin
DEBUG_OUTPUT (format(USBHID_MSG_SET_REPORT ,[HexbufToStr(Buffer,Len)]));
  if DLLHIDFunctions.HidD_SetFeature(usbh^.TheHandle, Buffer, len) then begin
    Result:=USBHID_OK;
  end else begin
    Result:=USBHID_ERR_IO_HID;
    DEBUG_OUTPUT (USBHID_ERROR_SET_REPORT);
  end;
end;

function usbhidGetReport(usbh: USBDEVHANDLE; ReportNumber: integer; Buffer: pchar; var len: integer): integer;
begin
  Result:=USBHID_ERR_IO_HID;
  if len>0 then begin
    PBYTE(Buffer)^:=ReportNumber;
    if DLLHIDFunctions.HidD_GetFeature(usbh^.TheHandle,Buffer,len) then begin
    DEBUG_OUTPUT (format(USBHID_MSG_GET_REPORT ,[HexbufToStr(Buffer,Len)]));
      Result:=USBHID_OK;
    end else begin
      DEBUG_OUTPUT (USBHID_ERROR_GET_REPORT);
    end;
  end;
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
  DLLHIDFunctions:=Default(TDLLHIDEntriesRecord);
  DLLSetupAPIFunctions:=Default(TDLLSetupAPIEntriesRecord);
  DLLInitializedFlag:=true;
  DLLHIDHandle:=LoadLibrary(WINDOWS_DLLHID_NAME);
  if DLLHIDHandle<>0 then begin
    Pointer(DLLHIDFunctions.HidD_GetHidGuid):=GetProcAddress(DLLHIDHandle,'HidD_GetHidGuid');
    Pointer(DLLHIDFunctions.HidD_GetAttributes):=GetProcAddress(DLLHIDHandle,'HidD_GetAttributes');
    Pointer(DLLHIDFunctions.HidD_GetManufacturerString):=GetProcAddress(DLLHIDHandle,'HidD_GetManufacturerString');
    Pointer(DLLHIDFunctions.HidD_GetProductString):=GetProcAddress(DLLHIDHandle,'HidD_GetProductString');
    Pointer(DLLHIDFunctions.HidD_GetSerialNumberString):=GetProcAddress(DLLHIDHandle,'HidD_GetSerialNumberString');
    Pointer(DLLHIDFunctions.HidD_GetFeature):=GetProcAddress(DLLHIDHandle,'HidD_GetFeature');
    Pointer(DLLHIDFunctions.HidD_SetFeature):=GetProcAddress(DLLHIDHandle,'HidD_SetFeature');
    Pointer(DLLHIDFunctions.HidD_GetNumInputBuffers):=GetProcAddress(DLLHIDHandle,'HidD_GetNumInputBuffers');
    Pointer(DLLHIDFunctions.HidD_SetNumInputBuffers):=GetProcAddress(DLLHIDHandle,'HidD_SetNumInputBuffers');
  end;
  if lSuccessFunctions then begin
    lSuccessFunctions:=CheckFunctionsRecord(@DLLHIDFunctions,sizeof(DLLHIDFunctions) div sizeof(Pointer));
  end;

  DLLSetupAPIHandle:=LoadLibrary(WINDOWS_SETUPAPI_NAME);
  if DLLHIDHandle<>0 then begin
    Pointer(DLLSetupAPIFunctions.SetupDiGetClassDevs):=GetProcAddress(DLLSetupAPIHandle,'SetupDiGetClassDevsW');
    Pointer(DLLSetupAPIFunctions.SetupDiEnumDeviceInterfaces):=GetProcAddress(DLLSetupAPIHandle,'SetupDiEnumDeviceInterfaces');
    Pointer(DLLSetupAPIFunctions.SetupDiGetDeviceInterfaceDetailW):=GetProcAddress(DLLSetupAPIHandle,'SetupDiGetDeviceInterfaceDetailW');
    Pointer(DLLSetupAPIFunctions.SetupDiDestroyDeviceInfoList):=GetProcAddress(DLLSetupAPIHandle,'SetupDiDestroyDeviceInfoList');
  end;
  if lSuccessFunctions then begin
    lSuccessFunctions:=CheckFunctionsRecord(@DLLSetupAPIFunctions,sizeof(DLLSetupAPIFunctions) div sizeof(Pointer));
  end;
  if not lSuccessFunctions then begin
    DEBUG_OUTPUT(USBHID_ERROR_GETPROCADDRESS);
  end;
  DLLInitializedSuccess:=lSuccessFunctions;
  Result:=DLLInitializedSuccess;
end;

finalization
  if DLLInitializedFlag then begin
    if (DLLHIDHandle<>0) then FreeLibrary(DLLHIDHandle);
    if (DLLSetupAPIHandle<>0) then FreeLibrary(DLLSetupAPIHandle);
  end;

end.

