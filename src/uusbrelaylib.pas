unit uusbrelaylib;

(*

 --------------------------------------
  USBRelayLIB
 --------------------------------------

 This library simplified the handling of V-USB [1] relays identified
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

*)

{$mode objfpc}{$H+}

{.$DEFINE  USBHID_USE_LIBUSB01}
{.$DEFINE  USBHID_USE_LIBUSB10}
{.$DEFINE  USBHID_USE_WINDOWS}

{$IFDEF UNIX}
  {$IFNDEF USBHID_USE_LIBUSB01}
    {$DEFINE USBHID_USE_LIBUSB10}
  {$ENDIF}
{$ELSE}
  {$IFDEF WINDOWS}
    {$IFNDEF USBHID_USE_LIBUSB10}
      {$DEFINE USBHID_USE_WINDOWS}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils
  , uvusbrelaybase
  , uusbids
  {$IFDEF USBHID_USE_LIBUSB10}
  , hid_libusb10mini
  {$ELSE}
    {$IFDEF USBHID_USE_LIBUSB01}
      , hid_libusb
    {$ELSE}
      {$IFDEF USBHID_USE_WINDOWS}
      , hid_windows
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  ;

type
  TUSBRelayDevice=class;

  { TUSBRelay }

  TUSBRelay=class
  private
    function GetState: Boolean;
    procedure SetState(aValue: Boolean);
  protected
    FUSBDevice: TUSBRelayDevice;
    FRelayIndex: integer;
  public
    constructor Create(const aRelayDevice: TUSBRelayDevice; const aRelayIndex: integer);
    property State: Boolean read GetState write SetState;
  end;

  { TUSBRelayDevice }

  TUSBRelayDevice=class
  private
    function GetRelaysCount: integer;
    function GetRelay(Index: integer): TUSBRelay;
    function GetUniqueID: string;
    function GetManufacturer: string;
    function GetRelaysStateBitMasked: integer;
  protected
    FHandle: USBDEVHANDLE;
    FUniqueID: string;
    FRelays: TFPList;
    procedure AddRelay(const aIndex: integer);
  public
    constructor Create(const aUSBDeviceHandle: USBDEVHANDLE);
    destructor Destroy; override;
    property RelaysCount: integer read GetRelaysCount;
    property Relay[Index: integer]: TUSBRelay read GetRelay;
    property UniqueID: string read GetUniqueID;
    property Manufacturer: string read GetManufacturer;
    property RelaysStateBitMasked: integer read GetRelaysStateBitMasked;
  end;

  { TUSBRelays }

  TUSBRelays=class
  private
    function GetUSBRelayDevice(Index: integer): TUSBRelayDevice;
    function GetDevicesCount: integer;
    procedure USBIDs(const aVendorID: WORD; const aProductID: WORD; out aVendorName: string; out aProductName: string);
  protected
    FQuiet: Boolean;
    FUSBIDs: TUSBIDS;
    FUSBRelaysDevices: TFPList;
    function EnumHID(const aUSBDevHandle: USBDEVHANDLE; const aContext: Pointer): integer;
    procedure AddDevice(const aUSBDevHandle: USBDEVHANDLE);
  public
    constructor Create;
    destructor Destroy; override;
    // This is a stream to a TFileStream, TStreamMemory, etc... which
    // hold USB identifiers in format usb.ids, like the one taken from
    // http://www.linux-usb.org/usb.ids
    procedure UseUSBIdentifiers(const aUSBIdentifiers: TStream); overload;
    procedure UseUSBIdentifiers(const aUSBIdentifiersFile: string); overload;
    function Scan: integer;
    property DevicesCount: integer read GetDevicesCount;
    property Device[Index: integer]: TUSBRelayDevice read GetUSBRelayDevice;
    property Quiet: Boolean read FQuiet write FQuiet;
  end;

implementation

{$PUSH}{$HINTS OFF}
procedure ClearMemory(out x;count:SizeInt); inline;
begin
  FillByte(x,count,0);
end;
{$POP}

function RelayReadStatusRAW(aDevice: USBDEVHANDLE; raw_data: Pointer): integer;
var
  lBuffer: array [0..9] of BYTE;
  lErrorCode: integer;
  lReportNum: integer = 0;
  lBufferLength: integer = 9; // report id 1 byte + 8 bytes data */
begin
  ClearMemory(lBuffer,sizeof(lBuffer));
  lErrorCode:=usbhidGetReport(aDevice,lReportNum,@lBuffer[0],lBufferLength);
  if lErrorCode<>USBHID_OK then begin
    exit(-1);
  end;
  if (lBufferLength<>9) and (lBuffer[0]<>lReportNum) then begin
    exit(-2);
  end;
  if Assigned(raw_data) then begin
    move(lBuffer[0],raw_data^,lBufferLength);
  end;
  Result:=lBuffer[8];
end;

function Relay_OnOff(aDevice: USBDEVHANDLE; aIsON: Boolean; aRelayIndex: integer): integer;
var
  lBuffer: array [0..10-1] of BYTE;
  lErrorCode: integer = -1;
  lCommand1, lCommand2, lMask, lMaskValue: BYTE;
begin
  ClearMemory(lBuffer,sizeof(lBuffer));
  if (aRelayIndex < 0) and  (-aRelayIndex <= 8) then begin
    lMask := $FF;
    lCommand2 := 0;
    if aIsON then begin
      lCommand1 := $FE;
      lMaskValue := BYTE (((1 shl (-aRelayIndex)) - 1));
    end else begin
      lCommand1 := $FC;
      lMaskValue := 0;
    end;
  end else begin
    if (aRelayIndex <= 0) or  (aRelayIndex > 8) then begin
      exit (1);
    end;
    lMask := BYTE ((1 shl (aRelayIndex - 1)));
    lCommand2 := BYTE (aRelayIndex);
    if aIsON then begin
      lCommand1 := $FF;
      lMaskValue := lMask;
    end else begin
      lCommand1 := $FD;
      lMaskValue := 0;
    end;
  end;

  lBuffer[0] := 0; //* report # */
  lBuffer[1] := lCommand1;
  lBuffer[2] := lCommand2;
  if usbhidSetReport(aDevice, @lBuffer[0], 9) <> USBHID_OK then begin
    exit(2);
  end;

  lErrorCode := RelayReadStatusRAW(aDevice, nil);
  if lErrorCode<0 then begin
    exit(3);
  end;
  lErrorCode := lErrorCode and lMask;
  if lErrorCode <> lMaskValue then begin
    exit(4);
  end;
  Result:=USBHID_OK;
end;

{ TUSBRelay }

function TUSBRelay.GetState: Boolean;
var
  lRelayStateMask: integer;
begin
  lRelayStateMask:=RelayReadStatusRAW(FUSBDevice.FHandle,nil);
  lRelayStateMask:=lRelayStateMask shr FRelayIndex;
  Result:=(lRelayStateMask and $01)<>0;
end;

procedure TUSBRelay.SetState(aValue: Boolean);
begin
  Relay_OnOff(FUSBDevice.FHandle,aValue,FRelayIndex+1);
end;

constructor TUSBRelay.Create(const aRelayDevice: TUSBRelayDevice; const aRelayIndex: integer);
begin
  FUSBDevice:=aRelayDevice;
  FRelayIndex:=aRelayIndex;
end;

{ TUSBRelayDevice }

function TUSBRelayDevice.GetRelaysCount: integer;
begin
  Result:=FRelays.Count;
end;

function TUSBRelayDevice.GetRelay(Index: integer): TUSBRelay;
begin
  Result:=TUSBRelay(FRelays.items[Index]);
end;

function TUSBRelayDevice.GetUniqueID: string;
var
  lErrorCode: integer;
  lBuffer: array [0..(128*2)-1] of BYTE;
begin
  if FUniqueID='' then begin
    ClearMemory(lBuffer,sizeof(lBuffer));
    lErrorCode:=RelayReadStatusRAW(FHandle,@lBuffer[0]);
    if lErrorCode<0 then begin
      FUniqueID:='';
    end else begin
      SetLength(FUniqueID,5);
      move(lBuffer[1],FUniqueID[1],5);
    end;
  end;
  Result:=FUniqueID;
end;

function TUSBRelayDevice.GetManufacturer: string;
var
  lManufacturerString: string;
begin
  if usbhidGetVendorString(FHandle,lManufacturerString)=USBHID_OK then begin
    Result:=lManufacturerString;
  end else begin
    Result:='';
  end;
end;

function TUSBRelayDevice.GetRelaysStateBitMasked: integer;
begin
  Result:=RelayReadStatusRAW(FHandle,nil);
end;

procedure TUSBRelayDevice.AddRelay(const aIndex: integer);
begin
  FRelays.Add(TUSBRelay.Create(Self,aIndex));
end;

constructor TUSBRelayDevice.Create(const aUSBDeviceHandle: USBDEVHANDLE);
var
  lProductString: string;
  lRelays: integer=0;
  j: Integer;
begin
  FRelays:=TFPList.Create;
  FHandle:=aUSBDeviceHandle;
  usbhidGetProductString(aUSBDeviceHandle,lProductString);
  for j := Length(lProductString) downto 1 do begin
    if not (lProductString[j] in ['0'..'9']) then begin
      lRelays:=StrToIntDef(copy(lProductString,j+1),0);
      break;
    end;
  end;
  for j := 0 to Pred(lRelays) do begin
    AddRelay(j);
  end;
end;

destructor TUSBRelayDevice.Destroy;
var
  j: integer;
begin
  for j := 0 to Pred(FRelays.Count) do begin
    TUSBRelay(FRelays.Items[j]).Free;
  end;
  FreeAndNil(FRelays);
  inherited Destroy;
end;

{ TUSBRelays }

function TUSBRelays.GetUSBRelayDevice(Index: integer): TUSBRelayDevice;
begin
  Result:=TUSBRelayDevice(FUSBRelaysDevices.Items[Index]);
end;

function TUSBRelays.GetDevicesCount: integer;
begin
  Result:=FUSBRelaysDevices.Count;
end;

procedure TUSBRelays.USBIDs(const aVendorID: WORD; const aProductID: WORD; out aVendorName: string; out aProductName: string);
begin
  FUSBIDs.Find(aVendorID,aProductID,aVendorName,aProductName);
end;

function TUSBRelays.EnumHID(const aUSBDevHandle: USBDEVHANDLE; const aContext: Pointer): integer;
begin
  AddDevice(aUSBDevHandle);
  Result:=1; // Continue
end;

procedure TUSBRelays.AddDevice(const aUSBDevHandle: USBDEVHANDLE);
var
  lDevice: TUSBRelayDevice;
begin
  lDevice:=TUSBRelayDevice.Create(aUSBDevHandle);
  if lDevice.Manufacturer=USB_RELAY_VENDOR_NAME then begin
    // Only devices with "www.dcttech.com" as manufacturer will be added.
    FUSBRelaysDevices.Add(lDevice);
  end else begin
    lDevice.Free;
  end;
end;

constructor TUSBRelays.Create;
begin
  FUSBRelaysDevices:=TFPList.Create;
  FQuiet:=true;
end;

destructor TUSBRelays.Destroy;
var
  j: integer;
  lDevice: TUSBRelayDevice;
begin
  for j := 0 to Pred(FUSBRelaysDevices.Count) do begin
    lDevice:=TUSBRelayDevice(FUSBRelaysDevices.Items[j]);
    usbhidCloseDevice(lDevice.FHandle);
    lDevice.Free;
  end;
  FreeAndNil(FUSBRelaysDevices);
  FreeAndNil(FUSBIDs);
  inherited Destroy;
end;

procedure TUSBRelays.UseUSBIdentifiers(const aUSBIdentifiers: TStream);
begin
  FreeAndNil(FUSBIDs);
  FUSBIDs:=TUSBIDS.Create;
  FUSBIDs.LoadFromStream(aUSBIdentifiers);
end;

procedure TUSBRelays.UseUSBIdentifiers(const aUSBIdentifiersFile: string);
var
  F: TFileStream=nil;
begin
  try
    if not FileExists(aUSBIdentifiersFile) then exit;
    F:=TFileStream.Create(aUSBIdentifiersFile,fmOpenRead or fmShareDenyWrite);
    UseUSBIdentifiers(F);
  finally
    FreeAndNil(F);
  end;
end;

function TUSBRelays.Scan: integer;
var
  lOldGUSBDeviceInfo: TUSBDeviceIDCallback;
begin
  lOldGUSBDeviceInfo:=gUSBDeviceIDInfo;
  gUSBHID_Quiet:=FQuiet;
  try
    if Assigned(FUSBIDs) then begin
      gUSBDeviceIDInfo:=@USBIDs;
    end;
    usbhidEnumDevices(USB_CFG_VENDOR_ID,USB_CFG_DEVICE_ID,nil,@EnumHID);
    Result:=FUSBRelaysDevices.Count;
  finally
    gUSBDeviceIDInfo:=lOldGUSBDeviceInfo;
  end;
end;

end.

