program vusbrelaycli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  cwstring,
  {$ENDIF}
  Classes, SysUtils, CustApp, uusbrelaylib
  { you can add units after this };

type

  { TVUSBDemo }

  TVUSBDemo = class(TCustomApplication)
  protected
    FRelayManager: TUSBRelays;
    procedure DoRun; override;
    procedure BaseCopyright;
    procedure UseUSBIds;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Scan;
  end;

{ TVUSBDemo }

procedure TVUSBDemo.DoRun;
var
  ErrorMsg: String;
  lDevice: string;
  lIndex: integer;
  lState: string;
  lStateV, j: integer;
  lUseDevice: TUSBRelayDevice=nil;
begin
  try
    // quick check parameters
    ErrorMsg:=CheckOptions('hsdipv', 'help;scan;device;index;power;verbose');
    if ErrorMsg<>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('s', 'scan') then begin
      Scan;
      Terminate;
      Exit;
    end;

    lDevice:=GetOptionValue('d','device');

    if lDevice='' then begin
      WriteHelp;
      Terminate(1);
      Exit;
    end;

    lIndex:=StrToIntDef(GetOptionValue('i','index'),-1);
    lState:=GetOptionValue('p','power');
    case LowerCase(lState) of
      'on': lStateV:=1;
      'off': lStateV:=0;
      'check': lStateV:=-1;
      '': lStateV:=-1;
      otherwise begin
        BaseCopyright;
        writeln('Error: Unrecognized power value ("',lState,'")');
        Terminate(1);
        exit;
      end;
    end;

    FRelayManager:=TUSBRelays.Create;
    if HasOption('v','verbose') then begin
      FRelayManager.Quiet:=false;
      UseUSBIds;
    end;
    FRelayManager.Scan;
    for j := 0 to Pred(FRelayManager.DevicesCount) do begin
      if FRelayManager.Device[j].UniqueID=lDevice then begin
        lUseDevice:=FRelayManager.Device[j];
        break;
      end;
    end;

    if not Assigned(lUseDevice) then begin
      BaseCopyright;
      writeln('Error: Device not found ("',lDevice,'")');
      Terminate(2);
      exit;
    end;

    if (lIndex>=lUseDevice.RelaysCount) or (lIndex<-1) then begin
      BaseCopyright;
      writeln('Error: Relay index out of range ("',lIndex,'")');
      Terminate(3);
      exit;
    end;

    if lIndex=-1 then begin
      for j := 0 to Pred(lUseDevice.RelaysCount) do begin
        case lStateV of
         -1: write(lUseDevice.Relay[j].State,' ');
          1: lUseDevice.Relay[j].State:=true;
          0: lUseDevice.Relay[j].State:=false;
        end;
      end;
      Terminate(0);
      exit;
    end else begin
      case lStateV of
       -1: write(lUseDevice.Relay[lIndex].State,' ');
        1: lUseDevice.Relay[lIndex].State:=true;
        0: lUseDevice.Relay[lIndex].State:=false;
      end;
      Terminate(0);
      exit;
    end;

    WriteHelp;

    // stop program loop
    Terminate;
  finally
    FreeAndNil(FRelayManager);
  end;
end;

procedure TVUSBDemo.BaseCopyright;
begin
  writeln(UTF8ToString(string('vusbrelaycli (c) 2018 José Mejuto - LGPL')));
  writeln;
end;

procedure TVUSBDemo.UseUSBIds;
begin
  if FileExists('usb.ids') then begin
    FRelayManager.UseUSBIdentifiers('usb.ids');
  end else begin
    if FileExists(ConcatPaths([ExtractFilePath(ExeName),'usb.ids'])) then begin
      FRelayManager.UseUSBIdentifiers(ConcatPaths([ExtractFilePath(ExeName),'usb.ids']));
    end;
  end;
end;

constructor TVUSBDemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TVUSBDemo.Destroy;
begin
  inherited Destroy;
end;

procedure TVUSBDemo.WriteHelp;
var
  lExeName: string;
begin
  lExeName:=ExtractFileName(ExeName);
  BaseCopyright;
  writeln(lExeName, ' [options] -d RelayUniqueID -r RelayIndex -p on/off/check');
  writeln;
  writeln('Options:');
  writeln(' -h --help      Show this help.');
  writeln(' -s --scan      Show available devices and number of relays.');
  writeln(' -d --device    UniqueID of the relay board.');
  writeln(' -i --index     Address the relay index in the board (0-7).');
  writeln(' -p --power     Power the relay index "on"/"off" or "check" state.');
  writeln(' -v --verbose   Show information about USB bus scan.');
  writeln;
  writeln('Samples:');
  writeln(' ',lExeName,' BIFTF 0 on  -> Activate relay index 0 in device "BIFTF"');
  writeln(' ',lExeName,' ADSGH 1 off -> Deactivate relay index 1 in device "ADSGH"');
  writeln;
  writeln('Return codes:');
  writeln(' 0 = Success. Other value error.');
  writeln;
  writeln('Note:');
  writeln(' If "usb.ids" file is in current path, or executable path it will be used');
  writeln(' in the scan process to show information about vendor and product name');
  writeln(' of devices attached to the USB bus.');
end;

procedure TVUSBDemo.Scan;
var
  lDevice: TUSBRelayDevice;
  j, k: Integer;
  lIndexes: string;
begin
  BaseCopyright;
  FRelayManager:=TUSBRelays.Create;
  if HasOption('v','verbose') then begin
    FRelayManager.Quiet:=false;
    UseUSBIds;
  end;
  try
    FRelayManager.Scan;
    if not FRelayManager.Quiet then writeln; // insert a blank line.
    writeln('DeviceID Relay Indexes');
    writeln('-------- -----------------');

    for j := 0 to Pred(FRelayManager.DevicesCount) do begin
      lDevice:=FRelayManager.Device[j];
      lIndexes:='';
      for k := 0 to Pred(lDevice.RelaysCount) do begin
        lIndexes:=lIndexes+' '+inttostr(k);
      end;
      writeln(lDevice.UniqueID,'   ',lIndexes);
    end;
  finally
    FreeAndNil(FRelayManager);
  end;
end;

var
  Application: TVUSBDemo;
begin
  Application:=TVUSBDemo.Create(nil);
  Application.Title:='VUSB Demo';
  Application.Run;
  Application.Free;
end.

