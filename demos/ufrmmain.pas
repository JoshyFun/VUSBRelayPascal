unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, uusbrelaylib;

type

  TClickProcedure=procedure(const aIndex: integer; const aNewState: Boolean) of object;

  { TRelayVisual }

  TRelayVisual=class
  private
  protected
    FPanelsList: TFPList;
    FLabels: TFPList;
    FOnClickProcedure: TClickProcedure;
    procedure ClickON(Sender: Tobject);
    procedure ClickOFF(Sender: Tobject);
  public
    constructor Create(const aNumberOfRelays: integer; const aParentControl: TWinControl; aClickProcedure: TClickProcedure);
    destructor Destroy; override;
    procedure SetState(const aIndex: integer; const aOn: Boolean);
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    Panel1: TPanel;
    cbDeviceID: TComboBox;
    Panel2: TPanel;
    imgRelay: TImage;
    GroupBox1: TGroupBox;
    imgRelay1: TImage;
    imgRelay2: TImage;
    imgRelay4: TImage;
    imgRelay8: TImage;
    butScan: TButton;
    lblRelays: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure cbDeviceIDChange(Sender: TObject);
    procedure butScanClick(Sender: TObject);
  private
    FUSBRelayManager: TUSBRelays;
    FRelayVisual: TRelayVisual;
    procedure ClickedRelay(const aIndex: integer; const aNewState: Boolean);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TRelayVisual }

procedure TRelayVisual.ClickON(Sender: Tobject);
begin
  FOnClickProcedure(TButton(Sender).Tag,true);
end;

procedure TRelayVisual.ClickOFF(Sender: Tobject);
begin
  FOnClickProcedure(TButton(Sender).Tag,false);
end;

constructor TRelayVisual.Create(const aNumberOfRelays: integer; const aParentControl: TWinControl; aClickProcedure: TClickProcedure);
var
  j: Integer;
  lPanel: TPanel=nil;
  lButton: TButton;
  lLabelLight: TLabel;
begin
  FOnClickProcedure:=aClickProcedure;
  FPanelsList:=TFPList.Create;
  FLabels:=TFPList.Create;
  for j := 0 to Pred(aNumberOfRelays) do begin
    lPanel:=TPanel.Create(aParentControl);
    lPanel.Top:=j+10;
    lPanel.Caption:='';
    lPanel.Align:=alTop;
    lPanel.AutoSize:=true;
    lPanel.BorderSpacing.Around:=3;
    lPanel.Parent:=aParentControl;
    lPanel.ChildSizing.ControlsPerLine:=3;
    lPanel.ChildSizing.HorizontalSpacing:=10;
    lPanel.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    lPanel.ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
    lPanel.ChildSizing.EnlargeVertical:=crsHomogenousChildResize;
    lButton:=TButton.Create(lPanel);
    lButton.Caption:='ON';
    lButton.OnClick:=@ClickON;
    lButton.Tag:=j;
    lButton.BorderSpacing.Around:=2;
    lButton.Parent:=lPanel;
    lButton:=TButton.Create(lPanel);
    lButton.Caption:='OFF';
    lButton.OnClick:=@ClickOFF;
    lButton.Tag:=j;
    lButton.BorderSpacing.Around:=2;
    lButton.Parent:=lPanel;
    lLabelLight:=TLabel.Create(lPanel);
    lLabelLight.Alignment:=taCenter;
    lLabelLight.Layout:=tlCenter;
    lLabelLight.Caption:=Inttostr(j);
    lLabelLight.AutoSize:=false;
    lLabelLight.BorderSpacing.Around:=2;
    lLabelLight.Parent:=lPanel;
    FLabels.Add(lLabelLight);
    FPanelsList.Add(lPanel);
  end;
end;

destructor TRelayVisual.Destroy;
var
  j: Integer;
begin
  for j := 0 to Pred(FPanelsList.Count) do begin
    TObject(FPanelsList[j]).Free;
  end;
  FreeAndNil(FPanelsList);
  FreeAndNil(FLabels);
  inherited Destroy;
end;

procedure TRelayVisual.SetState(const aIndex: integer; const aOn: Boolean);
var
  lLabel: TLabel;
begin
  lLabel:=TLabel(FLabels[aIndex]);
  if aOn then begin
    lLabel.Color:=clLime;
  end else begin
    lLabel.Color:=clRed;
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRelayVisual);
  FreeAndNil(FUSBRelayManager);
end;

procedure TfrmMain.cbDeviceIDChange(Sender: TObject);
var
  FDevice: TUSBRelayDevice;
  lIndex, j: integer;
begin
  lIndex:=TComboBox(Sender).ItemIndex;
  if lIndex<0 then begin
    FDevice:=nil;
  end else begin
    FDevice:=TUSBRelayDevice(TComboBox(Sender).items.Objects[lIndex]);
  end;
  FreeAndNil(FRelayVisual);
  if Assigned(FDevice) then begin
    FRelayVisual:=TRelayVisual.Create(FDevice.RelaysCount,GroupBox1,@ClickedRelay);
    for j := 0 to Pred(FDevice.RelaysCount) do begin
      FRelayVisual.SetState(j,FDevice.Relay[j].State);
    end;
    Case FDevice.RelaysCount of
      1: imgRelay.Picture:=imgRelay1.Picture;
      2: imgRelay.Picture:=imgRelay2.Picture;
      4: imgRelay.Picture:=imgRelay4.Picture;
      8: imgRelay.Picture:=imgRelay8.Picture;
      otherwise
      imgRelay.Picture:=nil;
    end;
  end else begin
    imgRelay.Picture:=nil;
  end;
end;

procedure TfrmMain.butScanClick(Sender: TObject);
var
  j: Integer;
begin
  cbDeviceID.Clear;
  FreeAndNil(FUSBRelayManager);
  FreeAndNil(FRelayVisual);
  imgRelay.Picture:=nil;
  FUSBRelayManager:=TUSBRelays.Create;
  FUSBRelayManager.UseUSBIdentifiers('usb.ids');
  lblRelays.Caption:=inttostr(FUSBRelayManager.Scan);
  for j := 0 to Pred(FUSBRelayManager.DevicesCount) do begin
    cbDeviceID.Items.AddObject(
                      format('%s (%s)',[FUSBRelayManager.Device[j].UniqueID,FUSBRelayManager.Device[j].Manufacturer])
                      ,FUSBRelayManager.Device[j]);
  end;
end;

procedure TfrmMain.ClickedRelay(const aIndex: integer; const aNewState: Boolean);
var
  FDevice: TUSBRelayDevice;
  lIndex, j: integer;
begin
  lIndex:=cbDeviceID.ItemIndex;
  if lIndex<0 then begin
    FDevice:=nil;
  end else begin
    FDevice:=TUSBRelayDevice(cbDeviceID.items.Objects[lIndex]);
  end;
  if Assigned(FDevice) then begin
    FDevice.Relay[aIndex].State:=aNewState;
    for j := 0 to Pred(FDevice.RelaysCount) do begin
      FRelayVisual.SetState(j,FDevice.Relay[j].State);
    end;
  end;
end;

end.

