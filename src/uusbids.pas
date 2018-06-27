unit uusbids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utextstreamfilter;

type

  TUSBIDEntry=packed record
    VendorID: WORD;
    ProductID: WORD;
    VendorName: string;
    ProductName: string;
  end;
  PUSBIDEntry=^TUSBIDEntry;

  { TUSBIDS }

  TUSBIDS=class
  private
  protected
    FUSBIDArray: Array of TUSBIDEntry;
    FStreamFilter: TTextStreamFilter;
  public
    function LoadFromStream(const aStream: TStream): Boolean;
    function Find(const aVendorID, aProductID: WORD; out aVendorName,aProductName: string): Boolean;
  end;

implementation

{ TUSBIDS }

function TUSBIDS.LoadFromStream(const aStream: TStream): Boolean;
const
  GROW_BY=1000;
var
  lLine: string;
  lLimit: integer;
  lIndex: integer;
  lVendorID: WORD;
  lVendorName: string;
  lProductID: WORD;
  lProductName: string;
  lCode: integer;
  function IsVendor(const aString: string): Boolean;
  var
    j: Integer;
  begin
    Result:=true;
    for j := 1 to 4 do begin
      if not (aString[j] in ['0'..'9','A'..'F','a'..'f']) then begin
        Result:=false;
        break;
      end;
    end;
  end;
  function IsProduct(const aString: string): Boolean;
  var
    j: Integer;
  begin
    Result:=true;
    if aString[1]<>#9 then begin
      exit(false);
    end;
    for j := 2 to 5 do begin
      if not (aString[j] in ['0'..'9','A'..'F','a'..'f']) then begin
        Result:=false;
        break;
      end;
    end;
  end;
  procedure AddProduct;
  begin
    if lIndex=lLimit then begin
      inc(lLimit,GROW_BY);
      SetLength(FUSBIDArray,lLimit);
    end;
    with FUSBIDArray[lIndex] do begin
      VendorID:=lVendorID;
      VendorName:=lVendorName;
      ProductID:=lProductID;
      ProductName:=lProductName;
    end;
    inc(lIndex);
  end;

begin
  Result:=false;
  try
    lLimit:=0;
    lIndex:=0;
    SetLength(FUSBIDArray,lLimit);
    FStreamFilter:=TTextStreamFilter.Create(aStream,false);
    try
      while FStreamFilter.ReadLn(lLine) do begin
        if Length(lLine)>4 then begin
          if IsVendor(lLine) then begin
            Val('$'+copy(lLine,1,4),lVendorID,lCode);
            lVendorName:=copy(lLine,7);
          end else if (lVendorID<>0) and (IsProduct(lLine)) then begin
            Val('$'+copy(lLine,2,4),lProductID,lCode);;
            lProductName:=copy(lLine,8);
            AddProduct;
          end else begin
            lVendorID:=0;
            lVendorName:='';
          end;
        end;
      end;
      Result:=true;
    finally
      SetLength(FUSBIDArray,lIndex);
      FreeAndNil(FStreamFilter);
    end;
  except
    // Eat exception
  end;
end;

function TUSBIDS.Find(const aVendorID, aProductID: WORD; out aVendorName, aProductName: string): Boolean;
var
  j: Integer;
begin
  Result:=false;
  aVendorName:='';
  aProductName:='';
  for j := Low(FUSBIDArray) to High(FUSBIDArray) do begin
    with FUSBIDArray[j] do begin
      if (VendorID=aVendorID) then begin
        aVendorName:=VendorName;
        if (ProductID=aProductID) then begin
          aProductName:=ProductName;
          Result:=true;
          break;
        end;
      end;
    end;
  end;
end;

end.

