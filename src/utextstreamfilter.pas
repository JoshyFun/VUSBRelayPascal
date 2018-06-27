unit utextstreamfilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TTextStreamFilter=class(TObject)
  private
    FHost: TStream;
    FBufferOffset,FSize: Integer;
    FBuffer: array[0..4095] of Char;
    FEOF: Boolean;
    FEatNext10: Boolean;
    FFreeHost: Boolean;
    function FillBuffer: Boolean;
  protected
    property Host: TStream read FHost;
  public
    constructor Create(const AHost: TStream; const AFreeHostOnFree: Boolean=true);
    destructor Destroy; override;
    function ReadLn: string; overload;
    function ReadLn(out Data: string): Boolean; overload;
    property EOF: Boolean read FEOF;
  end;

implementation

{ TTextStreamFilter }

constructor TTextStreamFilter.Create(const AHost: TStream; const AFreeHostOnFree: Boolean=true);
begin
  FHost:=AHost;
  FFreeHost:=AFreeHostOnFree;
  FillBuffer;
end;

destructor TTextStreamFilter.Destroy;
begin
  if FFreeHost then FHost.Free;
  inherited;
end;

function TTextStreamFilter.FillBuffer: Boolean;
begin
  FBufferOffset:=0;
  FSize:=FHost.Read(FBuffer,SizeOf(FBuffer));
  Result:=FSize>0;
  FEOF:=not Result;
end;

function TTextStreamFilter.ReadLn(out Data: string): Boolean;
var
  Len, Start: Integer;
  EOLChar: Char;
  FoundEOL: Boolean;
begin
  Data:='';
  Result:=False;
  FoundEOL:=false;
  repeat
    if FBufferOffset>=FSize then begin
      if not FillBuffer then Exit; // no more data to read from stream -> exit
    end;
    if FEatNext10 then begin
      if FBufferOffset<FSize then begin
        if FBuffer[FBufferOffset]=#10 then begin
          inc(FBufferOffset);
        end;
        FEatNext10:=false;
      end;
    end;
    Result:=True;
    Start:=FBufferOffset;
    while (FBufferOffset<FSize) do begin
     if (FBuffer[FBufferOffset] in [#13,#10]) then begin
       FoundEOL:=true;
       break;
     end;
     Inc(FBufferOffset);
    end;
    Len:=FBufferOffset-Start;
    if Len>0 then begin
      SetLength(Data,Length(Data)+Len);
      Move(FBuffer[Start],Data[Succ(Length(Data)-Len)],Len);
    end;
    if FoundEOL then begin
      EOLChar:=FBuffer[FBufferOffset];
      inc(FBufferOffset);
      if EOLChar in [#13,#10] then begin
        if EOLChar=#13 then begin
          FEatNext10:=true;
        end;
        break;
      end;
    end;
  until false;
end;

function TTextStreamFilter.ReadLn: string;
begin
  ReadLn(Result);
end;

end.

