unit uDataItem;

{
 Ollivier Civiol - 2019
 ollivie@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDataTypes,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Utils.Arrays, Utils.Logger;

const
  // I/O image format identifiers.
  FIF_UNKNOWN = -1;
  FIF_BMP     = 0;
  FIF_ICO     = 1;
  FIF_JPEG    = 2;
  FIF_JNG     = 3;
  FIF_WEBP    = 4;
  FIF_PNG     = 5;
  FIF_JP2     = 6;
  FIF_GIF     = 7;

type

  { TIntegerList }

  TIntegerList=class(TList)
  private
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; AValue: Integer);
  public
    function Add(Value: Integer): Integer;
    function IndexOf(AValue : Integer):Integer;
    property Items[Index: Integer]: Integer read GetValue write SetValue; default;
  end;

  TConvertDoneEvent = procedure(Index : Integer) of object;
  //TPutDataEvent = procedure(Index : Integer; Value : TMemoryStream) of object;
  TPutDataEvent = procedure of object;

  TRecType = (rtIn, rtOut);
  { TDataRec }

  TDataRec = Class
    Index,
    ImgType : Integer;
    Indexes : TIntArray;
    Operations : TImgOperations;
    Stream : TMemoryStream;
    DataType : TDataType;
    Filename : String;
    RecType : TRecType;
    constructor Create;
    destructor Destroy; override;
  End;

  { TThreadDataItem }

  TThreadDataItem = Class(TThreadList)
  private
    FLog : ILog;
    function GetCount(aType : TRecType):Integer;
    function GetInCount:Integer;
    function GetOutCount:Integer;
    function Find(Index : Integer; aType : TRecType):TDataRec;
    function FindFirst(aType : TRecType):TDataRec;
  protected
    FEnabled : Boolean;
    //FIndexList : TIntegerList;
    FNextIndex : Integer;
    FWorking : Integer;
    FOnPutData : TPutDataEvent;
    procedure BeginWork;
    procedure EndWork;
  public
    constructor Create(aLog : ILog);
    Destructor Destroy; override;
    procedure ClearLists;
    procedure AddItem2(Value : TMemoryStream;
                      Indexes : TIntArray;
                      Operations : TImgOperations = [opConvert];
                      DataType : TDataType = dtImage;
                      ImgType : Integer = FIF_UNKNOWN;
                      const Filename : String = '');
    procedure AddItem(Value : TMemoryStream; Index : Integer;
                      Indexes : TIntArray;
                      Operations : TImgOperations = [opConvert];
                      DataType : TDataType = dtImage;
                      ImgType : Integer = FIF_UNKNOWN;
                      const Filename : String = '');
    function GetIn:TDataRec;
    function GetOut(Index : Integer; out Value : TDataRec):Boolean;
    procedure Put(Rec : TDataRec);
    function Empty:Boolean;
    procedure Stats(var nbIn, nbOut : Integer);
    procedure Disable;
    property InCount : Integer read GetInCount;
    property Working : Integer read FWorking;
    property OnPutData : TPutDataEvent read FOnPutData write FOnPutData;
  end;



implementation

uses
  uCbz;

{ TDataRec }

constructor TDataRec.Create;
begin
  inherited;
  RecType := rtIn;
  Stream := nil;
end;

destructor TDataRec.Destroy;
begin
  if Assigned(Stream) then
    Stream.Free;
  inherited Destroy;
end;

{ TIntegerList }

function TIntegerList.GetValue(Index: Integer): Integer;
begin
  result := Integer(inherited Items[Index]);
end;

procedure TIntegerList.SetValue(Index: Integer; AValue: Integer);
begin
  inherited Items[Index] := Pointer(AValue);
end;

function TIntegerList.Add(Value: Integer): Integer;
begin
  result := inherited Add(Pointer(Value));
end;

function TIntegerList.IndexOf(AValue: Integer): Integer;
begin
  result := inherited IndexOf(Pointer(AVAlue));
end;

{ TThreadDataItem }

constructor TThreadDataItem.Create(aLog : ILog);
begin
  FLog := aLog;
  FWorking := 0;
  FNextIndex := 0;
  //FIndexList := TIntegerList.Create;
  inherited Create;
end;

destructor TThreadDataItem.Destroy;
begin
  ClearLists;
  FLog := nil;
  //FIndexList.Free;
  inherited;
end;

procedure TThreadDataItem.ClearLists;
var
  i : integer;
begin
  With LockList do
  try
    //FIndexList.Clear;
    try
      for i := 0 to Count - 1 do
        TDataRec(Items[i]).Free;
    except
    end;
    Clear;

    FNextIndex := 0;
    FWorking := 0;
    FEnabled := True;
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.AddItem2(Value: TMemoryStream;
                                  Indexes : TIntArray;
                                  Operations : TImgOperations = [opConvert];
                                  DataType : TDataType = dtImage;
                                  ImgType : Integer = FIF_UNKNOWN;
                                  const Filename : String = '');
begin
  LockList;
  try
    if FEnabled then
    begin
      AddItem(Value, FNextIndex, Indexes, Operations, DataType, ImgType, Filename);
      inc(FNextIndex);
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.AddItem(Value: TMemoryStream; Index : Integer;
                                  Indexes : TIntArray;
                                  Operations : TImgOperations = [opConvert];
                                  DataType : TDataType = dtImage;
                                  ImgType : Integer = FIF_UNKNOWN;
                                  const Filename : String = '');
var
  Rec : TDataRec;
begin
  with LockList do
  try
    if FEnabled  then
    begin
      Rec := TDataRec.Create;
      Rec.Index := Index;
      Rec.Indexes := Indexes;
      Rec.Operations := Operations;
      Rec.Stream := Value;
      Rec.DataType := DataType;
      if (ImgType = FIF_UNKNOWN) and (DataType = dtImage) then
        Rec.ImgType := TCbz.GetImageType(Rec.Stream, FileName)
      else
        Rec.ImgType := ImgType;
      Rec.Filename := Filename;
      //FIndexList.Add(Index);
      if Assigned(Value) then
        Value.Position := 0;
      Add(Rec);
      FLog.Log(Format('Created record with image index : %d', [Rec.Index]));
    end;
  finally
    UnlockList;
  end;
end;

function TThreadDataItem.Find(Index: Integer; aType : TRecType): TDataRec;
var
  i : integer;
begin
  with LockList do
  try
    for i:=0 to Count - 1 do
      if (TDataRec(Items[i]).RecType = aType) and
         (TDataRec(Items[i]).Index = Index) then
      begin
        result := TDataRec(Items[i]);
        Remove(Items[i]);
        Exit;
      end;
    result := nil;
  finally
    UnLockList;
  end;
end;

function TThreadDataItem.FindFirst(aType : TRecType):TDataRec;
var
  i : integer;
begin
  Result := nil;
  with LockList do
  try
    for i:=0 to Count - 1 do
      if (TDataRec(Items[i]).RecType = aType) then
      begin
        result := TDataRec(Items[i]);
        Remove(Items[i]);
        Exit;
      end;
  finally
    UnlockList;
  end;
end;

function TThreadDataItem.GetIn:TDataRec;
begin
  with LockList do
  try
    result := nil;
    if FEnabled then
    begin
      Result := FindFirst(rtIn);
      if Assigned(Result) then
        FLog.Log(Format('Got IN record with image index : %d', [Result.Index]));
    end;

    {
    if (FIndexList.Count > 0) and FEnabled then
    begin
      index := $FFFFFFF;
      v := 0;
      for i := 0 to FIndexList.Count - 1 do
        if (FIndexList[i] < index) then
        begin
          index := FIndexList[i];
          v := i;
        end;

      FIndexList.Delete(v);
      ind := Find(Index, rtIn);
      Result := TDataRec(Items[ind]);
      Remove(Result);
      BeginWork;
    end;
    }
  finally
    UnlockList;
  end;
end;

function TThreadDataItem.GetCount(aType : TRecType):Integer;
var
  i : integer;
begin
  with LockList do
  try
    result := 0;
    for i:=0 to Count - 1 do
      if (TDataRec(Items[i]).RecType = aType) then
        inc(result);
  finally
    UnLockList;
  end;
end;

function TThreadDataItem.GetInCount: Integer;
begin
  result := GetCount(rtIn);
end;

function TThreadDataItem.GetOutCount: Integer;
begin
  result := GetCount(rtOut);
end;

procedure TThreadDataItem.Put(Rec : TDataRec);
begin
  with LockList do;
  try
    if FEnabled then
    begin
      Rec.RecType := rtOut;
      Add(Rec);
      FLog.Log(Format('Put record with image index : %d', [Rec.Index]));
      if Assigned(FOnPutData) then
        FOnPutData;
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.Stats(var nbIn, nbOut: Integer);
begin
  nbIn := GetInCount;
  nbOut := GetOutCount;
end;

function TThreadDataItem.GetOut(Index: Integer; out Value : TDataRec):Boolean;
begin
  With LockList do
  try
    FLog.Log(Format('Asking OUT record with image index : %d', [Index]));
    Value := Find(Index, rtOut);
    result := Assigned(Value);
    if result then
    begin
      EndWork;
      FLog.Log(Format('Found OUT record with image index : %d', [Value.Index]));
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.BeginWork;
begin
  LockList;
  try
    Inc(FWorking);
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.EndWork;
begin
  LockList;
  try
    if FWorking > 0 then
      Dec(FWorking);
  finally
    UnlockList;
  end;
end;

function TThreadDataItem.Empty: Boolean;
begin
  LockList;
  try
    result := (FWorking = 0) and ((GetInCount = 0) and (GetOutCount = 0));
  finally
    UnlockList;
  end;
end;

procedure TThreadDataItem.Disable;
begin
  LockList;
  try
    FEnabled := False;
  finally
    UnlockList;
  end;
end;


end.

