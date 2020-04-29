unit uLibraryClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  utils.Logger, uxmldoc, Utils.Strings;

type
  TDisplayFilter = (dfUnread, dfAll);
  TDisplayFilters = set of TDisplayFilter;

  { TFileItem }

  TFileItem = Class
  private
    FFilename : String;
    FImg : TBitmap;
    FLog : ILog;
    FText: String;
    FReadState : Boolean;
    FGuid : TGUID;
    FModified : Boolean;
    FLock : TThreadList;
    FDateAdded : TDateTime;
    FStampGenerated : Boolean;

    function GetCacheFilename: String; inline;
    function GetDateAdded: TDAteTime;
    function GetFilename: String;
    function GetImg: TBitmap;
    function GetModified: Boolean;
    function GetReadState: Boolean;
    function GetStampGenerated: Boolean;
    function GetText: String;
    procedure SetDateAdded(AValue: TDAteTime);
    procedure SetReadState(AValue: Boolean);
    procedure SetStampGenerated(AValue: Boolean);
    procedure SetText(const AValue: String);
  protected
    procedure SaveToXml(aNode : TXmlElement);
    procedure LoadFromXml(aNode : TXmlElement);
    property CacheFilename : String read GetCacheFilename;
    property Modified : Boolean read GetModified;
    property StampGenerated : Boolean read GetStampGenerated write SetStampGenerated;
  public
    constructor Create;
    constructor Create(aLog : ILog; const aFilename : String);
    destructor Destroy; override;

    function GenerateStamp:TBitmap;
    property Filename : String read GetFilename;
    property ReadState : Boolean read GetReadState write SetReadState;
    property Img:TBitmap read GetImg;
    property Text : String Read GetText write SetText;
    property DateAdded : TDAteTime read GetDateAdded write SetDateAdded;
  end;

  { TItemList }

  TItemList = Class(TThreadStringList)
  private
    Flog : ILog;
    FRootPath : String;
    FModified : Boolean;

    function GetDeletedCount: Integer;
    function GetModified: Boolean;
    function GetRootPath: String;
    function GetStampLessCount: Integer;
    procedure SetRootPath(AValue: String);
    function GetReadCount:integer;
  protected
  public
    constructor Create(alog : ILog);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(index:integer); override;
    procedure LoadFromFile(const aFilename : String); override;
    procedure SaveToFile(const aFilename : String); override;
    procedure ResetStampState;
    property Modified : Boolean read GetModified;
    property RootPath : String read GetRootPath write SetRootPath;
    property StampLessCount : Integer read GetStampLessCount;
    property DeletedCount : Integer read GetDeletedCount;
    property ReadCount : Integer read GetReadCount;
  end;


function GetFirstPath(const aPath : String; Lvl : integer = 2):String;
function GetLastPath(const aPath : String):string;

implementation

uses
  Forms, uCbz, Utils.Zipfile;

const
  CS_StampWidth = 120;
  CS_StampHeight = 160;


function GetFirstPath(const aPath : String; Lvl : integer = 2):String;
var
  a : TStringArray;
  i : integer;
begin
  a := aPath.Split([PathDelim]);
  result := '';
  for i:=0 to lvl do
    result := result + a[i] + PathDelim;
end;

function GetLastPath(const aPath : String):string;
var
  a : TStringArray;
begin
  result := '';
  a := aPath.Split([PathDelim]);
  result := a[High(a)];
end;

{ TFileItem }

constructor TFileItem.Create;
begin
  inherited;
  FImg := nil;
  CreateGUID(FGuid);
  FDateAdded := now;
  FLock := TThreadList.Create;
  FStampGenerated:=False;
end;

constructor TFileItem.Create(aLog : ILog; const aFilename: String);
begin
  Create;
  FLog := aLog;
  FFilename := aFilename;
  FModified := false;
end;

destructor TFileItem.Destroy;
begin
  Flog := nil;
  if Assigned(FImg) then
    FImg.Free;
  FLock.Free;
  inherited Destroy;
end;

function TFileItem.GetImg: TBitmap;
begin
  FLock.LockList;
  try
    if not Assigned(FImg) then
    begin
      Fimg := TBitmap.Create;
      if FileExists(CacheFilename) then
        FImg.LoadFromFile(CacheFilename)
      else
        FImg := GenerateStamp;
    end;
    result := FImg;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetModified: Boolean;
begin
  FLock.LockList;
  try
    result := FModified;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetReadState: Boolean;
begin
  FLock.LockList;
  try
    result := FReadState;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetStampGenerated: Boolean;
begin
  FLock.LockList;
  try
    result := FStampGenerated;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetText: String;
begin
  FLock.LockList;
  try
    result := FText;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetDateAdded(AValue: TDAteTime);
begin
  FLock.LockList;
  try
    FDateAdded:=AVAlue;
    FModified:=True;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetReadState(AValue: Boolean);
begin
  FLock.LockList;
  try
    FReadState:=aValue;
    FModified := True;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetStampGenerated(AValue: Boolean);
begin
  FLock.LockList;
  try
    FStampGenerated := AValue;
    FModified:=True;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetText(const AValue: String);
begin
  FLock.LockList;
  try
    FText := AValue;
    FModified := True;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GenerateStamp:TBitmap;
begin
  result :=nil;
  if not FileExists(CacheFilename) then
    if FileExists(Filename) then
    begin
      FLock.LockList;
      try
        with TCbz.Create(FLog) do
        try
          Open(Self.Filename, zmRead);
          result := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
          result.SaveToFile(CacheFilename);
          StampGenerated:=True;
          FModified:=True;
        finally
          free;
        end;
      finally
        FLock.UnlockList;
      end;
    end;
end;

function TFileItem.GetCacheFilename: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/Library/';
{$else}
    IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library\';
{$endif}
  ForceDirectories(result);
  result :=  result + GUIDToString(FGuid) +'.bmp';
  //result := ChangeFileExt(result, '.bmp');
end;

function TFileItem.GetDateAdded: TDAteTime;
begin
  FLock.LockList;
  try
    Result := FDateAdded;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetFilename: String;
begin
  FLock.LockList;
  try
    Result := FFilename;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SaveToXml(aNode: TXmlElement);
begin
  with aNode do
  begin
    SetAttribute('Filename', FFilename);
    SetAttributeBool('ReadState', FReadState);
    SetAttributeBool('HasStamp', FStampGenerated);
    SetAttribute('Guid', GUIDToString(FGuid));
    SetAttributeDate('DateAdded', FDateAdded);
  end;
  FModified := False;
end;

procedure TFileItem.LoadFromXml(aNode: TXmlElement);
var
  s : string;
begin
  FModified := False;
  With aNode do
  begin
    FReadState := GetAttributeBool('ReadState');
    FStampGenerated := GetAttributeBool('HasStamp');
    FFilename:= GetAttribute('Filename');
    FDateAdded := GetAttributeDate('DateAdded', now);
    s := GetAttribute('Guid');
    if s <> '' then
      FGuid := StringToGUID(s)
    else
      FModified := True;
  end;
end;


{ TItemList }

function TItemList.GetModified: Boolean;
var
  i : integer;
begin
  result := FModified;
  if result then exit;

  Flock.LockList;
  try
    for i:= 0 to Count - 1 do
      if TFileItem(Objects[i]).Modified then
        Exit(True);
    result := False;
  finally
    FLock.UnlockList;
  end;
end;

function TItemList.GetDeletedCount: Integer;
var
  i : integer;
begin
  Flock.LockList;
  try
    result := 0;
    for i := 0 to Count - 1 do
      if not FileExists(TFileItem(Objects[i]).Filename) then
        inc(result);
  finally
    FLock.UnlockList;
  end;
end;

function TItemList.GetRootPath: String;
begin
  Flock.LockList;
  try
    result := FRootPath;
  finally
    FLock.UnlockList;
  end;
end;

function TItemList.GetStampLessCount: Integer;
var
  i : integer;
begin
  Flock.LockList;
  try
    result := 0;
    for i := 0 to Count - 1 do
      with TFileItem(Objects[i]) do
        if not StampGenerated then
        begin
          if not FileExists(CacheFilename) then
            inc(result)
          else
          begin
            FStampGenerated := True;
            FModified:=True;
          end;
        end;
  finally
    FLock.UnlockList;
  end;
end;

procedure TItemList.SetRootPath(AValue: String);
begin
  Flock.LockList;
  try
    FRootPath := AValue;
  finally
    FLock.UnlockList;
  end;
end;

function TItemList.GetReadCount: integer;
var
  i:integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if TfileItem(Objects[i]).ReadState then
      inc(result);
end;

constructor TItemList.Create(alog : ILog);
begin
  Flog := alog;
  inherited Create;
end;

destructor TItemList.Destroy;
begin
  Clear;
  Flog := nil;
  inherited Destroy;
end;

procedure TItemList.Clear;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    TFileItem(Objects[i]).free;

  inherited Clear;
  FModified := False;
end;

procedure TItemList.Delete(index: integer);
begin
  with TFileItem(Objects[index]) do
  begin
    if FileExists(CacheFilename) then
       DeleteFile(CacheFilename);

    free;
  end;

  inherited Delete(index);
  FModified := True;
end;

procedure TItemList.SaveToFile(const aFilename: String);
var
  i : integer;
  el, root : TXmlElement;
begin
  with TXMLDoc.Create do
  try
    root := CreateNewDocumentElement('Library');
    root.SetAttribute('RootPath', FRootPath);
    for i := 0 to Count - 1 do
   begin
     el := root.AddChildNode('Comic');
     TFileItem(Objects[i]).SaveToXml(el);
   end;
    SaveToFile(aFilename);
    FModified := False;
  finally
    Free;
  end;
end;

procedure TItemList.ResetStampState;
var
  i : integer;
begin
  for i:= 0 To Count - 1 do
    TFileItem(Objects[i]).StampGenerated:=False;
end;

procedure TItemList.LoadFromFile(const aFilename: String);
var
  i : integer;
  fi : TFileItem;
begin
  Clear;
  with TXMLDoc.Create do
  try
    LoadFromFile(aFilename);
    with DocumentElement do
    begin
      FRootPath := GetAttributeStr('RootPath');
      for i := 0 to NbElements - 1 do
      begin
        fi := TFileItem.Create(Flog, '');
        fi.LoadFromXml(Elements[i]);
        AddObject(fi.Filename, fi);
      end;
    end;
    FModified := False;
  finally
    Free;
  end;
end;


end.

