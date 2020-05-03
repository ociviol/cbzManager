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

  TItemList = Class;
  { TFileItem }

  TFileItem = Class
  private
    FFilename : String;
    FImg : TBitmap;
    FLog : ILog;
    FText: String;
    FReadState : Boolean;
    //FGuid : TGUID;
    FModified : Boolean;
    FLock : TThreadList;
    FDateAdded,
    FSyncFileDAte,
    FDateSetReadState: TDateTime;
    FStampGenerated : Boolean;
    FDeleted : Boolean;
    FParent : TItemList;
    FSyncFilename,
    FCacheFilename,
    FSyncPathFilename : String;

    function GetSyncFileDAte: TDateTime;
    procedure SetFSyncFileDAte(AValue: TDateTime);
    function SyncPathName(const aFilename : string):String;
    function SyncFilenameDelete: String;
    function SyncFilename : String;
    function GetCacheFilename: String;
    function GetDateAdded: TDAteTime;
    function GetDateSetReadState: TDateTime;
    function GetDeleted: Boolean;
    function GetFilename: String;
    function GetImg: TBitmap;
    function GetModified: Boolean;
    function GetReadState: Boolean;
    function GetStampGenerated: Boolean;
    function GetText: String;
    procedure SetDateAdded(AValue: TDAteTime);
    procedure SetDateSetReadState(AValue: TDateTime);
    procedure SetSyncFileDAte(AValue: TDateTime);
    procedure SetDeleted(AValue: Boolean);
    procedure SetReadState(AValue: Boolean);
    procedure SetStampGenerated(AValue: Boolean);
    procedure SetText(const AValue: String);
  protected
    procedure SaveToXml(aNode : TXmlElement);
    procedure LoadFromXml(aNode : TXmlElement);
    property CacheFilename : String read GetCacheFilename;
    property Modified : Boolean read GetModified;
    property StampGenerated : Boolean read GetStampGenerated write SetStampGenerated;
    property Parent : TItemList read FParent;
  public
    constructor Create;
    constructor Create(aParent : TItemList; aLog : ILog; const aFilename : String);
    destructor Destroy; override;

    function GenerateStamp:TBitmap;
    function CheckSync:integer;
    procedure SyncFileDelete;

    property Filename : String read GetFilename;
    property ReadState : Boolean read GetReadState write SetReadState;
    property Img:TBitmap read GetImg;
    property Text : String Read GetText write SetText;
    property DateAdded : TDAteTime read GetDateAdded write SetDateAdded;
    property DateSetReadState : TDateTime read GetDateSetReadState write SetDateSetReadState;
    property Deleted : Boolean read GetDeleted write SetDeleted;
    property SyncFileDAte : TDateTime read GetSyncFileDAte write SetFSyncFileDAte;
  end;

  { TItemList }

  TItemList = Class(TThreadStringList)
  private
    Flog : ILog;
    FRootPath,
    FSyncPath: String;
    FModified : Boolean;

    function GetDeletedCount: Integer;
    function GetModified: Boolean;
    function GetRootPath: String;
    function GetStampCount: Integer;
    function GetStampLessCount: Integer;
    function GetSyncPath: String;
    procedure SetRootPath(AValue: String);
    function GetReadCount:integer;
    procedure SetSyncPath(AValue: String);
  protected
  public
    constructor Create(alog : ILog; const aSyncPath : String = '');
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(index:integer); override;
    procedure LoadFromFile(const aFilename : String); override;
    procedure SaveToFile(const aFilename : String); override;
    procedure ResetStampState;

    property Modified : Boolean read GetModified;
    property RootPath : String read GetRootPath write SetRootPath;
    property StampLessCount : Integer read GetStampLessCount;
    property StampCount : Integer read GetStampCount;
    property DeletedCount : Integer read GetDeletedCount;
    property ReadCount : Integer read GetReadCount;
    property SyncPath : String read GetSyncPath write SetSyncPath;
  end;


function GetFirstPath(const aPath : String; Lvl : integer = 2):String;
function GetLastPath(const aPath : String):string;

implementation

uses
  Forms, uCbz, Utils.Zipfile
  //, utils.epub
  ;

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
  //CreateGUID(FGuid);
  FDateAdded := now;
  FLock := TThreadList.Create;
  FStampGenerated:=False;
end;

constructor TFileItem.Create(aParent : TItemList; aLog : ILog; const aFilename: String);
begin
  Create;
  FParent := aParent;
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
        with TPicture.Create do
        try
          LoadFromFile(CacheFilename);
          FImg.Width:=Graphic.Width;
          FImg.Height:=Graphic.Height;
          FImg.PixelFormat:=pf24bit;
          FImg.Canvas.Draw(0, 0, Graphic);
        finally
          Free;
        end
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

procedure TFileItem.SetDateSetReadState(AValue: TDateTime);
begin
  FLock.LockList;
  try
    FDateSetReadState := AValue;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetSyncFileDAte(AValue: TDateTime);
begin

end;

{
procedure TFileItem.SetDateLastSync(AValue: TDateTime);
begin
  FLock.LockList;
  try
    FDateLastSync := AValue;
  finally
    FLock.UnlockList;
  end;
end;
}

procedure TFileItem.SetDeleted(AValue: Boolean);
begin
  FLock.LockList;
  try
    FDeleted := AValue;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetReadState(AValue: Boolean);
begin
  FLock.LockList;
  try
    FReadState:=aValue;
    FDateSetReadState:=now;
    FModified := True;
    FSyncFileDAte:=now;
    CheckSync;
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
    if FileExists(FFilename) then
    begin
      FLock.LockList;
      try
        {
        s := LowerCase(ExtractFileExt(FFilename));
        if (s = '.epub') then
          with TEpubHandler.Create do
          try
            LoadFromFile(FFilename);
            s := CoverImage;
          finally
            Free;
          end
        else
        if (s = '.cbz') then
        }
        with TCbz.Create(FLog) do
        try
          try
            Open(Self.FFilename, zmRead);
            result := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
            if Assigned(result) then
            begin
              with TPicture.Create do
              try
                Bitmap.Assign(result);
                SaveToFile(CacheFilename, 'jpg');
              finally
                Free;
              end;
              FStampGenerated:=True;
              FModified:=True;
            end;
          except
            on e: Exception do
              FLog.Log('TFileItem.GenerateStamp:Error:' + E.Message);
          end;
        finally
          free;
        end;
      finally
        FLock.UnlockList;
      end;
    end
    else
    if not StampGenerated then
    begin
      FLock.LockList;
      try
        FStampGenerated := True;
        FModified := True;
      finally
        FLock.UnlockList;
      end;
    end;
end;

function TFileItem.CheckSync:integer;
begin
  result := 0;
  with TXmlDoc.Create do
  try
    // update
    if FileExists(SyncFilename) then
    begin
      if (FileDateTodateTime(FileAge(SyncFilename)) <> FSyncFileDAte) then
      begin
        LoadFromFile(SyncFilename);
        with DocumentElement do
          if FDateSetReadState < GetAttributeDate('DateSetReadState', 0) then
          begin
            FDateSetReadState := GetAttributeDate('DateSetReadState', 0);
            FReadState := GetAttributeBool('ReadState');
            FModified := True;
            result := 1;
          end
          else
          if FDateSetReadState > GetAttributeDate('DateSetReadState', 0) then
          begin
            SetAttributeDate('DateSetReadState', FDateSetReadState);
            SetAttributeBool('ReadState', FReadState);
            SaveToFile(SyncFilename);
            result := 1;
          end;
        FSyncFileDAte := FileDateTodateTime(FileAge(SyncFilename));
      end;
    end
    else
    // deleted
    if FileExists(SyncFileNameDelete) then
      exit(-1)
    else
      // create file
    begin
      with CreateNewDocumentElement('Comic') do
      begin
        SetAttributeDate('DateSetReadState', FDateSetReadState);
        SetAttributeBool('ReadState', FReadState);
        result := 1;
      end;
      SaveToFile(SyncFilename);
      FSyncFileDAte := FileDateTodateTime(FileAge(SyncFilename));
    end;
  finally
    Free;
  end;
end;

function TFileItem.SyncFilenameDelete: String;
begin
  result := ExtractFilePath(SyncFilename) + '.' + ExtractFileName(SyncFilename);
end;

procedure TFileItem.SyncFileDelete;
var
  s : string;
begin
  s := SyncFilenameDelete;
  if FileExists(s) then
      DeleteFile(s);

  if FileExists(SyncFilename) then
    RenameFile(SyncFilename, s);
end;

function BestFit(const AInput: String): String;
//const
//  ChrRemoved : array[0..4] of char = ('_', '?', ' ', '''', '"');
//
//  Char_Accents      = 'ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÇçÌÍÎÏìíîïÙÚÛÜùúûüÿÑñ';
//  Char_Sans_Accents = 'AAAAAAaaaaaaOOOOOOooooooEEEEeeeeCcIIIIiiiiUUUUuuuuyNn';
//var
//  c : char;
//  i : integer;
begin
  result := aInput;
  {
  for i := 1 to Length(AInput) do
    if AInput[i] < #128 then
      result := result + aInput[i];
      }
 (*
  for c in Char_Accents do
    if result[]
  Result := UTF8ToISO_8859_15(AInput);
{$ifdef Darwin}
{$endif}

  while result.Contains('?') do
    result := result.Remove(result.IndexOf('?')-1, 1);

  for c in ChrRemoved do
    if Result.Contains(c) then
      result := result.Replace(c, '', [rfReplaceAll]);
 *)
end;


function TFileItem.SyncPathName(const aFilename : string):String;
begin
  if FSyncPathFilename <> '' then
    Exit(FSyncPathFilename);

  result := extractFilePath(aFilename);
  result := ExcludeLeadingPathDelimiter(result.Replace(Parent.FRootPath, ''));
  FSyncPathFilename := result;
end;

function TFileItem.GetSyncFileDAte: TDateTime;
begin
  FLock.LockList;
  try
    Result := FSyncFileDAte;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SetFSyncFileDAte(AValue: TDateTime);
begin
  FLock.LockList;
  try
    FSyncFileDAte := AValue;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.SyncFilename: String;
var
  s : string;
begin
  FLock.LockList;
  try
    if FSyncFilename <> '' then
      Exit(FSyncFilename);

    s := lowercase(ExtractFilename(FFilename));
    //md := MD5Print(MD5String(lowercase(ExtractFilename(FFilename))));
    result := BestFit(IncludeTrailingPathDelimiter(Parent.FSyncPath) + SyncPathName(FFilename));
    ForceDirectories(result);
    result := IncludeTrailingPathDelimiter(result) + ChangeFileExt(s, '.xml');
    FSyncFilename := BestFit(result);
  finally
    Flock.UnlockList;
  end;
end;

function TFileItem.GetCacheFilename: String;
var
  s : string;
begin
  FLock.LockList;
  try
    if FCacheFilename <> '' then
      Exit(FCacheFilename);

    s := lowercase(ExtractFilename(FFilename));
    //md := MD5Print(MD5String(lowercase(ExtractFilename(FFilename))));
    result := BestFit(IncludeTrailingPathDelimiter(Parent.FSyncPath) + SyncPathName(FFilename));
    ForceDirectories(result);
    result := IncludeTrailingPathDelimiter(result) + ChangeFileExt(s, '.jpg');
    FCacheFilename := BestFit(result);
  finally
    Flock.UnlockList;
  end;
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

function TFileItem.GetDateSetReadState: TDateTime;
begin
  FLock.LockList;
  try
    Result := FDateSetReadState;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetDeleted: Boolean;
begin
  FLock.LockList;
  try
    Result := FDeleted;
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
    SetAttributeDate('DateAdded', FDateAdded);
    SetAttributeDate('SyncFileDAte', FSyncFileDAte);
    SetAttributeDate('DateSetReadState', FDateSetReadState);
  end;
  FModified := False;
end;

procedure TFileItem.LoadFromXml(aNode: TXmlElement);
begin
  FModified := False;
  With aNode do
  begin
    FReadState := GetAttributeBool('ReadState');
    FStampGenerated := GetAttributeBool('HasStamp');
    FFilename:= GetAttribute('Filename');
    FSyncFileDAte := GetAttributeDate('SyncFileDAte', 0);
    FDateAdded := GetAttributeDate('DateAdded', now);
    FDateSetReadState := GetAttributeDate('DateSetReadState', 0);
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

function TItemList.GetStampCount: Integer;
var
  i : integer;
begin
  Flock.LockList;
  try
    result := 0;
    for i := 0 to Count - 1 do
      with TFileItem(Objects[i]) do
        if FStampGenerated and FileExists(CacheFilename) then
          inc(result);
  finally
    Flock.UnlockList;
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
        if not FStampGenerated then
        begin
          if not FileExists(CacheFilename) then
          begin
            inc(result);
//            FLog.Log('TItemList.GetStampLessCount:Cache File not found:'+CacheFilename);
          end
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

function TItemList.GetSyncPath: String;
begin
    Flock.LockList;
  try
    Result := FSyncPath;
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

procedure TItemList.SetSyncPath(AValue: String);
begin
    Flock.LockList;
  try
    FSyncPath := AValue;
  finally
    FLock.UnlockList;
  end;
end;

constructor TItemList.Create(alog: ILog; const aSyncPath: String = '');
begin
  Flog := alog;
  FSyncPath := aSyncPath;
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
    begin
       DeleteFile(CacheFilename);
       FLog.Log('TItemList.Delete : Delete cache : ' + CacheFilename);
    end;

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
      if GetAttributeStr('RootPath') <> '' then
        FRootPath := GetAttributeStr('RootPath');
      for i := 0 to NbElements - 1 do
      begin
        fi := TFileItem.Create(Self, Flog, '');
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

