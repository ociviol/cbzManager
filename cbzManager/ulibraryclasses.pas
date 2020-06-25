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
    procedure SetImg(AValue: TBitmap);
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
    procedure SetFilename(AValue: String);
    procedure SetFSyncFileDAte(AValue: TDateTime);
    function SyncPathName(const aFilename : string):String;
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
    property  Img : TBitmap write SetImg;
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
    function CheckSync(bForce : Boolean = False):integer;
    procedure SyncFileDelete;

    property Filename : String read GetFilename write SetFilename;
    property ReadState : Boolean read GetReadState write SetReadState;
    property Image:TBitmap read GetImg;
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
    FFilename,
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
    procedure Clear;
    procedure Delete(index:integer);
    procedure LoadFromFile(const aFilename : String);
    procedure SaveToFile(const aFilename : String);
    procedure Save;
    procedure ResetStampState;
    procedure Cleanup;

    property Modified : Boolean read GetModified;
    property RootPath : String read GetRootPath write SetRootPath;
    property StampLessCount : Integer read GetStampLessCount;
    property StampCount : Integer read GetStampCount;
    property DeletedCount : Integer read GetDeletedCount;
    property ReadCount : Integer read GetReadCount;
    property SyncPath : String read GetSyncPath write SetSyncPath;
  end;



implementation

uses
  Forms, uCbz, Utils.Zipfile, strutils, LazUTF8
  //, utils.epub
  ;

const
  CS_StampWidth = 120;
  CS_StampHeight = 160;

{ TFileItem }

constructor TFileItem.Create;
begin
  inherited;
  FImg := nil;
  //CreateGUID(FGuid);
  FDateAdded := now;
  Fimg := nil;
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

function TFileItem.GetImg:TBitmap;
  procedure _Load;
  begin
    FImg := TBitmap.Create;
    with TPicture.Create do
    try
      LoadFromFile(CacheFilename);
      FImg.Width:=Graphic.Width;
      FImg.Height:=Graphic.Height;
      FImg.PixelFormat:=pf24bit;
      FImg.Canvas.Draw(0, 0, Graphic);
    finally
      Free;
    end;
  end;

begin
  FLock.LockList;
  try
    if Assigned(FImg) then
      Exit(FImg);

    if FileExists(CacheFilename) then
      _Load
    else
      Img := GenerateStamp;

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
    if AValue then
    begin
      if FileExists(CacheFilename) then
        DeleteFile(CacheFilename);
      If FileExists(SyncFilename) then
        DeleteFile(SyncFilename);
    end;
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
    FDateSetReadState:=now;
    FModified := True;
    FSyncFileDAte:=now;
    CheckSync(True);
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
var
  p : Tpicture;
begin
  result :=nil;
  if not FileExists(CacheFilename) then
  try
    if FileExists(FFilename) then
    begin
      FLock.LockList;
      try
        with TCbz.Create(FLog) do
        try
          try
            Open(Self.FFilename, zmRead);
            if FileCount <= 0 then
              Exit;
            result := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
            if Assigned(result) then
            begin
              p := TPicture.Create;
              with p do
              try
                Bitmap.Assign(result);
                SaveToFile(CacheFilename, 'jpg');
              finally
                p.Free;
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
    end;
  except
    on e: Exception do
      Flog.Log('TFileItem.GenerateStamp:Error:' + E.Message);
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

function TFileItem.CheckSync(bForce : Boolean = False):integer;
begin
  result := 0;
  FLock.LockList;
  try
    // update
    if FileExists(SyncFilename) and not bForce then
    try
      if (FileDateTodateTime(FileAge(SyncFilename)) > FSyncFileDAte) then
        with TXmlDoc.Create do
        try
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
              result := 2;
            end;
          FSyncFileDAte := FileDateTodateTime(FileAge(SyncFilename));
        finally
          free;
        end;
    except
      on E: Exception do
        FLog.Log('TFileItem.CheckSync:'+E.Message);
    end
    else
    // create file
    try
      with TXmlDoc.Create do
      try
        with CreateNewDocumentElement('Comic') do
        begin
          SetAttributeDate('DateSetReadState', FDateSetReadState);
          SetAttributeBool('ReadState', FReadState);
          result := 2;
        end;
        SaveToFile(SyncFilename);
      finally
        Free;
      end;
      FSyncFileDAte := FileDateTodateTime(FileAge(SyncFilename));
    except
      on e: Exception do
        FLog.Log('TFileItem.CheckSync:Error:' + E.Message);
    end;
  finally
    FLock.UnlockList;
  end;
end;

procedure TFileItem.SyncFileDelete;
var
  s : string;
begin
  try
    s := SyncFilename;
    if FileExists(s) then
        DeleteFile(s);
  except
    on e: exception do
      Flog.Log('TItemList.SyncFileDelete:Error:' + e.Message);
  end;
end;

function TFileItem.SyncPathName(const aFilename : string):String;
begin
  if FSyncPathFilename <> '' then
    Exit(FSyncPathFilename);

  result := extractFilePath(aFilename);
  result := ExcludeLeadingPathDelimiter(result.Replace(Parent.FRootPath, ''));
  FSyncPathFilename := result;
end;

procedure TFileItem.SetImg(AValue: TBitmap);
begin
  if Assigned(FImg) then
    FreeAndNil(FImg);
  FImg:=AValue;
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

procedure TFileItem.SetFilename(AValue: String);
begin
  FLock.LockList;
  try
    FFilename := AValue;
    FModified:=True;
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

function Fix(AnUTF8String: string):string;
var
  p: PChar;
  CPLen: integer;
  FirstByte, SecondByte, ThirdByte, FourthByte: Char;
begin
  result := '';
  p:=PChar(AnUTF8String);
  repeat
    CPLen := UTF8CodepointSize(p);

    if CPLen = 1 then
      result := result + String(p[0])
    else
    // Here you have a pointer to the char and its length
    // You can access the bytes of the UTF-8 Char like this:
    if CPLen >= 1 then FirstByte := P[0];
    if CPLen >= 2 then SecondByte := P[1];
    if CPLen >= 3 then ThirdByte := P[2];
    if CPLen = 4 then FourthByte := P[3];

    inc(p,CPLen);
  until (CPLen=0) or (p^ = #0);
end;

//const
//  accented : String =   'ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÇçÌÍÎÏìíîïÙÚÛÜùúûüÿÑñ';
//  arracc : array[0..52] of string =
//      ('À','Á','Â','Ã','Ä','Å','à','á','â','ã','ä','å',
//       'Ò','Ó','Ô','Õ','Ö','Ø','ò','ó','ô','õ','ö','ø',
//       'È','É','Ê','Ë','è','é','ê','ë',
//       'Ç','ç',
//       'Ì','Í','Î','Ï','ì','í','î','ï',
//       'Ù','Ú','Û','Ü','ù','ú','û','ü','ÿ',
//       'Ñ','ñ'
//      );
//  unaccented : ansistring = 'AAAAAAaaaaaaOOOOOOooooooEEEEeeeeCcIIIIiiiiUUUUuuuuyNn';
//
function makefilename(const aFilename : String):String;
//var
//  p: PChar;
//  CPLen: integer;
//  s : string;
  //FirstByte, SecondByte, ThirdByte, FourthByte: Char;
begin
  //if s <> accented then
  //  convert(s);

  result := aFilename;
  (*
  p:=PChar(aFilename);
  repeat
    CPLen := UTF8CodepointSize(p);

    if CPLen = 1 then
      result := result + String(p[0]);
    //else
    //if p[0] < #128  then
    //  result := result + String(p[0])
    //else
    //  result := result + '.';
    {
    // Here you have a pointer to the char and its length
    // You can access the bytes of the UTF-8 Char like this:
    if CPLen >= 1 then FirstByte := P[0];
    if CPLen >= 2 then SecondByte := P[1];
    if CPLen >= 3 then ThirdByte := P[2];
    if CPLen = 4 then FourthByte := P[3];
    }
    inc(p,CPLen);
  until (CPLen=0) or (p^ = #0);
  *)
end;

function TFileItem.SyncFilename: String;
var
  s : string;
begin
  FLock.LockList;
  try
    if FSyncFilename <> '' then
      Exit(FSyncFilename);

    s := makefilename(ExtractFilename(FFilename));
    result := IncludeTrailingPathDelimiter(Parent.FSyncPath) + SyncPathName(FFilename);
    ForceDirectories(result);
    result := IncludeTrailingPathDelimiter(result) + ChangeFileExt(s, '.xml');
    FSyncFilename := result;
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

    s := makefilename(ExtractFilename(FFilename));
    result := IncludeTrailingPathDelimiter(Parent.FSyncPath) + SyncPathName(FFilename);
    ForceDirectories(result);
    result := IncludeTrailingPathDelimiter(result) + ChangeFileExt(s, '.jpg');
    FCacheFilename := result;
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
    SetAttributeBool('ReadState', FReadState);
    SetAttributeBool('HasStamp', FStampGenerated);
    SetAttributeDate('DateAdded', FDateAdded);
    SetAttributeDate('SyncFileDAte', FSyncFileDAte);
    SetAttributeDate('DateSetReadState', FDateSetReadState);
    AddChildNode('Filename').Text:=FFilename;
    //AddChildNode('CacheFile').Text:=FCacheFilename;
    //AddChildNode('SyncFile').Text:=FSyncFilename;
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
    FDateAdded := GetAttributeDate('DateAdded', now);
    FSyncFileDAte := GetAttributeDate('SyncFileDAte', 0);
    FDateSetReadState := GetAttributeDate('DateSetReadState', 0);
    FFilename:= GetNode('Filename', true).Text;
    //FCacheFilename := GetNode('CacheFile', true).Text;
    //FSyncFilename := GetNode('SyncFile', true).Text;
  end;
end;


{ TItemList }

function TItemList.GetModified: Boolean;
var
  i : integer;
begin
  result := FModified;
  if result then exit;

  with LockList do
  try
    for i:= 0 to Count - 1 do
      if TFileItem(Objects[i]).Modified then
        Exit(True);
    result := False;
  finally
    UnlockList;
  end;
end;

function TItemList.GetDeletedCount: Integer;
var
  i : integer;
begin
  with LockList do
  try
    result := 0;
    for i := 0 to Count - 1 do
      if TFileItem(Objects[i]).Deleted then
        inc(result);
  finally
    UnlockList;
  end;
end;

function TItemList.GetRootPath: String;
begin
  with LockList do
  try
    result := FRootPath;
  finally
    UnlockList;
  end;
end;

function TItemList.GetStampCount: Integer;
var
  i : integer;
begin
  with LockList do
  try
    result := 0;
    for i := 0 to Count - 1 do
      with TFileItem(Objects[i]) do
        if FStampGenerated and FileExists(CacheFilename) then
          inc(result);
  finally
    UnlockList;
  end;
end;

function TItemList.GetStampLessCount: Integer;
var
  i : integer;
begin
  with LockList do
  try
    result := 0;
    for i := 0 to Count - 1 do
      with TFileItem(Objects[i]) do
        if not FStampGenerated then
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
  finally
    UnlockList;
  end;
end;

function TItemList.GetSyncPath: String;
begin
  with LockList do
  try
    Result := FSyncPath;
  finally
    UnlockList;
  end;
end;


procedure TItemList.SetRootPath(AValue: String);
begin
  with LockList do
  try
    FRootPath := AValue;
  finally
    UnlockList;
  end;
end;

function TItemList.GetReadCount: integer;
var
  i:integer;
begin
  with LockList do
  try
    result := 0;
    for i := 0 to Count - 1 do
      if TfileItem(Objects[i]).ReadState then
        inc(result);
  finally
    UnlockList;
  end;
end;

procedure TItemList.SetSyncPath(AValue: String);
begin
  with LockList do
  try
    FSyncPath := AValue;
  finally
    UnlockList;
  end;
end;

constructor TItemList.Create(alog: ILog; const aSyncPath: String = '');
begin
  Flog := alog;
  FSyncPath := aSyncPath;
  FFilename:='';
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
  with LockList do
  try
    try
      for i := 0 to Count - 1 do
        TFileItem(Objects[i]).Free;
    except
    end;

    inherited Clear;
    FModified := False;
  finally
    UnlockList;
  end;
end;

procedure TItemList.Delete(index: integer);
begin
  with LockList do
  try
    with TFileItem(Objects[index]) do
    try
      if FileExists(CacheFilename) then
      begin
         DeleteFile(CacheFilename);
         FLog.Log('TItemList.Delete : Delete cache : ' + CacheFilename);
      end;

      free;

    except
      on e: exception do
        Flog.Log('TItemList.Delete:Error:' + e.Message);
    end;

    inherited Delete(index);
    FModified := True;
  finally
    UnlockList;
  end;
end;

procedure TItemList.SaveToFile(const aFilename: String);
var
  i : integer;
  el, root : TXmlElement;
begin
  with LockList do
  try
    with TXMLDoc.Create do
    try
      try
        root := CreateNewDocumentElement('Library');
        root.SetAttribute('RootPath', FRootPath);
        for i := 0 to Count - 1 do
          if not TFileItem(Objects[i]).Deleted then
          begin
            el := root.AddChildNode('Comic');
            TFileItem(Objects[i]).SaveToXml(el);
          end;
        SaveToFile(aFilename);
        FFilename:=aFilename;
        FModified := False;
      except
        on E: Exception do
          FLog.Log('TItemList.SaveToFile:'+E.Message);
      end;
    finally
      Free;
    end;
  finally
    UnlockList;
  end;
end;

procedure TItemList.Save;
begin
  if Modified and (FFilename <> '') then
    SaveToFile(FFilename);
end;

procedure TItemList.ResetStampState;
var
  i : integer;
begin
  with LockList do
  try
    for i:= 0 To Count - 1 do
    TFileItem(Objects[i]).StampGenerated:=False;
  finally
    UnlockList;
  end;
end;

procedure TItemList.Cleanup;
var
  i,j : integer;
begin
  with LockList do
  try
    for i:= Count - 1 downto 0 do
      for j:=0 to i-1 do
        if Utf8CompareStr(TFileItem(Objects[i]).Filename,
                          TFileItem(Objects[j]).Filename) = 0 then
           Delete(i);
  finally
    UnlockList;
  end;
end;

procedure TItemList.LoadFromFile(const aFilename: String);
var
  i : integer;
  fi : TFileItem;
begin
  with LockList do
  try
    Clear;
    with TXMLDoc.Create do
    try
      try
        LoadFromFile(aFilename);
        FFilename:=aFilename;
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
      except
        on E: Exception do
          FLog.Log('TItemList.LoadFromFile:'+E.Message);
      end;
    finally
      Free;
    end;
    Sort;
  finally
    UnlockList;
  end;

end;


end.

