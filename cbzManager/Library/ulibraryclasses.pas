unit uLibraryClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  utils.Logger, uxmldoc, Utils.Strings, utils.Json;

type
  TDisplayFilter = (dfUnread, dfAll);
  TDisplayFilters = set of TDisplayFilter;

  TItemList = Class;

  { TSyncObject }

  TSyncObject = class(TJsonObject)
    private
      FCurPage: integer;
      FDateSetReadState: string;
      FReadState: boolean;
      FLogicalPath : string;
    published
      constructor Create(const aDateSetReadState: string=''; bReadState: boolean=false; iCurPage : integer = 0;const aLogicalPath:string = '');
      property DateSetReadState : string read FDateSetReadState write FDateSetReadState;
      property ReadState : boolean read FReadState write FReadState;
      property CurPage : integer read FCurPage write FCurPage;
      property LogicalPath:String read FLogicalPath write FLogicalPath;
  end;

  { TFileItem }

  TFileItem = Class
  private
    FCacheFilename,
    FJsonFileName : string;
    procedure SetImg(AValue: TBitmap);
  private
    FFilename : String;
    FImg : TBitmap;
    FLog : ILog;
    FText: String;
    FReadState: Boolean;
    FCurPage : integer;
    //FGuid : TGUID;
    FModified : Boolean;
    FLock : TThreadList;
    FDateAdded,
    FSyncFileDAte,
    FDateSetReadState: TDateTime;
    FDeleted : Boolean;
    FParent : TItemList;
    //FSyncFilename,
    //FSyncJsonFilename,
    //FCacheFilename,
    FSyncPathFilename : String;

    function GetCurPage: Integer;
    function GetLogicalPath: string;
    function GetSyncFileDAte: TDateTime;
    procedure SetCurPage(AValue: Integer);
    procedure SetFilename(AValue: String);
    procedure SetFSyncFileDAte(AValue: TDateTime);
    function SyncPathName(const aFilename : string):String;
    //function SyncFilename : String;
    function SyncJsonFilename : String;
    function GetCacheFilename: String;
    function GetDateAdded: TDAteTime;
    function GetDateSetReadState: TDateTime;
    function GetDeleted: Boolean;
    function GetFilename: String;
    function GetImg: TBitmap;
    function GetModified: Boolean;
    function GetReadState: Boolean;
    function GetText: String;
    procedure SetDateAdded(AValue: TDAteTime);
    procedure SetDateSetReadState(AValue: TDateTime);
    procedure SetSyncFileDAte(AValue: TDateTime);
    procedure SetDeleted(AValue: Boolean);
    procedure SetReadState(AValue: Boolean);
    procedure SetText(const AValue: String);
    property  Img : TBitmap write SetImg;
  protected
    procedure SaveToXml(aNode : TXmlElement);
    procedure LoadFromXml(aNode : TXmlElement);
    property CacheFilename : String read GetCacheFilename;
    property Modified : Boolean read GetModified;
    property Parent : TItemList read FParent;
  public
    constructor Create;
    constructor Create(aParent : TItemList; aLog : ILog; const aFilename : String);
    destructor Destroy; override;

    procedure GenerateStamp;
    function CheckSync(bForce : Boolean = False):integer;
    procedure SyncFileDelete;
    property Image:TBitmap read GetImg;
    property LogicalPath : string read GetLogicalPath;
 published
    property Filename : String read GetFilename write SetFilename;
    property ReadState : Boolean read GetReadState write SetReadState;
    property Text : String Read GetText write SetText;
    property DateAdded : TDAteTime read GetDateAdded write SetDateAdded;
    property DateSetReadState : TDateTime read GetDateSetReadState write SetDateSetReadState;
    property Deleted : Boolean read GetDeleted write SetDeleted;
    property SyncFileDAte : TDateTime read GetSyncFileDAte write SetFSyncFileDAte;
    property CurPage : Integer read GetCurPage write SetCurPage;
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
    procedure Cleanup;

    property Modified : Boolean read GetModified;
    property RootPath : String read GetRootPath write SetRootPath;
    property StampLessCount : Integer read GetStampLessCount;
    property StampCount : Integer read GetStampCount;
    property DeletedCount : Integer read GetDeletedCount;
    property ReadCount : Integer read GetReadCount;
    property SyncPath : String read GetSyncPath write SetSyncPath;
  end;

function ConfigPath: String;
function RemoveDiacritics(const S: string): string;
function ConvertString(const Src: shortstring): shortstring;

implementation

uses
  Forms, uCbz, Utils.Zipfile, LazUTF8, DateUtils, md5
  //, utils.epub
  ;

const
  CS_StampWidth = 120; //120;
  CS_StampHeight = 156; //160;

function ConfigPath: String;
begin
  result := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
  result := StringReplace(result, 'cbzLibrary', 'cbzManager', []);
end;

{ TSyncObject }

constructor TSyncObject.Create(const aDateSetReadState: string;
  bReadState: boolean; iCurPage: integer;const aLogicalPath:string = '');
begin
  inherited Create;
  FReadState:=bReadState;
  FCurPage := iCurPage;
  FLogicalPath:= aLogicalPath;
  if aDateSetReadState <> '' then
    FDateSetReadState := aDateSetReadState
  else
    FDateSetReadState:=FormatDateTime('yyyy-mm-dd', 0) + 'T' +
                       FormatDateTime('hh":"nn":"ss.zzz', 0);
end;

{ TFileItem }

constructor TFileItem.Create;
begin
  inherited;
  FImg := nil;
  //CreateGUID(FGuid);
  FCacheFilename := '';
  FJsonFileName := '';
  FDateAdded := now;
  Fimg := nil;
  FLock := TThreadList.Create;
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

    if not FileExists(CacheFilename) then
      GenerateStamp;

    _Load;

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
      If FileExists(SyncJsonFilename) then
        DeleteFile(SyncJsonFilename);
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

procedure TFileItem.GenerateStamp;
var
  p : Tpicture;
  b : TBitmap;
begin
  //result :=nil;
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
            b := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
            if Assigned(b) then
            try
              p := TPicture.Create;
              with p do
              try
                Bitmap.Assign(b);
                SaveToFile(CacheFilename, 'jpg');
              finally
                p.Free;
              end;
              FModified:=True;
            finally
              b.free;
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
  end;
end;

function _IsDateTime(aText : String): Boolean; {$ifdef O_INLINE} inline; {$endif}
begin
  result := False;
  // is xmldatetime
  if (length(aText) = 23) or (length(aText) = 19) then
    result := (aText[5] = '-') and (aText[11] = 'T');
end;

function _IsDate(aText : String): Boolean; {$ifdef O_INLINE} inline; {$endif}
begin
  // 2015-12-31
  result := False;
  if length(aText) = 10 then
  begin
    result := (aText[5] = '-') and (aText[8] = '-');
    if result then
      try
        strtoint(copy(aText, 1, 4));
        strtoint(copy(aText, 6, 2));
        strtoint(copy(aText, 9, 2));
      except
        result := False;
      end;
  end;
end;

function _IsoDateToDateTime(isoDate : String):TDateTime;
var
  sy, sm, sd, sh, smm, ss, sms : string;
  y, m, d, h, mm, s, ms : word;
begin
  // i.e : 2015-01-01
  sy := '';
  sm := '';
  sd := '';
  sh := '';
  smm := '';
  ss := '';
  sms := '';

  try
    if (length(isoDate) >= 10) and
       (_IsDate(isoDate) or _IsDateTime(isoDate)) then
    begin
      sy  := copy(isoDate, 1, 4);
      sm  := copy(isoDate, 6, 2);
      sd  := copy(isoDate, 9, 2);
    end;
    // has time part
    // i.e : 2015-01-01T00:00:00
    if (length(isoDate) >= 19) and _IsDateTime(isoDate) then
    begin
      sh  := copy(isoDate, 12, 2);
      smm := copy(isoDate, 15, 2);
      ss  := copy(isoDate, 18, 2);
    end;
    // has milliseconds part
    // i.e : 2015-01-01T00:00:00.000
    // i.e : 2015-01-01T00:00:00.5
    if (length(isoDate) > 19) and _IsDateTime(isoDate) then
      sms := copy(isoDate, 21, 3);

    y  := StrToIntDef(sy, 0);
    m  := StrToIntDef(sm, 0);
    d  := StrToIntDef(sd, 0);
    h  := StrToIntDef(sh, 0);
    mm := StrToIntDef(smm, 0);
    s  := StrToIntDef(ss, 0);
    ms := StrToIntDef(sms, 0);

    Result := EncodeDateTime(y, m, d, h, mm, s, ms);
  except
    result := 0;
  end;
end;

function TFileItem.CheckSync(bForce : Boolean = False):integer;
var
  o : TSyncObject;
begin
  result := 0;
  FLock.LockList;
  try
    // update
    if (FileExists(SyncJsonFilename) and (not bForce)) then
    begin
      if (FileDateTodateTime(FileAge(SyncJsonFilename)) > FSyncFileDate) or Modified then
      begin
        o := TSyncObject(TJsonObject.Load(SyncJsonFilename, TSyncObject.Create));
        try
          // pull
          if (FDateSetReadState < _IsoDateToDateTime(o.DateSetReadState)) then
          begin
            FDateSetReadState := _IsoDateToDateTime(o.DateSetReadState);
            FReadState := o.ReadState;
            FCurPage := o.CurPage;
            FModified := True;
            result := 1;
          end
          else
          begin
            // push
            if FDateSetReadState > _IsoDateToDateTime(o.DateSetReadState) then
            begin
              o.DateSetReadState := FormatDateTime('yyyy-mm-dd', FDateSetReadState) + 'T' +
                                    FormatDateTime('hh":"nn":"ss.zzz', FDateSetReadState);
              o.ReadState := FReadState;
              o.Save(SyncJsonFilename);
              result := 2;
            end;

            if o.CurPage <> FCurPage then
              if o.CurPage > FCurPage then
                FCurPage := o.CurPage
              else
              begin
                o.CurPage := FCurPage;
                o.Save(SyncJsonFilename);
                result := 2;
              end;
          end;
        finally
          o.free;
        end;
        FSyncFileDate := FileDateTodateTime(FileAge(SyncJsonFilename));
      end;
    end
    else
    // create file
    try
      o := TSyncObject.Create(FormatDateTime('yyyy-mm-dd', FDateSetReadState) + 'T' +
                              FormatDateTime('hh":"nn":"ss.zzz', FDateSetReadState),
                              FReadState, FCurPage, LogicalPath);
      try
        o.Save(SyncJsonFilename);
      finally
        o.free;
      end;
      FSyncFileDAte := FileDateTodateTime(FileAge(SyncJsonFilename));
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
    s := SyncJsonFilename;
    if FileExists(s) then
        DeleteFile(s);
    // delete cache file
    s := GetCacheFilename;
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

function TFileItem.GetCurPage: Integer;
begin
  FLock.LockList;
  try
    Result := FCurPage;
  finally
    FLock.UnlockList;
  end;
end;

function TFileItem.GetLogicalPath: string;
begin
  result := FFilename.Replace(Parent.RootPath, '').Replace('\', '/', [rfReplaceAll]);
end;

procedure TFileItem.SetCurPage(AValue: Integer);
begin
  FLock.LockList;
  try
    FCurPage:=aValue;
    FDateSetReadState:=now;
    FModified := True;
    FSyncFileDate:=now;
    CheckSync(True);
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

function makefilename(const aFilename : String):String; inline;
begin
  result := aFilename;
end;

function ConvertString(const Src: shortstring): shortstring;
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.ASCII.GetBytes(Src);
  Result := TEncoding.ASCII.GetString(Bytes);
  result := StringReplace(result, '?', '', [rfReplaceAll]);
end;

function RemoveDiacritics(const S: string): string;
var
  F: Boolean;
  I: SizeInt;
  PS, PD: PChar;
begin
  SetLength(Result, Length(S));
  PS := PChar(S);
  PD := PChar(Result);
  I := 0;
  while PS^ <> #0 do
  begin
    F := PS^ = #195;
    if F then
      case PS[1] of
        #128..#134: PD^ := 'A';
        #135: PD^ := 'C';
        #136..#139: PD^ := 'E';
        #140..#143: PD^ := 'I';
        #144: PD^ := 'D';
        #145: PD^ := 'N';
        #146..#150, #152: PD^ := 'O';
        #151: PD^ := 'x';
        #153..#156: PD^ := 'U';
        #157: PD^ := 'Y';
        #158: PD^ := 'P';
        #159: PD^ := 's';
        #160..#166: PD^ := 'a';
        #167: PD^ := 'c';
        #168..#171: PD^ := 'e';
        #172..#175: PD^ := 'i';
        #176: PD^ := 'd';
        #177: PD^ := 'n';
        #178..#182, #184: PD^ := 'o';
        #183: PD^ := '-';
        #185..#188: PD^ := 'u';
        #190: PD^ := 'p';
        #189, #191: PD^ := 'y';
      else
        F := False;
      end;
    if F then
      Inc(PS)
    else
      PD^ := PS^;
    Inc(I);
    Inc(PD);
    Inc(PS);
  end;
  SetLength(Result, I);
  result := lowercase(result);
end;

function TFileItem.SyncJsonFilename: String;
var
  s : shortstring;
begin
  FLock.LockList;
  try
    if FJsonFileName = '' then
    begin
      //s := RemoveDiacritics(LogicalPath);
      s := ConvertString(lowercase(RemoveDiacritics(LogicalPath)));
      FJsonFileName := IncludeTrailingPathDelimiter(Parent.FSyncPath) + MD5Print(MD5String(s)) + '.json';
    end;
    result := FJsonFileName;
  finally
    Flock.UnlockList;
  end;
end;

function TFileItem.GetCacheFilename: String;
var
  p : string;
  s : shortstring;
begin
  FLock.LockList;
  try
    if FCacheFileName = '' then
    begin
      p := IncludeTrailingPathDelimiter(Parent.FSyncPath) + 'covers';
      ForceDirectories(p);
      //s := RemoveDiacritics(LogicalPath);
      s := ConvertString(lowercase(RemoveDiacritics(LogicalPath)));
      FCacheFileName := IncludeTrailingPathDelimiter(p) + MD5Print(MD5String(s)) + '.jpg';
    end;
    result := FCacheFileName
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
        if FileExists(CacheFilename) then
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
        if not FileExists(CacheFilename) then
        begin
          inc(result);
//            FLog.Log('TItemList.GetStampLessCount:Cache File not found:'+CacheFilename);
        end
        else
        begin
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

