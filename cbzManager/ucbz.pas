unit uCbz;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  Utils.ZipFile,
  Utils.Logger, Utils.Graphics,
  uDataTypes,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Graphics,
  Utils.Arrays, uDataItem;

const
  CbzComment = 'Created using cbzManager';
  RarSign : array[0..3] of Byte = ($52, $61, $72, $21);
  ZipSign : array[0..3] of Byte = ($50, $4B, 3, 4);
  SZipSign : array[0..1] of Byte = (byte('7'), byte('z'));
  PdfSign : array[0..3] of AnsiChar = ('%', 'P', 'D', 'F');

  AllowedMasks : array[0..5] of string = ('*.jpg', '*.jpeg', '*.png', '*.webp', '*.jp2', '*.gif');
  AllowedExts  : array[0..5] of string = ('.jpg', '.jpeg', '.png', '.webp', '.jp2', '.gif');
  AllowedImgs  : array[0..5] of string = ('.jpg', '.jpeg', '.png', '.webp', '.jp2', '.gif');

{$if defined(Darwin) or defined(Linux)}
  CS_CONFIG_PATH = '.config/cbzManager';
  CS_CONFIG_JSON = '/config.json';
{$else}
  CS_CONFIG_PATH = 'cbzManager';
{$endif}

type

  { TUndoObject }

  TUndoObject = Class
  private
    FIndexes : TIntArray;
    FOperation : TOperation;
    FStreams : TStreamArray;
  public
    constructor Create(Indexes : TIntArray;
                       Operation : TOperation;
                       Streams : TStreamArray);
    destructor Destroy; override;
    property Operation : TOperation read FOperation;
    property Indexes : TIntArray read FIndexes;
    property Streams : TStreamArray read FStreams;
  End;
  { TCbz }
  TCbz = Class;
  TSaveStampOp = (soAdd, soSuppr, soNone);
  TRewriteOperation = (roContinue, roSkip, roStop);
  TUserData = record
    Indexes : TIntArray;
    SaveStamps : Boolean;
    SaveStampsOp : TSaveStampOp;
    Data : TIntArray;
    Progress : TCbzProgressEvent;
    ProgressID : QWord;
  end;
  TRewriteFunction = function(Index : Integer; UserData : TUserData;
                              var Stream : TMemoryStream;
                              const outz : TCbz):TRewriteOperation of Object;

  TStampThread = Class;
  TCbz = Class(TZipFile)
  private
    FCallBack :TCbzProgressEvent;
    FStampWidth,
    FStampHeight : Integer;
    FCache: TStringlist;
    FStampSync : TThreadList;
    FNotify : TStampReadyEvent;
    FMode : TZipMode;
    FFileNameFormat : String;
    Flog : ILog;
    FStampThread : Array[0..0] of TStampThread;
    FInUndo : Boolean;
    FUndoList : TFPObjectList;
    FWebpQualityFactor : Integer;

    procedure AddUndo(Indexes : TIntArray; Operation : TOperation; Streams : TStreamArray);
    function GetImage(Index: QWord): TBitmap;
    function GetImageCount: Integer;
    function GetStamp(Index: Integer): TBitmap;
    function GetStampCount: Integer;
    procedure SetImage(Index: QWord; AValue: TBitmap);
    procedure StartStampThread;
    procedure StopStampThread;
    procedure ClearCache;
    function DoRotateFunct(Index : Integer; UserData : TUserData; var  Stream : TMemoryStream;
                           const outz : Tcbz):TRewriteOperation;
    function CreateUserData(aIndexes : TIntArray;
                            bSaveStamps : Boolean; aSaveStampsOp : TSaveStampOp;
                            aData : TIntArray; aProgress : TCbzProgressEvent):TUserData;
    function SaveCachedStamps(Ignores : TIntArray; bOp : TSaveStampOp = soNone):TStringList;
    function FinishRewrite(const aName, fname : string):Boolean;
    procedure DoSetImage(Indexes: TIntArray; const Values: TStreamArray;
                         CallBack : TCbzProgressEvent = nil);
    function DoSetImageFunct(Index : Integer; UserData : TUserData;
                              var Stream : TMemoryStream;
                              const outz : Tcbz):TRewriteOperation;
    procedure DoAdd(Streams : TStreamArray; CallBack : TCbzProgressEvent = nil);
    function DoOperation(UserFunction : TRewriteFunction; UserData : TUserData;
                          Operation : TOperation):String;
    function DoDeleteFunct(Index : Integer; UserData : TUserData;
                            var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
    function DoInsertFunct(Index : Integer; UserData : TUserData; var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
    procedure DoFlip(Indexes : TIntArray; Orientation : TFlipDir; CallBack : TCbzProgressEvent = nil);
    function DoInvertFunct(Index : Integer; UserData : TUserData;
                            var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;

    class function ConvertBitmapToStream(const fimg : TBitmap; aFLog : ILog; aWebpQuality : Integer):TMemoryStream;
    function DoFlipFunct(Index : Integer; UserData : TUserData;
                          var  Stream : TMemoryStream;
                          const outz : Tcbz):TRewriteOperation;
  public
    constructor Create(Log : ILog; const aStampWidth : Integer = -1; const aStampHeight : Integer = -1;
                       const WebpQualityFactor : Integer = 75;
                       aNotify : TStampReadyEvent = nil); reintroduce;
    destructor Destroy; override;

    class function GetArcType(Stream : TFileStream):TArcType;
    class function GetArcType(const aFilename : String):TArcType;
    class function GetImageType(const aSrc : TMemoryStream; const aFileName : String = ''):Integer;
    class function ConvertImageToStream(const aSrc : TMemoryStream; aFLog : ILog; aWebpQuality : Integer):TMemoryStream; overload;
    class function ConvertImageToStream(const fimg : String; aFLog : ILog; aWebpQuality : Integer):TMemoryStream; overload;
    class function AllowedFile(const aFilename : String):Boolean;
    class function CleanFilename(const aFilename : String; bForce: Boolean = True):String;
    class function FilenameCleaningConfigFile:String;

    procedure Open(const aFileName: string; OpenMode: TZipMode;
                   StampCache: TStringList = nil; NbCharFileName : Integer = 0); reintroduce;
    procedure Close;
    function IsWebp(Index : Integer):Boolean;

    function AllowedFileCount:Integer;
    procedure ClearUndo;
    function CanUndo:Boolean;
    procedure Add(Streams : TStreamArray; CallBack : TCbzProgressEvent = nil);
    procedure Insert(Streams : TStreamArray; AboveIndex : Integer; CallBack : TCbzProgressEvent = nil);
    procedure Rotate(Indexes : TIntArray; Angle:Integer; CallBack : TCbzProgressEvent = nil);
    procedure VerticalFlip(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil);
    procedure HorizontalFlip(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil);
    function Delete(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil):String;
    function TestFile(Index : Integer):Boolean;
    function GetNextFileName:String;
    procedure Undo(CallBack : TCbzProgressEvent);
    function RewriteManga(CallBack : TCbzProgressEvent = nil):String;
    procedure Invert(Index1, Index2 : Integer; CallBack : TCbzProgressEvent = nil);
    function GenerateStamp(Index : Integer):Tbitmap; overload;
    function GenerateStamp(Index, aStampWidth, aStampHeight : Integer):Tbitmap; overload;

    property ImageCount:Integer read GetImageCount;
    property Progress : TCbzProgressEvent read FCallBack write FCallBack;
    property Mode: TZipMode read FMode;
    property Image[Index:QWord]:TBitmap read GetImage write SetImage;
    property Stamp[Index:Integer]:TBitmap read GetStamp;
    property StampCount : Integer read GetStampCount;
    property Cache: TStringList read FCache;
  end;

  { TStampThread }

  TStampThread = Class(TThread)
  private
    FCur : integer;
    FProgressID : QWord;
    FCbz : TCbz;
    FNotify : TStampReadyEvent;
    FStampSync : TThreadList;
    MakeStampIndex : Integer;
    MakeStampResult : Boolean;
    procedure DoMakeStamp;
    function GetCount:Integer;
    procedure DoProgress;
  public
    constructor Create(aCbz : TCBZ; aStampSync : TThreadList; aProgressID : QWord; aNotify : TStampReadyEvent); reintroduce;
    procedure StopThread;
    procedure Execute; override;
    property ProgressID : QWord read FProgressID;
  End;

  { TCleanFilename }

  TCleanFilename = Class
  private
    FRemoveDots,
    FRemoveDashes,
    FRemoveUnderscores,
    FRemoveBetweenParent : Boolean;
    FWordList : TStringlist;
  public
    constructor Create;
    destructor Destroy; override;
    property RemoveDots : Boolean read FRemoveDots;
    property RemoveDashes : Boolean read FRemoveDashes;
    property RemoveUnderscores : Boolean read FRemoveUnderscores;
    property RemoveBetweenParent : Boolean read FRemoveBetweenParent;
    property WordList : TStringlist read FWordlist;
  End;

procedure FileToTrash(const aFileName : String);

implementation

uses
  Webp, Utils.SoftwareVersion, StrUtils,
{$if defined(Linux) or Defined(Darwin)}
  unix,
{$else}
  process, Forms,
{$endif}
  Utils.Files, zstream, uXmldoc;


procedure FileToTrash(const aFileName: String);
begin
  DeleteFile(aFileName);
end;

{ TUndoObject }

constructor TUndoObject.Create(Indexes : TIntArray;
                               Operation : TOperation;
                               Streams : TStreamArray);
begin
  FIndexes := Indexes;
  FStreams := Streams;
  FOperation := Operation;
  inherited Create;
end;

destructor TUndoObject.Destroy;
var
  i : integer;
begin
  for I := Low(FStreams) to High(FStreams) do
    if Assigned(FStreams[i]) then
      try
        FStreams[i].Free;
        // this is ugly but during undo some streams are freed
        // and I still have to find a way to update the undo object arrays
        // to know the object has been freed, for now we swallow the exception
      except
      end;
  inherited;
end;

{ TCleanFilename }

constructor TCleanFilename.Create;
var
  Node : TXmlElement;
  i : integer;
begin
  inherited;
  FWordList := TStringlist.Create;
  {
  FRemoveDots := True;
  FWordList.Add('-');
  FWordList.Add('BD FR');
  FRemoveUnderscores := True;
  FRemoveBetweenParent := True;
  }

  if FileExists(TCbz.FilenameCleaningConfigFile) then
    with TXmlDoc.Create do
    try
      LoadFromFile(TCbz.FilenameCleaningConfigFile);
      with DocumentElement.GetNode('Config') do
      begin
        FRemoveDots := GetAttributeBool('RemoveDots', True);
        FRemoveDashes := GetAttributeBool('RemoveDashes', True);
        if FRemoveDashes then
          FWordList.Add('-');
        FRemoveUnderscores := GetAttributeBool('Value', True);
        FRemoveBetweenParent := GetAttributeBool('Value', True);
        Node := GetNode('Keywords');
        for i := 0 to Node.NbElements - 1 do
          FWordList.Add(Node.Elements[i].Text);
      end;
    finally
      Free;
    end;
end;

destructor TCleanFilename.Destroy;
begin
  FWordlist.Free;
  inherited Destroy;
end;


{ TCbz }

constructor TCbz.Create(Log: ILog;
                        const aStampWidth: Integer; const aStampHeight: Integer;
                        const WebpQualityFactor : Integer = 75;
                        aNotify: TStampReadyEvent = nil);
begin
  FLog := Log;
  FStampWidth := aStampWidth;
  FStampHeight:= aStampHeight;
  FWebpQualityFactor := WebpQualityFactor;
  FNotify := aNotify;
  FStampSync := TThreadList.Create;
  FCache := TStringlist.Create;
  FUndoList := TFPObjectList.Create;

  inherited Create;
end;

destructor TCbz.Destroy;
begin
  if Mode <> zmClosed then
    Close;
  StopStampThread;
  FCache.Free;
  FStampSync.Free;
  FUndoList.Free;

  //FUndoList.Free;
  FLog.Log(ClassName + ' Destroyed.');
  FLog := nil;

  inherited Destroy;
end;

procedure TCbz.Open(const aFileName: string; OpenMode: TZipMode;
                    StampCache: TStringList; NbCharFileName: Integer);
  procedure RecupSavedStamps;
  var
    i,ind : integer;
  begin
    FStampSync.LockList;
    try
      for i:=0 to StampCache.Count-1 do
      begin
        ind := StrToInt(StampCache[i]);
        if FCache.IndexOf(IntToStr(ind)) < 0 then
          FCache.AddObject(StampCache[i], StampCache.Objects[i]);
      end;
    finally
      FStampSync.UnLockList;
    end;
  end;

begin
  FLog.Log(Format('%s %s "%s"', [ClassName, 'Open', aFilename]));
  if FMode <> zmClosed then
  begin
    ClearUndo;
    Close;
  end;

  FMode := OpenMode;
  FileName := aFileName;

  if (OpenMode in [zmWrite, zmReadWrite]) then
  begin
    FFileNameFormat := Format('%%.%dd', [NbCharFileName]);
    Comment := CbzComment +
        {$if defined(darwin)}
              ' OsX' +
        {$elseif defined(Linux)}
              ' Linux' +
        {$else}
              ' Windows' +
        {$endif}
              ' v' + GetFileVersion;
  end
  else
    FFileNameFormat := Format('%%.%dd', [IntToStr(FileCount).Length]);

  Active := True;

  if FFilename <> FileName then
    FUndoList.Clear;

  if Assigned(FStampThread[0]) then
    StopStampThread;

  ClearCache;
  if Assigned(StampCache) then
    RecupSavedStamps;

  if Assigned(FNotify) then
    StartStampThread;
end;

procedure TCbz.Close;
begin
  if FMode = zmClosed then
    Exit;

  if Assigned(FLog) then
    FLog.Log(Format('%s %s %s', [ClassName, 'Close.', FileName]));
  StopStampThread;
  ClearCache;
//  FData.Clear;

//  FModified := False;
  Active := False;
  FMode := zmClosed;
end;

function TCbz.CreateUserData(aIndexes : TIntArray;
                             bSaveStamps : Boolean; aSaveStampsOp : TSaveStampOp;
                             aData : TIntArray; aProgress : TCbzProgressEvent):TUserData;
begin
  result.Indexes := aIndexes;
  result.SaveStamps := bSaveStamps;
  result.SaveStampsOp := aSaveStampsOp;
  result.Data := aData;
  result.Progress := aProgress;
end;

function TCbz.SaveCachedStamps(Ignores : TIntArray; bOp : TSaveStampOp = soNone):TStringList;
var
  i,j : integer;
begin
  StopStampThread;
  result := TStringList.Create;
  FStampSync.LockList;
  try
    for i := 0 to FCache.Count - 1 do
    begin
      j := StrToInt(FCache[i]);
      if InIntArray(j, Ignores) then
      begin
         Tbitmap(FCache.Objects[i]).Free;
         continue;
      end
      else
      begin
        if (bOp = soSuppr) and GreateThanInIntArray(j, Ignores) then
          result.AddObject(IntToStr(j-length(Ignores)), FCache.Objects[i])
        else
        if (bOp = soAdd) and GreateThanInIntArray(j, Ignores) then
          result.AddObject(IntToStr(j+length(Ignores)), FCache.Objects[i])
        else
          result.AddObject(IntToStr(j), FCache.Objects[i]);
      end;
    end;
  finally
    FCache.Clear;
    FStampSync.UnLockList;
  end;
end;

procedure TCbz.AddUndo(Indexes : TIntArray; Operation : TOperation;
                       Streams : TStreamArray);
begin
  FUndoList.Insert(0, TUndoObject.Create(Indexes, Operation, Streams));
end;

procedure TCbz.DoAdd(Streams : TStreamArray; CallBack : TCbzProgressEvent = nil);
var
  outz : TCbz;
  fname, fn, s : string;
  i : integer;
  ms : TMemoryStream;
  FCache2: TStringList;
  Indexes : TIntArray;
  ProgressId : QWord;
  ar : TStreamArray;
begin
  SetLength(Indexes, 0);
  FCache2 := SaveCachedStamps(Indexes, soNone);
  try
    ProgressId := GetTickCount64;
    fname := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(ThreadID)) + IntToStr(QWord(GetTickCount64)));
    outz := TCbz.Create(FLog);
    try
      // create undo later
      if not FInUndo then
      begin
        for i := 0 to Length(Streams) do
        begin
          SetLength(Indexes, Length(Indexes) + 1);
          Indexes[i] := FileCount + i;
        end;
        SetLength(ar, 0);
        AddUndo(Indexes, opAdd, ar);
      end;

      outz.Open(fname, zmWrite, nil, IntToStr(FileCount + Length(Streams)).Length);

      for i := 0 to FileCount - 1 do
      begin
        fn := outz.GetNextFilename;

        ms := GetFileStream(i);
        try
           outz.AppendStream(ms, fn, now, zstream.clnone);
        finally
          ms.Free;
        end;

        if Assigned(CallBack) then
          CallBack(Self, ProgressID, i, FileCount + Length(Streams) -1, 'Rewriting file :' + FFilename);
      end;

      for i := 0 to Length(Streams) - 1 do
      begin
        fn := Format(FFileNameFormat + '%s', [outz.FileCount + 1, '.webp']);
        outz.AppendStream(Streams[i], fn, now, zstream.clNone);

        if Assigned(CallBack) then
          CallBack(Self, ProgressID, FileCount+i-1, FileCount + Length(Streams) -1, 'Rewriting file :' + FFilename);
      end;

      if Assigned(CallBack) then
        CallBack(Self, ProgressID, 0, 0);

      s := TCbz.CleanFilename(FileName);
      outz.Close;
      Close;
      FinishRewrite(s, fname);
      Open(FFilename, zmRead, FCache2);
    finally
      outz.Free;
    end;
  finally
    FCache2.Free;
  end;
end;

function TCbz.DoOperation(UserFunction : TRewriteFunction; UserData : TUserData;
                          Operation : TOperation):String;
var
  outz : TCbz;
  s, fname, fn, ext : string;
  i : integer;
  ms : TMemoryStream;
  cnt : Integer;
  FCache2: TStringList;
  StreamLst : Array of TMemoryStream;
  Indx : TIntArray;
begin
  FCache2 := nil;
  if UserData.SaveStamps then
    FCache2 := SaveCachedStamps(UserData.Indexes, UserData.SaveStampsOp);

  try
    UserData.ProgressID := GetTickCount64;
    fname := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(ThreadID)) + IntToStr(QWord(GetTickCount64)));
    outz := TCbz.Create(FLog);
    try
      // create undo
      if not FInUndo then
      begin
        SetLength(StreamLst, 0);
        for i := Low(UserData.Indexes) to High(UserData.Indexes) do
        begin
          if ((UserData.Indexes[i] < FileCount) and
             IsWebP(UserData.Indexes[i])) and not (Operation = opAdd) then
          begin
            SetLength(StreamLst, Length(StreamLst)+1);
            StreamLst[i] := GetFileStream(UserData.Indexes[i]);
          end;
        end;

        if Operation = opAdd then
        begin
          SetLength(Indx, Length(UserData.Data));
          for i := Low(Indx) to High(Indx) do
            Indx[i] := UserData.Indexes[0] + i;
          AddUndo(Indx, Operation, StreamLst);
        end
        else
        if (length(StreamLst) > 0) then
          AddUndo(UserData.Indexes, Operation, StreamLst);
      end;

      case Operation of
        opAdd:    outz.Open(fname, zmWrite, nil, IntToStr(FileCount + Length(UserData.Indexes)).Length);
        opDelete: outz.Open(fname, zmWrite, nil, IntToStr(FileCount - Length(UserData.Indexes)).Length);
        else
          outz.Open(fname, zmWrite, nil, IntToStr(FileCount-1).Length);
      end;

      for i := 0 to FileCount - 1 do
      begin
        ms := nil;
        if UserFunction(i, UserData, ms, outz) = roSkip then
        begin
          if Assigned(UserData.Progress) then
            UserData.Progress(Self, UserData.ProgressID, Outz.FileCount, cnt,
                              'Rewriting file :' + FFilename);
          Continue;
        end;

        Ext := ExtractFileExt(FileNames[i]);
        fn := ChangeFileExt(outz.GetNextFilename, Ext);

        if Assigned(ms) then
        try
          outz.AppendStream(ms, fn, now, zstream.clNone);
        finally
          ms.Free;
        end
        else
        begin
          ms := GetFileStream(i);
          try
            outz.AppendStream(ms, fn, now, zstream.clNone);
          finally
            ms.Free;
          end;
        end;

        if Operation = opAdd then
          cnt := FileCount + Length(UserData.Data)
        else
          cnt := FileCount;

        if Assigned(UserData.Progress) then
          UserData.Progress(Self, UserData.ProgressID, Outz.FileCount, cnt,
                            'Rewriting file :' + FFilename);
      end;

      if InIntArray(outz.FileCount, UserData.Indexes) then
        UserFunction(outz.FileCount, UserData, ms, outz);

      if Assigned(UserData.Progress) then
        UserData.Progress(Self, UserData.ProgressID, 0, 0);

      s := TCbz.CleanFilename(FileName);
      outz.Close;
      Close;
      FinishRewrite(s, fname);
      Open(s, zmRead, FCache2);
      result := FFilename;
    finally
      outz.Free;
    end;
  finally
    if UserData.SaveStamps then
      FCache2.Free;
  end;
end;

function TCbz.FinishRewrite(const aName, fname : string):Boolean;
var
  i : integer;
  newf : String;
begin
  result := false;
  FileToTrash(aName);
  newf := aName;
  i := 1;
  while Sysutils.FileExists(aName) do
  begin
    newf := ChangeFileExt(aName, ' (' + inttostr(i)+').cbz');
    inc(i);
  end;
  if not Sysutils.FileExists(aName) then
  begin
    FFilename := newf;
    result := True;
    CopyFile(fname, newf);
    if Sysutils.FileExists(newf) then
      Sysutils.DeleteFile(fname);
  end;
end;

function TCbz.RewriteManga(CallBack : TCbzProgressEvent = nil):String;
var
  outz : TCBz;
  ProgressID : QWord;
  fname, s : string;
  i : integer;
  ms : TMemoryStream;
begin
  FLog.Log(Format('%s %s', [ClassName, 'Rewrite.']));
  StopStampThread;
  ClearCache;
  ProgressID := GetTickCount64;
  fname := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(ThreadID)) + IntToStr(QWord(GetTickCount64)));
  outz := TCBz.Create(FLog);
  try
    outz.Open(fname, zmWrite);

    for i := FileCount - 1 downto 0 do
    begin
      if AllowedFile(FileNames[i]) then
      begin
        ms := GetFileStream(i);
        try
          ms.Position := 0;
          outz.AppendStream(ms, Format(FFileNameFormat + '%s', [outz.FileCount + 1, '.webp']), now, zstream.clnone);
        finally
          ms.free;
        end;
      end;

      if Assigned(CallBack) then
        CallBack(Self, ProgressID, outz.FileCount, FileCount, 'Rewriting Manga file :' + FFilename);
    end;

    if Assigned(CallBack) then
      CallBack(Self, ProgressID, 0, 0);

    s := TCbz.CleanFilename(FileName);
    outz.Close;
    Close;
    FinishRewrite(s, fname);
    Open(s, zmRead);
    result := Filename;
  finally
    outz.Free;
  end;
end;

procedure TCbz.Invert(Index1, Index2 : Integer; CallBack : TCbzProgressEvent = nil);
var
  UserData : TUserData;
  Indexes, ar : TIntArray;
begin
  SetLength(Indexes, 2);
  Indexes[0] := Index1;
  Indexes[1] := Index2;
  SetLength(ar, 2);
  ar[0] := QWord(GetFileStream(Index1));
  ar[1] := QWord(GetFileStream(Index2));
  UserData := CreateUserData(Indexes, False, soAdd, ar, FCallBack);
  DoOperation(@DoInvertFunct, UserData, opReplace);
end;

function TCbz.DoInvertFunct(Index : Integer; UserData : TUserData;
                            var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) then
  begin
    if Index = UserData.Indexes[0] then
      Stream := TMemoryStream(UserData.Data[1])
    else
      Stream := TMemoryStream(UserData.Data[0]);
  end;

  Result := roContinue;
end;

function TCbz.DoRotateFunct(Index : Integer; UserData : TUserData; var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
var
  b : TBitmap;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) then
  begin
    b := Image[Index];
    if Assigned(b) then
    try
      RotateBitmap(b, Integer(UserData.Data[0]));
      Stream := ConvertBitmapToStream(b, FLog, FWebpQualityFactor);
    finally
      b.Free;
    end;
  end;

  Result := roContinue;
end;

{$region 'Inserts'}
function TCbz.DoInsertFunct(Index : Integer; UserData : TUserData; var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
var
  fn : String;
  i : integer;
  ms : TMemoryStream;
  aMax : integer;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) then
  begin
    aMax := outz.FileCount + Length(UserData.Data);
    for I := Low(UserData.Data) to High(UserData.Data) do
    begin
      fn := Format(FFileNameFormat + '%s', [outz.FileCount + 1, '.webp']);
      try
        ms := ConvertImageToStream(TMemoryStream(UserData.Data[i]), FLog, FWebpQualityFactor);
        try
          outz.AppendStream(ms, fn, now, zstream.clNone);

          if Assigned(UserData.Progress) then
            UserData.Progress(Self, UserData.ProgressID, Outz.FileCount,
                              aMax, 'Rewriting file :' + FFilename);
        finally
          ms.Free;
        end;
      finally
        if not FInUndo then
          TMemoryStream(UserData.Data[i]).Free;
      end;
    end;
  end;

  Result := roContinue;
end;
{
procedure TCbz.Insert(Filenames : TStrings; AboveIndex : Integer; CallBack : TProgressEvent = nil);
var
  tmp : TArray<TMemoryStream>;
  i : integer;
begin
  for I := 0 to Filenames.Count - 1 do
  begin
    SetLength(tmp, Length(tmp) + 1);
    tmp[i] := TMemoryStream.Create;
    TMemoryStream(tmp[i]).LoadFromFile(FileNames[i]);
    tmp[i].Position := 0;
  end;
  Insert(tmp, AboveIndex, CallBack);
end;
}
procedure TCbz.Insert(Streams : TStreamArray; AboveIndex : Integer; CallBack : TCbzProgressEvent = nil);
var
  UserData : TUserData;
  Indexes, ar : TIntArray;
  i : integer;
begin
  SetLength(Indexes, 1);
  Indexes[0] := AboveIndex;
  SetLength(ar, length(Streams));
  for i:= low(Streams) to high(Streams) do
    ar[i] := QWord(Streams[i]);
  UserData := CreateUserData(Indexes, False, soAdd, ar, FCallBack);
  DoOperation(@DoInsertFunct, UserData, opAdd);
end;
{$endregion}

procedure TCbz.Add(Streams : TStreamArray; CallBack : TCbzProgressEvent = nil);
begin
  DoAdd(Streams, CallBack);
end;

procedure TCbz.Rotate(Indexes : TIntArray; Angle:Integer; CallBack : TCbzProgressEvent = nil);
var
  UserData : TUserData;
  ar : TIntArray;
begin
  SetLength(ar, 1);
  ar[0] := Qword(Angle);
  FLog.Log(Format('%s %s Angle : %d', [ClassName, 'Rotate', Angle]));
  UserData := CreateUserData(Indexes, True, soNone, ar, CallBack);
  DoOperation(@DoRotateFunct, UserData, opReplace);
end;

function TCbz.DoDeleteFunct(Index : Integer; UserData : TUserData;
                            var  Stream : TMemoryStream;
                            const outz : Tcbz):TRewriteOperation;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) and IsWebp(Index) then
    result := roSkip
  else
    Result := roContinue;
end;

function TCbz.Delete(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil):String;
var
  UserData : TUserData;
  ar : TIntArray;
begin
  SetLength(ar, 0);
  UserData := CreateUserData(Indexes, True, soSuppr, ar, CallBack);
  result := DoOperation(@DoDeleteFunct, UserData, opDelete);
end;

function TCbz.DoFlipFunct(Index : Integer; UserData : TUserData;
                          var  Stream : TMemoryStream;
                          const outz : Tcbz):TRewriteOperation;
var
  b : TBitmap;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) then
  begin
    b := Image[Index];
    if Assigned(b) then
    try
      Flip(b, TFlipDir(UserData.Data[0]));
      Stream := ConvertBitmapToStream(b, FLog, FWebpQualityFactor);
    finally
      b.Free;
    end;
  end;
  Result := roContinue;
end;

procedure TCbz.DoFlip(Indexes : TIntArray; Orientation : TFlipDir; CallBack : TCbzProgressEvent = nil);
var
  UserData : TUserData;
  ar : TIntArray;
begin
  SetLength(ar, 1);
  ar[0] := QWord(Orientation);
  UserData := CreateUserData(Indexes, True, soSuppr, ar, CallBack);
  //UserData.Create(Indexes, True, soNone, [TObject(Orientation)], CallBack);
  DoOperation(@DoFlipFunct, UserData, opReplace);
end;

procedure TCbz.VerticalFlip(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil);
begin
  FLog.Log(Format('%s %s', [ClassName, 'VerticalFlip']));
  DoFlip(Indexes, fdVertical, CallBack);
end;

procedure TCbz.HorizontalFlip(Indexes : TIntArray; CallBack : TCbzProgressEvent = nil);
begin
  FLog.Log(Format('%s %s', [ClassName, 'HorizontalFlip']));
  DoFlip(Indexes, fdHorizontal, CallBack);
end;

function TCbz.TestFile(Index: Integer):Boolean;
begin
  try
    GetFileStream(Index).Free;

    result := True;
  except
    result := false;
  end;
end;

function TCbz.GetNextFileName: String;
begin
  result := Format(FFileNameFormat + '.webp', [FileCount + 1]);
end;

procedure TCbz.Undo(CallBack: TCbzProgressEvent);
begin
  if not CanUndo then Exit;
  FLog.Log(Format('%s %s', [ClassName, 'DoUndo.']));

  FInUndo := True;
  try
    with TUndoObject(FUndoList.Items[0]) do
    begin
      case Operation of
        opAdd      : Self.Delete(Indexes, CallBack);

        opDelete   : if Assigned(Streams) then
                       if Indexes[0] >= FileCount then
                         DoAdd(Streams, CallBack)
                       else
                         Insert(Streams, Indexes[0], CallBack);

        opReplace :  DoSetImage(Indexes, Streams, CallBack);
      end;
      FUndoList.Delete(0);
    end;
  finally
    FInUndo := False;
  end;
end;

function TCbz.GetImageCount: Integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to FileCount - 1 do
    if IsWebP(i) then
      inc(Result);
end;

function _cbzGetTempFileName:String;
begin
  result := '';

  repeat
    result := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(GetThreadID)) + IntToStr(QWord(GetTickCount64)));
              //IncludeTrailingPathDelimiter(FTempPath) +
              //'Cbz' + IntToStr(GetCurrentThreadID) + '.tmp'
  until not SysUtils.FileExists(result);
end;

{$ifdef Mswindows}
procedure CustomRunCommand(const cmdline:string);
var
  p : TProcess;
begin
  p:=TProcess.create(nil);
  try
    p.CommandLine := cmdline;
    p.ShowWindow:=swoHIDE;
    p.Options:=[poWaitOnExit];
    p.Execute;
  finally
    P.Free;
  end;
end;
{$endif}

function TCbz.GetImage(Index: QWord): TBitmap;

  function ConvertImageToBMP(const fimg : String):String;
  var
    cmd : string;
  begin
    FLog.Log(ClassName + '.ConvertImageToBMP : ' +Fimg);

    if Sysutils.FileExists(fimg) then
    begin
      result := ChangeFileExt(fimg, '.bmp');
{$if Defined(Darwin)}
      cmd := '/usr/local/bin/dwebp -mt -quiet -bmp "' + fimg + '" -o "' + result + '"';
      fpsystem(cmd);
{$elseif Defined(Linux)}
      cmd := '/usr/bin/dwebp -mt -quiet -bmp "' + fimg + '" -o "' + result + '"';
      fpsystem(cmd);
{$elseif Defined(MsWindows)}
      cmd := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +  {$ifdef DEBUG} 'Bin-Win\' + {$endif} 'dwebp.exe';
      cmd := cmd + ' -mt -quiet -bmp "' + fimg + '" -o "' + result + '"';
      CustomRunCommand(cmd);
{$endif}
      FLog.Log(ClassName + '.ConvertImageToBMP Executing : ' + cmd);

      if not Sysutils.FileExists(result) then
        result := '';

      if result = '' then
        FLog.Log(ClassName + '.ConvertImageToBMP failed.')
      else
        FLog.Log(ClassName + '.ConvertImageToBMP done : ' + result);
    end
    else
      result := '';
  end;

var
  m : TMemoryStream;
  fin, fout : string;
  wi : TWebpImage;
begin
  result := nil;
  with FStampSync.LockList do
  try
    try
      m := GetFileStream(Index);
      if Assigned(m) then
      try
        wi := TWebpImage.Create(m);
        try
          if InternaldWebpAvail then
             result := wi.GetBitmap;
          //result := WebpToBitmap(m.Memory, m.Size);
          if not Assigned(result) then
          begin
            fin := ChangeFileExt(_cbzGetTempFileName, '.webp');
            m.SaveToFile(fin);
            try
              fout := ConvertImageToBMP(fin);
              if Sysutils.FileExists(fout) then
              try
                result := TBitmap.Create;
  {$ifdef Linux}
                result.PixelFormat := pf24bit;
  {$else}
                result.PixelFormat := pf32bit;
  {$endif}
                result.LoadFromFile(fout);
              finally
                Sysutils.DeleteFile(fout);
              end;
            finally
              Sysutils.DeleteFile(fin);
            end;
          end;
        finally
          wi.free;
        end;
      finally
        m.Free;
      end;
    except
      on e: exception do
      begin
        FLog.Log(Format('%s %s : %s', [ClassName, 'GetImage Error', e.Message]));
        raise;
      end;
    end;
  finally
    FStampSync.UnLockList;
  end;
end;

function AllowedExt(const Ext : String):Boolean; inline;
var
  e : string;
begin
  for e in AllowedExts do
    if e = Ext.ToLower then
      Exit(True);
  Exit(False);
end;

class function TCbz.CleanFilename(const aFilename : String; bForce: Boolean = True):String;
var
  p, f, e : string;
  clconfig : TCleanFilename;

  function wordinlist(const s : string; wordlist : TStringlist):String;
  var
    i : integer;
  begin
    for i := 0 to wordlist.Count - 1 do
      if s.ToLower.Contains(wordlist[i].ToLower) then
        exit(wordlist[i]);
    result := '';
  end;

  function removeword(const s, ss : string):String;
  var
    i : integer;
  begin
    i := pos(ss.ToLower, s.ToLower);
    if i > 0 then
    begin
      result := copy(s, 1, i - 1) + copy(s, i + length(ss), length(s));
      result := result.Trim;
    end;
  end;

  function CleanBDFR(s : string):string;
  var
    ss : string;
    i,j,z : integer;
  begin
    result := s;
    {
    while s.Contains('µ') do
    begin
      i := s.LastIndexOf('µ');
      if i > 0 then
        s := Copy(s, 1, i-1).Trim.TrimRight(['-']).Trim;
    end;
    }
    if clconfig.RemoveDots then
      s := s.Replace('.', ' ');
    if clconfig.RemoveDashes then
      s := s.Replace('-', ' ');
    if clconfig.RemoveUnderscores then
      s := s.Replace('_', ' ');

    begin
      s := ReplaceStr(s, ' -', '');
      s := ReplaceStr(s, '- ', '');
      s := ReplaceStr(s, '-', '');
    end;
    s := ReplaceStr(s, '  ', ' ');

    repeat
      ss := wordinlist(s, clconfig.WordList);
      if ss <> '' then
        s := removeword(s, ss);
    until ss = '';

    if clconfig.RemoveBetweenParent then
    begin
      s := s.Replace('[', '(');
      s := s.Replace(']', ')');
      s := StringReplace(s, '()', '', []).Trim;

      z := 0;
      repeat
        i := s.LastIndexOf(')');
        if i > 0 then
        begin
          j := s.LastIndexOf('(');
          if j >= 0 then
          begin
            ss := copy(s, j+1, i-j+1);
            s := StringReplace(s, ss, '', []).Trim;
          end;
        end;
        inc(z);
      until (i < 0) or (z > 100);
    end;

    for i := 0 to 50 do
    begin
      ss := format('T%d', [i]);
      if pos(ss, s) > 0 then
      begin
        s := s.Replace(ss, IntTostr(i));
        break;
      end
      else
      if pos(ss.ToLower, s) > 0 then
      begin
        s := s.Replace(ss.ToLower, IntTostr(i));
        break;
      end;
    end;

    s := ReplaceStr(s, '  ', ' ');

    if s.Length > 0 then
      result := s;
  end;
begin
  if {CleanFilenamesVar or} bForce then
  begin
    clconfig := TCleanFilename.Create;
    try
      p := ExtractFilePath(aFilename);
      e := ExtractFileExt(aFilename);
      f := ExtractFileName(aFilename);
      f := f.Replace(e, '');
      f := CleanBDFR(CleanBDFR(f));
      result := ifthen(p.Length > 0, IncludeTrailingPathDelimiter(p), '') + f.Trim + '.cbz';
    finally
      clconfig.Free;
    end;
  end
  else
    result := ChangeFileExt(aFilename, '.cbz');
end;

class function TCbz.FilenameCleaningConfigFile: String;
begin
{$if defined(Darwin) or defined(Linux)}
  result := expandfilename('~/') + CS_CONFIG_PATH + '/'
{$else}
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
{$endif}
  + 'FilenameCleaningConfig.xml';
end;

class function TCbz.AllowedFile(const aFilename : String):Boolean;
var
  ext : String;
begin
  if ExtractFilename(aFilename)[1] = '.' then
    Exit(False);

  ext := ExtractFileExt(aFilename);
  result := AllowedExt(ext);
end;

function TCbz.GetStamp(Index: Integer): TBitmap;
var
  ind : integer;
begin
  result := nil;
  if (FStampWidth < 0) or (FStampHeight < 0) then Exit;

  with FStampSync.LockList do
  try
    ind := Fcache.IndexOf(IntToStr(Index));
    if ind >= 0 then
      result := TBitmap(Fcache.Objects[ind]);
  finally
    FStampSync.UnlockList;
  end;
end;

function TCbz.GetStampCount: Integer;
begin
  with FStampSync.LockList do
  try
    result := Fcache.Count;
  finally
    FStampSync.UnlockList;
  end;
end;

function TCbz.DoSetImageFunct(Index : Integer; UserData : TUserData;
                              var Stream : TMemoryStream;
                              const outz : Tcbz):TRewriteOperation;
var
  tmp : TMemoryStream;
  i : integer;
begin
  Stream := nil;
  if InIntArray(Index, UserData.Indexes) then
  begin
    i := PosInIntArray(Index, UserData.Indexes);
    if (i >= 0) and (i < Length(UserData.Indexes)) then
    begin
{$ifdef Darwin}
      if TObject(Pointer(UserData.Data[i])) is TBitmap then
{$else}
      if TObject(UserData.Data[i]) is TBitmap then
{$endif}
      begin
        tmp := TMemoryStream.Create;
        try
{$ifdef Darwin}
          TBitmap(Pointer(USerData.Data[i])).SaveToStream(tmp);
{$else}
          TBitmap(USerData.Data[i]).SaveToStream(tmp);
{$endif}
          tmp.Position := 0;
          Stream := ConvertImageToStream(tmp, FLog, FWebpQualityFactor);
          Stream.Position := 0;
        finally
          tmp.Free;
        end;
      end
      else
      begin
{$ifdef Darwin}
        Stream := TMemoryStream(Pointer(UserData.Data[i]));
{$else}
        Stream := TMemoryStream(UserData.Data[i]);
{$endif}
        UserData.Data[i] := 0;
        Stream.Position := 0;
      end;
    end;
  end;

  Result := roContinue;
end;

procedure TCbz.DoSetImage(Indexes: TIntArray; const Values: TStreamArray;
                          CallBack : TCbzProgressEvent = nil);
var
  UserData : TUserData;
  ar : TIntArray;
  i : integer;
begin
  SetLength(ar, length(Values));
  for i:=low(Values) to high(Values) do
    ar[i] := QWord(Values[i]);

  UserData := CreateUserData(Indexes, True, soNone, ar, FCallBack);
  DoOperation(@DoSetImageFunct, UserData, opReplace);
end;

procedure TCbz.SetImage(Index: QWord; AValue: TBitmap);
var
  UserData : TUserData;
  ind, ar : TIntArray;
begin
  SetLength(ar, 1);
  SetLength(ind, 1);
  ar[0] := QWord(AValue);
  ind[0] := Index;
  UserData := CreateUserData(ind, True, soNone, ar, FCallBack);
  DoOperation(@DoSetImageFunct, UserData, opReplace);
end;

function TCbz.IsWebp(Index : Integer):Boolean;
begin
  result := LowerCase(ExtractFileExt(Filenames[Index])) = '.webp';
end;

function TCbz.AllowedFileCount: Integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to FileCount - 1 do
    if AllowedFile(FileNames[i]) then
      inc(result);
end;

procedure TCbz.ClearUndo;
begin
  FUndoList.Clear;
end;

function TCbz.CanUndo: Boolean;
begin
  result := FUndoList.Count > 0;
end;


procedure TCbz.ClearCache;
var
  i : integer;
begin
  if Assigned(FCache) then
  begin
    for i:=0 to FCache.Count-1 do
      TBitmap(FCache.Objects[i]).Free;
    FCache.Clear;
  end;
end;


procedure TCbz.StartStampThread;
var
  PID : Qword;
  i : integer;
begin
  PId := GetTickCount64;
  for i := low(FStampThread) to High(FStampThread) do
  begin
    if (FStampWidth > 0) and (FStampHeight > 0) then
    begin
      FStampThread[i] := TStampThread.Create(self, FStampSync, Pid, FNotify);
      FLog.Log(Format('%s Starting thumbnail thread ID %s', [ClassName, IntToStr(QWord(FStampThread[i].ThreadID))]));
    end
    else
      FStampThread[i] := nil;
  end;
end;

procedure TCbz.StopStampThread;
var
  z : QWord;
  i : integer;
begin
  for i := low(FStampThread) to High(FStampThread) do
    if Assigned(FStampThread[i]) then
    begin
      FLog.Log(Format('%s Stopping thumbnail thread ID %s', [ClassName, IntToStr(QWord(FStampThread[i].ThreadID))]));
      z := FStampThread[i].ProgressID;
      FStampThread[i].StopThread;
      if Assigned(FNotify) then
        FNotify(z, -1);
      //FreeANdNil(FStampThread[i]);
      FStampThread[i] := nil;
    end;
end;

function TCbz.GenerateStamp(Index: Integer): Tbitmap;
begin
  result := GenerateStamp(Index, FStampWidth, FStampHeight);
end;

function TCbz.GenerateStamp(Index, aStampWidth, aStampHeight : Integer):Tbitmap;
var
  b : TBitmap;
  ratio : extended;
  w, h : integer;

  function ResampleBitmap(const aSrc: TBitmap; DestWidth, DestHeight: Integer):TBitmap;
  begin
    result := TBitmap.Create;
    result.PixelFormat := aSrc.PixelFormat;
    result.SetSize(DestWidth, DestHeight);
    Result.Canvas.AntialiasingMode := amOn;
    Result.Canvas.StretchDraw(Rect(0, 0, DestWidth, DestHeight), aSrc);
  end;
begin
  result := nil;
  try
    b := GetImage(Index);
    if Assigned(b) then
    try
      w := b.Width;
      h := b.Height;
      if (w = 0) and (h = 0) then
        Exit(nil);

      if h > w then
      begin
        ratio := w / h;
        h := aStampHeight;
        w := round(h * ratio);
        while w > aStampWidth do
        begin
          dec(h);
          w := round(h * ratio);
        end;
      end
      else
      begin
        ratio := h / w;
        w := aStampWidth;
        h := round(w * ratio);
        while h > aStampHeight do
        begin
          dec(w);
          h := round(w * ratio);
        end;
      end;
      result := ResampleBitmap(b, w, h);
    finally
      b.Free;
    end;
  except
    on E: Exception do
      FLog.Log(Format('%s %s : %s', [ClassName, 'MakeStamp Error', e.Message]));
  end;
end;

class function TCbz.GetArcType(const aFilename : String):TArcType;
var
  f : TFileStream;
begin
  f := TFileStream.Create(aFilename, fmOpenRead);
  try
    Result := GetArcType(f);
  finally
    f.Free;
  end;
end;

class function TCbz.GetArcType(Stream : TFileStream):TArcType;
var
  b : TBytes;
begin
  SetLength(b, 4);
  try
    Stream.ReadBuffer(b[0], 4);

    if b[0] = RarSign[0] then
    begin
      if (b[1] = RarSign[1]) and
         (b[2] = RarSign[2]) and
         (b[3] = RarSign[3]) then
            Exit(arcRar);
    end
    else
    if (b[0] = ZipSign[0]) and (b[1] = ZipSign[1]) then
        Exit(arcZip)
    else
    if (b[0] = SZipSign[0]) and (b[1] = SZipSign[1]) then
        Exit(arc7Zip)
    else
    if b[0] = Byte(PdfSign[0]) then
    begin
      if (b[1] = Byte(PdfSign[1])) and
         (b[2] = Byte(PdfSign[2])) and
         (b[3] = Byte(PdfSign[3])) then
        Exit(arcPdf);
    end
  except
  end;

  result := arcUnknown;
end;

class function TCbz.GetImageType(const aSrc : TMemoryStream; const aFileName : String = ''):Integer;
var
  p : pbyte;
  s : string;
begin
  if Assigned(aSrc) then
  begin
    p := aSrc.Memory;

    if (p[0] = $FF) and (p[1] = $D8) then
      Exit(FIF_JPEG);

    if (p[0] = $89) and (p[1] = $50) and
       (p[2] = $4E) and (p[3] = $47) and
       (p[4] = $0D) and (p[5] = $0A) and
       (p[6] = $1A) and (p[7] = $0A) then
      Exit(FIF_PNG);

    if (p[0] = $00) and (p[1] = $00) and (p[2] = $00) and
       (p[3] = $0C) and (p[4] = $6A) and (p[5] = $50) and
       (p[6] = $20) and (p[7] = $20) and (p[8] = $0D) and
       (p[9] = $0A) then
      Exit(FIF_JP2);

    if ((p[0] = $47) and (p[1] = $49) and (p[2] = $46) and
        (p[3] = $38) and (p[4] = $37) and (p[5] = $61)) or
       ((p[0] = $47) and (p[1] = $49) and (p[2] = $46) and
        (p[3] = $38) and (p[4] = $39) and (p[5] = $61)) then
      Exit(FIF_GIF);

    if (p[0] = $52) and (p[1] = $49) and (p[2] = $46) and
       (p[3] = $46) and (p[8] = $57) and (p[9] = $45) and
       (p[10] =$42) and (p[11] = $50) then
      Exit(FIF_WEBP);

    if (p[0] = $42) and (p[1] = $4D) then
      Exit(FIF_BMP);
  end
  else
  begin
    // test using FileName
    s := ExtractFileExt(aFileName);
    if s.ToLower = '.webp' then
      Exit(FIF_WEBP);
    if (s.ToLower = '.jpg') or (s.ToLower = '.jpeg') then
      Exit(FIF_JPEG);
    if (s.ToLower = '.jp2') or (s.ToLower = '.jpx') or (s.ToLower = '.j2k') then
      Exit(FIF_JP2);
    if s.ToLower = '.gif' then
      Exit(FIF_GIF);
    if s.ToLower = '.bmp' then
      Exit(FIF_BMP);
    if s.ToLower = '.png' then
      Exit(FIF_PNG);
  end;
  result := FIF_UNKNOWN;
end;

class function TCbz.ConvertImageToStream(const aSrc : TMemoryStream; aFLog : ILog; aWebpQuality : Integer):TMemoryStream;

  function cwebp:String;
  begin
{$if defined(Darwin)}
    result := '/usr/local/bin/cwebp';
{$elseif defined(Linux)}
    result := '/usr/bin/cwebp';
{$else}
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +  {$ifdef DEBUG} 'Bin-Win\' + {$endif} 'cwebp.exe';
{$endif}
  end;

  function MakeFileExt(const Filename : String; aSrc : TMemoryStream):String;
  begin
    case GetImageType(aSrc) of
      FIF_BMP     : Result := ChangeFileExt(Filename, '.bmp');
      FIF_JPEG    : Result := ChangeFileExt(Filename, '.jpg');
      FIF_PNG     : Result := ChangeFileExt(Filename, '.png');
      //FIF_TIFF    : Result := ChangeFileExt(Filename, '.tif');
      FIF_GIF     : Result := ChangeFileExt(Filename, '.gif');
      //FIF_J2K     : Result := ChangeFileExt(Filename, '.j2k');
      FIF_JP2     : Result := ChangeFileExt(Filename, '.jp2');
    end;
  end;

  function ConvertImage(const fimg : String):String;
  var
    cmd : string;
  begin
    aFLog.Log(ClassName + '.ConvertImage : ' +Fimg);

    if Sysutils.FileExists(fimg) then
    begin
      result := ChangeFileExt(fimg, '.webp');
      cmd := cwebp + ' -mt -quiet -q ' + IntToStr(Integer(aWebpQuality)) + ' "' + fimg + '" -o "' + result +'"';
{$if Defined(Darwin) or Defined(Linux)}
      fpsystem(cmd);
{$elseif Defined(MsWindows)}
      CustomRunCommand(cmd);
{$endif}
      aFLog.Log(ClassName + '.ConvertImage Executing : ' + cmd);

      if not Sysutils.FileExists(result) then
        result := '';

      if result = '' then
        aFLog.Log(ClassName + '.ConvertImage failed.')
      else
        aFLog.Log(ClassName + '.ConvertImage done : ' + result);
    end
    else
      result := '';
  end;

  function ExternalConvert(const aSrc, aDest : TMemoryStream):Boolean;
  var
    fin, fout : string;
    retry : integer;
    done : boolean;
  begin
    result := False;
    if SysUtils.FileExists(cwebp) then
    begin
      fin := MakeFileExt(_cbzGetTempFileName, aSrc);
      aSrc.SaveToFile(fin);

      try
        fout := ConvertImage(fin);
        if SysUtils.FileExists(fout) then
        begin
          retry := 0;
          done := false;
          repeat
            try
              aDest.LoadFromFile(fout);
              done := True;
            except
              on e: Exception do
              begin
                inc(retry);
                aFLog.Log('External convert : "' + e.Message + '" retry ' + IntTostr(retry));
                sleep(500);
              end;
            end;
          until (retry > 4) or Done;
          Exit(Done);
        end
        else
          Exit(False);
      finally
        if SysUtils.FileExists(fin) then
        try
          SysUtils.DeleteFile(fin);
        except
        end;
        if SysUtils.FileExists(fout) then
        try
          SysUtils.DeleteFile(fout);
        except
          on e: Exception do
            aFLog.Log('Externale convert : ' + e.Message);
        end;
      end;
    end;
  end;

  function InternalConvert(const aSrc, aDest : TMemoryStream):Boolean;
  var
    wi : TWebpImage;
  begin
    try
      aSrc.Position:=0;
      wi := TWebpImage.Create(aSrc);
      try
        wi.SaveToStream(aDest, aWebpQuality);
        result := true;
      finally
        wi.Free;
      end;
    except
      on e: Exception do
      begin
        aFLog.Log('Internal convert : ' + e.Message);
        result := False;
      end;
    end;
  end;

  procedure ConvertToPng(aSrc : TMemoryStream);
  var
    aPic:TPicture;
    fin : String;
    //fout : string;
  begin
    fin := MakeFileExt(_cbzGetTempFileName, aSrc);
    aSrc.SaveToFile(fin);

    aPic:=TPicture.Create;
    try
      //fout := ChangeFileExt(fin, '.png');
      aPic.LoadFromFile(fin);
      aPic.SaveToStreamWithFileExt(aSrc, 'png');
      //aPic.SaveToFile(fout);
    finally
      aPic.free;
    end;
    //aSrc.LoadFromFile(fout);
    aSrc.Position := 0;
  end;

var
  imgtype : integer;
  done : Boolean;
begin
  done := false;
  result := TMemoryStream.Create;
  try
    imgtype := GetImageType(aSrc);
    if imgtype = FIF_WEBP then
      result.CopyFrom(aSrc, aSrc.Size)
    else
    begin
      if InternalcWebpAvail then
        done := InternalConvert(aSrc, Result);

      if not done then
      begin
        if (imgtype <> FIF_PNG) and (imgtype <> FIF_JPEG) then
          ConvertToPng(aSrc);

        if SysUtils.FileExists(cwebp) then
        begin
          if not ExternalConvert(aSrc, Result) then
          begin
            FreeAndNil(Result);
            raise Exception.Create('Could not convert image.');
          end;
        end
        else
          raise Exception.Create('cwebp not found.');
      end;
    end;

    if Assigned(result) then
      result.Position := 0;
  except
    on e: Exception do
    begin
      if Assigned(result) then
        FreeAndNil(result);
      aFLog.Log(ClassName + '.ConvertImage failed : ' + e.Message);
      raise;
    end;
  end;
end;

class function TCbz.ConvertBitmapToStream(const fimg : TBitmap; aFLog : ILog; aWebpQuality : Integer):TMemoryStream;
var
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    fimg.SaveToStream(ms);
    ms.Position := 0;
    result := ConvertImageToStream(ms, aFLog, aWebpQuality);
  finally
    ms.Free;
  end;
end;

class function TCbz.ConvertImageToStream(const fimg : String; aFLog : ILog; aWebpQuality : Integer):TMemoryStream;
var
  ms : TmemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(fimg);
    ms.Position := 0;
    result := ConvertImageToStream(ms, aFLog, aWebpQuality);
  finally
    ms.Free;
  end;
end;

{ TStampThread }

constructor TStampThread.Create(aCbz: TCBZ; aStampSync: TThreadList;
                                aProgressID : QWord;
                                aNotify: TStampReadyEvent);
begin
  FCbz := aCbz;
  FNotify := aNotify;
  FProgressID := aProgressID;
  FStampSync := aStampSync;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TStampThread.DoMakeStamp;
begin
  if not Terminated then
    with Fcbz do
    begin
      with FStampSync.LockList do
      try
        MakeStampResult := Cache.IndexOf(IntToStr(MakeStampIndex)) >= 0;
        if not MakeStampResult then
        begin
          Cache.AddObject(IntTostr(MakeStampIndex), GenerateStamp(MakeStampIndex));
        end;
      finally
        FStampSync.UnlockList;
      end;
    end;
end;

function TStampThread.GetCount: Integer;
begin
  with FStampSync.LockList do
  try
    result := Count;
  finally
    FStampSync.UnlockList;
  end;
end;

procedure TStampThread.StopThread;
begin
  if not Terminated then
  begin
    Terminate;
    if Suspended then
      Resume;
    //while not Terminated do
    //  Sleep(250);
    //WaitFor;
  end;
end;

procedure TStampThread.DoProgress;
begin
  if not Terminated then
    FNotify(ProgressID, FCur);
end;

procedure TStampThread.Execute;
var
  i,j : integer;
  ImgCount : Integer;
begin
  while not Terminated do
  begin
    with FCbz do
    begin
      ImgCount := FileCount;
      if {(GetCount < ImgCount) and} (ImgCount > 0) then
      begin
        // get all others
        for I := 0 to ImgCount div 2 do //ImgCount - 1 do
        begin
          if not Terminated then
          begin
            MakeStampIndex := i;
            Synchronize(@DoMakeStamp);
            if not Terminated then
            begin
              if not MakeStampResult then
                if Assigned(FNotify) then
                begin
                  FCur := i;
                  Synchronize(@DoProgress);
                  //Sleep(50);
                  Yield;
                end;
            end
            else
              break;
          end;

          j := (ImgCount - 1) - i;
          if (j >= 0) and (j > ImgCount div 2) then
            if not Terminated then
            begin
              MakeStampIndex := j;
              Synchronize(@DoMakeStamp);
              if not Terminated then
              begin
                if not MakeStampResult then
                  if Assigned(FNotify) then
                  begin
                    FCur := j;
                    Synchronize(@DoProgress);
                    //Sleep(50);
                    Yield;
                  end;
              end
              else
                Break;
            end;

          if Terminated then
            break;
        end;
      end
      else
        if not Terminated then
          Suspend;
    end;

    if Assigned(FNotify) then
    begin
      FCur := -1;
      Synchronize(@DoProgress);
    end;

    if not Terminated then
      Terminate;
  end;

end;

end.

