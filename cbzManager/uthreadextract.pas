unit uThreadExtract;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics,
  uCbz, Utils.Logger,
  //uRar,
  uDataItem, uDataTypes
{$ifdef Darwin or Linux}
  ,cthreads
{$endif}
  //, sevenzip
  ;

{$define INTERNAL_ZIP}
type
  TCBzErrorFileException = Class(Exception);
  { TThreadExtract }
  TThreadExtract = Class(TThread)
  private
    FSync : TThreadList;
    FWorking : Boolean;
    FPoolData : TThreadDataItem;
    FLog : ILog;
    FResults : TStrings;
    FFilename : string;
    FHasError : Boolean;
    FErrorMsg : String;
    FNbFiles : Integer;
    FProgress : TCbzProgressEvent;
    FProgressID : QWord;
    FOnBadFile : TNotifyEvent;
    FOperations: TImgOperations;
    FOwner : TObject;
    FTmpDir : String;
    function GetWorking: Boolean;
    function GetErrorMsg: String;
    function GetHasError: Boolean;
    function GetNbFiles: Integer;
    procedure GetFileNames(FileNames:TStringList);
  protected
    FCur,
    FMax : Integer;
    FCmd,
    FMSg : String;
    FFiles : TSTringList;
    FTmpFileName : String;
    procedure CopyFileToTemp(const aFileName:String);
    procedure DoProgress;
    procedure DoOnBadFile;
    property TmpDir : String read FTmpDir;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : QWord;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    property HasError : Boolean read GetHasError;
    property ErrorMsg : String read GetErrorMsg;
    property NbFiles : Integer read GetNbFiles;
    property Working : Boolean read GetWorking;
  End;
  {
  TThreadPdfExtract = Class(TThreadExtract)
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;
  }
  TThreadRarExtract = Class(TThreadExtract)
  private
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : QWord;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
  End;

  { TThreadZipExtract }

  TThreadZipExtract = Class(TThreadExtract)
  private
{$ifdef INTERNAL_ZIP}
    Cbz : TCbz;
{$endif}
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : QWord;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
{$ifdef INTERNAL_ZIP}
    procedure Execute; override;
{$endif}
  End;

  TThread7ZipExtract = Class(TThreadExtract)
  private
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : QWord;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
  End;

implementation

uses
  Utils.ZipFile, uWorkerThread, FileUtil,
{$if defined(Darwin) or defined(Linux)}
  unix,
{$else}
  Forms,
{$endif}
  Process,
  Utils.Arrays, Utils.SearchFiles, Utils.Files
{$ifdef INTERNAL_ZIP}
  ,Math
{$endif}
  //, Helper.TStringList, uStrings
  ;


//const
//  MB = 1024 * 1024;

function Unrar:String;
begin
{$if defined(darwin)}
  result := '/usr/local/bin/unrar';
{$elseif Defined(Linux)}
  result := '/usr/bin/unrar';
{$else}
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +  {$ifdef DEBUG} 'Bin-Win\' + {$endif} 'unrar.exe';
{$endif}
end;

function SevenZip:String;
begin
{$if defined(darwin)}
  result := '/usr/local/bin/7z';
{$elseif Defined(Linux)}
  result := '/usr/bin/7z';
{$else}
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + {$ifdef DEBUG} 'Bin-Win\' + {$endif} '7z.exe';
{$endif}
end;

{ TThreadExtract }

constructor TThreadExtract.Create(aOwner : TObject; const Filename: String;
                                  Operations : TImgOperations;
                                  PoolData: TThreadDataItem; Log: ILog;
                                  Results: TStrings;
                                  Progress : TCbzProgressEvent; ProgressID : QWord;
                                  OnBadFile : TNotifyEvent);
var
  outs : String;
{$ifdef Mswindows}
  function _RunCommand(const cmdline:string;out outputstring:string):boolean; deprecated;
  var
    p : TProcess;
    exitstatus : integer;
    ErrorString : String;
  begin
    p:=TProcess.create(nil);
    p.CommandLine := cmdline;
    p.ShowWindow:=swoHIDE;
    p.Options:=[poWaitOnExit];
    p.Execute;
  end;
{$endif}
begin
  FWorking := True;
  FOperations := Operations;
  FSync := TThreadList.Create;
  FOwner := aOwner;
  FOnBadFile := OnBadFile;
  FHasError := False;
  FFilename := Filename;
  FPoolData := PoolData;
  FResults := Results;
  FLog := Log;
  FProgress := Progress;
  FProgressID := ProgressID;
  FFiles := TStringList.Create;
  FFiles.Sorted := True;

{$ifdef INTERNAL_ZIP}
  if not (self is TThreadZipExtract) then
  begin
{$endif}
    FMax := 1;
    FCur := 0;
    FMsg := 'Processing file : ' + FFilename;
    Synchronize(@DoProgress);
    try
      // extract
{$if defined(Darwin) or defined(Linux)}
      fpSystem(FCmd);
{$else}
      _RunCommand(FCmd, outs);
{$endif}
      // get files
      GetFileNames(FFiles);
      FNbFiles := FFiles.Count;

      if FFiles.Count = 0 then
      //  raise Exception.Create(FormatDateTime('hh:nn:ss' ,now) +
      //                         ' Cannot process ' + FFileName + ' no files found.');
      FHasError := True;
    Except
      FFiles.Free;
      FHasError := True;
      DeleteDirectory(FTmpDir, False);
//{$ifdef Darwin or Linux}
//      fpsystem('rm -Rf ' + FTmpDir);
//{$endif}
      raise;
    end;
{$ifdef INTERNAL_ZIP}
  end;
{$endif}
  ;
  inherited Create(False);
end;

destructor TThreadExtract.Destroy;
begin
  FSync.Free;
  FFiles.Free;
//{$ifdef Darwin or Linux}
//  fpsystem('rm -Rf ' + FTmpDir);
//{$endif}
  DeleteDirectory(FTmpDir, False);
  DeleteFile(FTmpFileName);
  inherited;
end;

procedure TThreadExtract.Execute;
var
  i : integer;
  ar : TIntArray;
begin
  SetLength(ar, 0);
  try
    while not Terminated do
    begin
      try
        if not FHasError then
        begin
          FMax := FFiles.Count;
          FCur := 0;
          FMsg := '(Writing ' + TCbz.CleanFilename(ExtractFilename(FFilename)) + ')';
          if Assigned(FProgress) then
            Synchronize(@DoProgress);

          for i := 0 to FFiles.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.ClearLists;
              Exit;
            end;

            FPoolData.AddItem2(nil, ar, FOperations, dtImage, FIF_UNKNOWN, FFiles[i]);
          end;
        end;

        Terminate;
      except
        on e: Exception do
        begin
          Terminate;
          FHasError := True;
          FErrorMsg := e.message;
          FResults.Add('Extract ' + 'Error extracting ' + ' : ' + FFilename + ' ' + ErrorMsg);

          Flog.Log(ClassName + ' Error :  ' + e.Message);
          Synchronize(@DoOnBadFile);
          TCbzWorkerThread(FOwner).Cancel(nil);
          exit;
        end;
      end;
    end;
  finally
    FWorking := False;
  end;
end;

function TThreadExtract.GetErrorMsg: String;
begin
  FSync.LockList;
  try
    result := FErrorMsg;
  finally
    FSync.UnlockList;
  end;
end;

function TThreadExtract.GetHasError: Boolean;
begin
  FSync.LockList;
  try
    result := FHasError;
  finally
    FSync.UnlockList;
  end;
end;

function TThreadExtract.GetNbFiles: Integer;
begin
  FSync.LockList;
  try
    result := FNbFiles;
  finally
    FSync.UnlockList;
  end;
end;

function TThreadExtract.GetWorking: Boolean;
begin
  FSync.LockList;
  try
    result := FWorking;
  finally
    FSync.UnlockList;
  end;
end;

procedure TThreadExtract.DoProgress;
begin
  FProgress(Self, FProgressID, FCur, FMax, FMsg);
end;

procedure TThreadExtract.DoOnBadFile;
begin
  if Assigned(FOnBadFile) then
    FOnBadFile(Self);
end;

procedure TThreadExtract.GetFileNames(FileNames:TStringList);
begin
  GetFiles(FTmpDir, AllowedMasks, FileNames);
end;

procedure TThreadExtract.CopyFileToTemp(const aFileName: String);
begin
  FTmpFileName := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(GetThreadID)) + IntToStr(QWord(GetTickCount64)));
  CopyFile(aFileName, FTmpFileName);
end;


{ TThreadRarExtract }

constructor TThreadRarExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress : TCbzProgressEvent; ProgressID : QWord;
                                     OnBadFile : TNotifyEvent);
begin
  CopyFileToTemp(FileName);
  FTmpDir := GetTempDir + 'fld' + ExtractFileName(FTmpFileName);
  ForceDirectories(FTmpDir);
{$ifdef Darwin or Linux}
  FCmd := Format('%s e -y ''%s'' ''%s''', [Unrar, FtmpFileName, FTmpDir]);
{$else}
  FCmd := Format('%s e -y %s %s', [Unrar, FtmpFileName, FTmpDir]);
{$endif}
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;

destructor TThreadRarExtract.Destroy;
begin
  inherited;
end;

{ TThread7ZipExtract }

constructor TThread7ZipExtract.Create(aOwner: TObject; const Filename: String;
  Operations: TImgOperations; PoolData: TThreadDataItem; Log: ILog;
  Results: TStrings; Progress: TCbzProgressEvent; ProgressID: QWord;
  OnBadFile: TNotifyEvent);
begin
  CopyFileToTemp(FileName);
  FTmpDir := GetTempDir + 'fld' + ExtractFileName(FTmpFileName);
  ForceDirectories(FTmpDir);
{$ifdef Darwin or Linux}
  FCmd := Format('%s e -y ''%s'' ''%s''', [Unrar, FtmpFileName, FTmpDir]);
{$else}
  FCmd := Format('%s e -y %s %s', [Unrar, FtmpFileName, FTmpDir]);
{$endif}
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;

destructor TThread7ZipExtract.Destroy;
begin
  inherited;
end;

{ TThreadZipExtract }

{$ifndef INTERNAL_ZIP}
constructor TThreadZipExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress: TCbzProgressEvent; ProgressID: QWord;
                                     OnBadFile : TNotifyEvent);
begin
  CopyFileToTemp(FileName);
  FTmpDir := GetTempDir + 'fld' + ExtractFileName(FTmpFileName);
  ForceDirectories(FTmpDir);
  FCmd := Format('unzip -o -j -qq %s -d ''%s''', [FTmpFileName, FTmpDir]);
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;
{$endif}

destructor TThreadZipExtract.Destroy;
begin
{$ifdef INTERNAL_ZIP}
  Cbz.Free;
{$endif}
  inherited;
end;

{$ifdef INTERNAL_ZIP}
constructor TThreadZipExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress: TCbzProgressEvent; ProgressID: QWord;
                                     OnBadFile : TNotifyEvent);
begin
  Cbz := TCbz.Create(Log);
  Cbz.Open(Filename, zmRead);
  FNbFiles := Cbz.FileCount; //AllowedFileCount;
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;


procedure TThreadZipExtract.Execute;
var
  Results : TStringList;
  i : integer;
  Filenames : TStringList; //TNaturalSortStringList;
  MetaFiles : TStringlist;
  f : TStringArray;

  procedure AddBlock(const aFilename : String; Index : Integer;
                     DataType : TDataType = dtImage);
  var
    ms : TMemoryStream;
    s : String;
    ar : TIntArray;
  begin
    begin
      if Index >= 0 then
        ms := Cbz.GetFileStream(Index)
      else
        ms := Cbz.GetFileStream(aFileName);

      //ms := TMemoryStream.Create;
      //ms.CopyFrom(st, info.UncompressedSize);
      s := ExtractFileName(aFilename);
      FPoolData.AddItem2(ms, ar, FOperations, DataType, FIF_UNKNOWN, s);
      Sleep(50);
    end;
  end;
begin
  try
    while not Terminated do
    begin
      Results := TStringList.Create;
      try
        try
          // test
          FMax := Cbz.FileCount-1;
          for i := 0 to Cbz.FileCount -1 do
          begin
            FMsg := 'Testing' + ExtractFileName(FFilename) + ' (' +
                     ExtractFilename(Cbz.Filenames[i].Replace('/', '\')) + ')';

            if Assigned(FProgress) then
            begin
              FCur := i;
              Synchronize(@DoProgress);
            end;

            if not Cbz.TestFile(i) then
              raise TCBzErrorFileException.Create(FormatDateTime('hh:nn:ss' ,now) +
                                                  ' Cannot process ' + FFileName + ' because it has errors.');

            if Terminated then
            begin
              FPoolData.ClearLists;
              Cbz.Close;
              Exit;
            end;

            Sleep(50);
          end;
        finally
          Results.free;
        end;

        // extract
        Filenames := TStringlist.Create; //TNaturalSortStringList.Create;
        MetaFiles := TStringlist.Create;
        try
          f := Cbz.GetFileNames;
          for i := 0 to length(f) - 1 do
            //if Tcbz.IsMetaFile(f[i]) then
            //  MetaFiles.AddObject(f[i], TObject(i))
            //else
            if Tcbz.AllowedFile(f[i]) then
              Filenames.AddObject(f[i], TObject(-1))
            else
            if f[i].ToLower.EndsWith('zip') or
               f[i].ToLower.EndsWith('rar') or
               f[i].ToLower.EndsWith('cbz') or
               f[i].ToLower.EndsWith('cbr') then
            begin
              cbz.Extract(f[i], ExtractFilePath(FFilename));
            end;

          Filenames.Sort;
          FMax := Max(0, FNbfiles - 1);
          FMsg := '(Writing : ' + TCbz.CleanFilename(ExtractFilename(FFilename)) + ')';
          FCur := 0;
          if Assigned(FProgress) then
            Synchronize(@DoProgress);

          for i := 0 to Filenames.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.ClearLists;
              Cbz.Close;
              Exit;
            end;

            AddBlock(Filenames[i], -1);
          end;
          {
          for i := 0 to MetaFiles.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.ClearLists;
              Cbz.Close;
              Exit;
            end;

            AddBlock(MetaFiles[i], Integer(MetaFiles.Objects[i]), dtMeta);
          end;
          }
        finally
          MetaFiles.Free;
          Filenames.Free;
        end;
        Cbz.Close;
        if NbFiles = 0 then
          FileToTrash(FFilename);
        Terminate;
      except
        on e: Exception do
        begin
          Terminate;
          FHasError := True;
          FErrorMsg := e.message;
          Flog.Log('TThreadZipExtract Error :  ' + e.Message);
          FResults.Add(FErrorMsg);
          Synchronize(@DoOnBadFile);
          TCbzWorkerThread(FOwner).Cancel(nil);
          exit;
        end;
      end;
    end;
  finally
    FWorking := False;
  end;
end;
{$endif}

{ TThreadPdfExtract }
{
constructor TThreadPdfExtract.Create(aOwner : TObject; const Filename : String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem;
                                     Log: ILog; Results : TStrings;
                                     Progress : TCbzProgressEvent; ProgressID : Cardinal;
                                     OnBadFile : TNotifyEvent);
var
  Doc : TgtPDFDocument;
begin
  Doc := TgtPDFDocument.Create(nil);
  try
    Doc.LoadFromFile(Filename);
    FNbFiles := Doc.PageCount;
  finally
    Doc.Free;
  end;
  inherited;
end;

destructor TThreadPdfExtract.Destroy;
begin
  inherited;
end;

procedure TThreadPdfExtract.Execute;
var
  i : integer;
//  ms : TMemoryStream;
//  aDoc : TgtPDFDocument;
begin
  try
    while not Terminated  do
    try
      if Assigned(FProgress) then
        Synchronize(procedure begin
                      FProgress(Self, FProgressID, 0, FNbfiles - 1, '(' + str_writing +
                                TCbz.CleanFilename(ExtractFilename(FFilename)) + ')');
                    end);

      for i := 1 to NbFiles do
      begin
        if Terminated then
        begin
          FPoolData.ClearLists;
          Exit;
        end;

        Flog.Log('TThreadPdfConvert Generate Pdf block : ' + IntToStr(i));
        FPoolData.AddItem(nil, [], [opConvert], dtPdf, FIF_UNKNOWN, FFilename);
        Sleep(50);
        {
        aDoc := TgtPDFDocument.Create(nil);
        try
          ms := TMemoryStream.Create;
          FPDFDocument.ExtractPagesTo(aDoc, IntTostr(i));
          aDoc.SaveToStream(ms);
          ms.Position := 0;
          Flog.Log('TThreadPdfConvert Adding image : ' + IntTostr(i) + '.pdf');
          FPoolData.AddItem(ms, [], [opConvert], dtPdf);
          Sleep(50);
        finally
          aDoc.Free;
        end;
        }
      end;
//      FPDFDocument.Reset;
      Terminate;
    except
      on e: Exception do
      begin
        Terminate;
        FHasError := True;
        FErrorMsg := e.message;
        Flog.Log('TThreadPdfConvert Error :  ' + e.Message);
        Synchronize(procedure begin
                      FResults.Add(ErrorMsg);
                      if Assigned(FOnBadFile) then
                        FOnBadFile(Self);
                    end);
        TCbzWorkerThread(FOwner).Cancel(nil);
        exit;
      end;
    end;
  finally
    FWorking := False;
  end;
end;
}


end.
