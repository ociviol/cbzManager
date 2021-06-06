unit uThreadExtract;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics,
  uCbz, Utils.Logger,
  Utils.NaturalSortStringList,
  //uRar,
  uDataItem, uDataTypes
{$if defined(Darwin) or defined(Linux)}
  ,cthreads
{$endif}
  //, sevenzip
  ;

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
  (*
  TThreadZipExtract = Class(TThreadExtract)
  private
    Cbz : TCbz;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TCbzProgressEvent; ProgressID : QWord;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;
  *)
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
  Utils.Arrays, Utils.SearchFiles, Utils.Files, Math;


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

function CleanFileName(const aFileName : String):String;
  function ReplaceAccents(const aStr: String): string;
  var
    i:integer;
    Str: ansistring;
  begin
    Str:=UTF8ToANSI(aStr);
    for i:=1 to length(Str) do
      if aStr[i]>#127 then Str[i]:='z';
    result:=ANSITOUTF8(Str);
  end;

begin
  result := StringReplace(aFileName, ' ', '', [rfReplaceAll]);
  result := StringReplace(result, '''', '', [rfReplaceAll]);
  result := StringReplace(result, '"', '', [rfReplaceAll]);
  result := ReplaceAccents(Result);
end;

{ TThreadExtract }

constructor TThreadExtract.Create(aOwner : TObject; const Filename: String;
                                  Operations : TImgOperations;
                                  PoolData: TThreadDataItem; Log: ILog;
                                  Results: TStrings;
                                  Progress : TCbzProgressEvent; ProgressID : QWord;
                                  OnBadFile : TNotifyEvent);
{$ifdef Mswindows}
  procedure _RunCommand(const cmdline:string);
  var
    p : TProcess;
  begin
    p:=TProcess.create(nil);
    try
      p.CommandLine := cmdline;
      p.ShowWindow:=swoHIDE;
      p.Options:=[poWaitOnExit];
      p.Execute;
      if p.ExitCode <> 0 then
        raise Exception.Create('Error while extracting file "' + ExtractFilename(FFilename) + '"');
    finally
      P.Free;
    end;
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
  FFiles.SortStyle:=sslUser;
  FFiles.Sorted := False;

  FMax := 1;
  FCur := 0;
  FMsg := 'Processing file : ' + ExtractFileName(FFilename);
  Synchronize(@DoProgress);
  try
    // extract
    FLog.Log('Running : ' + FCmd);
{$if defined(Darwin) or defined(Linux)}
    fpSystem(FCmd);
{$else}
    _RunCommand(FCmd);
{$endif}
    // get files
    GetFileNames(FFiles);
    FLog.Log('Found : ' + IntToStr(FFiles.Count) + ' Files');
    FNbFiles := FFiles.Count;

    if FFiles.Count = 0 then
      FHasError := True;
  Except
    FFiles.Free;
    FHasError := True;
    if FTmpDir <> '' then
      DeleteDirectory(FTmpDir, False);
    raise;
  end;

  inherited Create(False);
end;

destructor TThreadExtract.Destroy;
begin
  FSync.Free;
  FFiles.Free;
//{$ifdef Darwin or Linux}
//  fpsystem('rm -Rf ' + FTmpDir);
//{$endif}
  if FTmpDir <> '' then
    DeleteDirectory(FTmpDir, False);
  DeleteFile(FTmpFileName);
  inherited;
end;

procedure TThreadExtract.Execute;
var
  i : integer;
  ar : TIntArray;
  tm : TMemoryStream;
begin
  SetLength(ar, 0);
  FNbFiles := 0;
  try
    while not Terminated do
    begin
      try
        if not FHasError then
        begin
          FMax := FFiles.Count;

          for i := 0 to FFiles.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.Clear;
              Exit;
            end;

            if Tcbz.AllowedFile(FFiles[i]) then
            begin
              if (lowercase(ExtractFileExt(FFiles[i])) = '.xml') then
              begin
                tm := TMemoryStream.Create;
                tm.LoadFromFile(FFiles[i]);
                FPoolData.AddItem(tm, FNbFiles, ar, FOperations, dtMeta, FIF_UNKNOWN, ExtractFileName(FFiles[i]));
              end
              else
                FPoolData.AddItem(nil, FNbFiles, ar, FOperations, dtImage, FIF_UNKNOWN, FFiles[i]);

              FCur := i;
              FMsg := '(' + ExtractFileName(FFilename) + ') Loading images ...';
              Synchronize(@DoProgress);
              inc(FNbFiles);
              Sleep(20);
            end;
          end;
        end;

        FCur := 0;
        FMsg := 'Starting conversion ...';
        Synchronize(@DoProgress);

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
var
  t : TNaturalSortStringList;
  i : integer;
  meta : string;
begin
  GetFiles(FTmpDir, AllowedMasks, FileNames);

  t := TNaturalSortStringList.Create;
  try
    t.Assign(Filenames);
    t.Sort;
    meta := '';
    for i := t.count - 1 downto 0 do
      if lowercase(ExtractFileExt(t[i])) = '.xml' then
      begin
        meta := t[i];
        t.Delete(i);
      end;

    if (meta <> '') then
      t.add(meta);

    Filenames.Assign(t);
  finally
    t.Free;
  end;
end;

procedure TThreadExtract.CopyFileToTemp(const aFileName: String);
begin
  FTmpFileName := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(GetThreadID)) +
                                  CleanFileName(ExtractFileName(aFileName)));
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
  FTmpDir := GetTempDir + 'fld' + CleanFileName(ExtractFileName(Filename));
  ForceDirectories(FTmpDir);
{$if defined(Darwin) or defined(Linux)}
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
  FTmpDir := GetTempDir + 'fld' + CleanFileName(ExtractFileName(Filename));
  ForceDirectories(FTmpDir);
{$if defined(Darwin) or defined(Linux)}
  FCmd := Format('%s e -y ''%s'' -o''%s''', [SevenZip, FtmpFileName, FTmpDir]);
{$else}
  FCmd := Format('%s e -y %s -o%s', [SevenZip, FtmpFileName, FTmpDir]);
{$endif}
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;

destructor TThread7ZipExtract.Destroy;
begin
  inherited;
end;

{ TThreadZipExtract }

(*
destructor TThreadZipExtract.Destroy;
begin
  Cbz.Free;
  inherited;
end;

constructor TThreadZipExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress: TCbzProgressEvent; ProgressID: QWord;
                                     OnBadFile : TNotifyEvent);
begin
  FTmpDir := '';
  Cbz := TCbz.Create(Log);
  Cbz.Open(Filename, zmRead);
  FNbFiles := 0;
  inherited Create(aOwner, Filename, Operations, PoolData, Log,
                   Results, Progress, ProgressID, OnBadFile);
end;


procedure TThreadZipExtract.Execute;
var
  Results : TStringList;
  i : integer;
  Filenames : TNaturalSortStringList;

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

      SetLength(ar, 0);
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
          Filenames := TNaturalSortStringList.Create;
          try
            // test
            FMax := Cbz.FileCount-1;
            for i := 0 to Cbz.FileCount -1 do
            begin
              if Tcbz.AllowedFile(cbz.FileNames[i]) then
              begin
                FMsg := 'Testing : ' + ExtractFileName(FFilename) + ' (' +
                         ExtractFilename(Cbz.Filenames[i].Replace('/', '\')) + ')';

                if Assigned(FProgress) then
                begin
                  FCur := i;
                  Synchronize(@DoProgress);
                end;

                if not Cbz.TestFile(i) then
                  raise TCBzErrorFileException.Create(FormatDateTime('hh:nn:ss' ,now) +
                                                    ' Cannot process ' + FFileName + ' because it has errors.');
                Filenames.AddObject(cbz.FileNames[i], TObject(pointer(i)));
              end;

              if Terminated then
              begin
                FPoolData.ClearLists;
                Cbz.Close;
                Exit;
              end;

              Sleep(10);
            end;
          finally
            Results.free;
            FMax := 0;
            FCur := 0;
            if Assigned(FProgress) then
               Synchronize(@DoProgress);
          end;

          FNbFiles := Cbz.AllowedFileCount;
          FMax := Max(0, FNbfiles - 1);
          FMsg := '(Writing : ' + TCbz.CleanFilename(ExtractFilename(FFilename)) + ')';
          FCur := 0;

          if Assigned(FProgress) then
            Synchronize(@DoProgress);

          Filenames.Sort;
          for i := 0 to Filenames.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.ClearLists;
              Cbz.Close;
              Exit;
            end;

            AddBlock(Filenames[i], Integer(pointer(Filenames.Objects[i])));
            Sleep(20);
          end;
        finally
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
*)

{ TThreadPdfExtract }
(*
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
*)


end.
