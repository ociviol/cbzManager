unit Utils.Logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, cthreads, Process; //system.generics.collections, system.ioutils;

type
  ILog = interface
  ['{36573377-D6D3-42F0-BD07-5ED2806D392E}']
    procedure Log(const msg : string);
    procedure SetActive(const bState : boolean);
  end;

function GetIlog(const Filename : string; Activate : Boolean = True; MaxLogSizeMb : integer = 10):ILog;

implementation

uses
  unix,
  DateUtils,
  Utils.Files;

type
  TLogList = Class(TThreadList)
  private
    FBuffer : TStringStream;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSize:Integer;
    procedure AddItem(const Msg : string);
    function Dump(aDest : TStream):String;
  end;

  TLogThread = Class(TThread)
  private
    FDayStarted : integer;
    FMaxLogSize : integer;
    FOriginalFilename,
    FFilename : String;
    FList : TLogList;
    FStartedDate : TDateTime;
    procedure ZipLog(const aFilename : string);
    function GetDay:Integer;
    function MakeFilename(const Filename: String): String;
    function GetArchivePath:String; inline;
    procedure Log(const msg: string);
  public
    constructor Create(const Filename : String; List : TLogList; MaxLogSizeMb : integer);
    procedure Execute; override;
    procedure Dump;
  End;

  TLog = Class(TInterfacedObject, Ilog)
  private
    FFilename : String;
    FList : TLogList;
    FLogThread : TLogThread;
    FActive : Boolean;
    FMaxLogSizeMb : Integer;
    procedure StopThread;
    procedure ZipLogs;
    function GetArchivePath:String; inline;
  public
    constructor Create(const Filename : String; Activate : Boolean; MaxLogSizeMb : integer);
    destructor Destroy; override;
    procedure Log(const msg : string);
    procedure Dump;
    procedure SetActive(const bState : boolean);
  End;


function GetIlog(const Filename : string; Activate : Boolean = True; MaxLogSizeMb : integer = 10):ILog;
begin
  result := TLog.Create(Filename, Activate, MaxLogSizeMb) as ILog;
end;

function SafeOpen(const Filename : string; Mode : Word):TFileStream;
var
  retry : integer;
begin
  retry := 0;
  result := nil;
  repeat
    try
      result := TFileStream.Create(Filename, Mode);
      retry := -1;
    except
      inc(retry);
      sleep(100);
    end;
  until (retry < 0) or (retry > 4);
end;

function SafeDelete(const Filename : string):Boolean;
var
  retry : integer;
begin
  retry := 0;
  repeat
    try
      DeleteFile(Filename);
      retry := -1;
    except
      inc(retry);
      sleep(100);
    end;
  until (retry < 0) or (retry > 4);
  result := retry < 0;
end;

procedure CheckArchivedLogs(const Filename, ArchivePath : String);
{var
  files : TStringDynArray;
  mask : string;
  }
begin
  {
  mask := '*' + ChangeFileExt(ExtractFilename(Filename), '.zip');
  files := TDirectory.GetFiles(ArchivePath, mask);
  while length(files) > 31 do
  begin
    if not SafeDelete(files[0]) then
      Exit;
    files := TDirectory.GetFiles(ArchivePath, mask);
  end;
  }
end;

{ TLogList }

procedure TLogList.AddItem(const Msg: string);
begin
  with LockList do
  try
    FBuffer.WriteString(Msg + sLineBreak);
  finally
    UnlockList;
  end;
end;

constructor TLogList.Create;
begin
  FBuffer := TStringStream.Create(''); //, TEncoding.UTF8);
  inherited;
end;

destructor TLogList.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

function TLogList.Dump(aDest: TStream): String;
begin
  with LockList do
  try
    try
      aDest.Seek(0, soEnd);
      FBuffer.Position := 0;
      aDest.CopyFrom(FBuffer, FBuffer.Size);
      FBuffer.Size:= 0;
    except
    end;
  finally
    UnlockList;
  end;
end;

function TLogList.GetSize: Integer;
begin
  with LockList do
  try
    result := FBuffer.Size;
  finally
    UnlockList;
  end;
end;

{ TLog }

procedure TLog.Log(const msg: string);
begin
  if FActive then
  FList.AddItem(Format('%s Thread ID : %.8d : %s',
                         [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now),
                          QWord(GetCurrentThreadId()), Msg]));
end;

constructor TLog.Create(const Filename: String; Activate : Boolean; MaxLogSizeMb : integer);
begin
  inherited Create;
  FFilename := Filename;
  FActive := Activate;
  FList := TLogList.Create;
  FMaxLogSizeMb := MaxLogSizeMb;
  if FActive then
    FLogThread := TLogThread.Create(Filename, FList, MaxLogSizeMb)
  else
    FLogThread := nil;
  Log('Logger created.');
end;

destructor TLog.Destroy;
begin
  Log('Logger destroying.');
  StopThread;
  FList.Free;
  ZipLogs;
  CheckArchivedLogs(FFilename, GetArchivePath);
  inherited;
end;

procedure TLog.SetActive(const bState: Boolean);
begin
  if Factive = bState then
    Exit;

  if Factive and not bState then
  begin
    Log('Logger deactivated.');
    StopThread;
  end;

  if not Factive and bState then
  begin
    Log('Logger activated.');
    FLogThread := TLogThread.Create(FFilename, FList, FMaxLogSizeMb);
  end;

  FActive := bState;
end;

procedure TLog.Dump;
begin
  if Assigned(FLogThread) then
    FLogThread.Dump;
end;

function TLog.GetArchivePath: String;
begin
  result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(FFilename)) + 'Archives');
  if not DirectoryExists(result) then
    ForceDirectories(result);
end;

procedure TLog.StopThread;
begin
  if Assigned(FLogThread) then
  begin
    with FLogThread do
    begin
      Terminate;
      WaitFor;
    end;
    FreeAndNil(FLogThread);
  end;
end;

procedure TLog.ZipLogs;
{
var
  f, fname : string;
  files : TStringDynArray;
  st : TFileStream;
  z : TZipFile;
}
var
  cmd : string;
begin
    cmd := '/usr/bin/zip -9 -m ' + GetArchivePath +
           FormatDateTime('yyyy-mm-dd-', now) +
           ChangeFileExt(ExtractFileName(FFilename), '.zip ') +
           ExtractFilePath(FFilename) + FormatDateTime('yyyy-mm-dd', now) + '*.log';

//  Sysutils.ExecuteProcess('/usr/bin/zip', cmd);
  fpsystem(cmd);

  {
  fname := GetArchivePath + //IncludeTrailingPathDelimiter(ExtractFilePath(FFilename)) +
           FormatDateTime('yyyy-mm-dd - ', now) +
           ChangeFileExt(ExtractFileName(FFilename), '.zip');


  z := TZipFile.Create;
  with z do
  try
    files := TDirectory.GetFiles(ExtractFilePath(FFilename), '*.log');
    if length(files) > 0 then
    try
      if TFile.Exists(fname) then
        Open(fname, zmReadWrite)
      else
        Open(fname, zmWrite);

      for f in files do
      begin
        st := SafeOpen(f, fmOpenRead or fmShareDenyWrite);
        if Assigned(st) then
        try
          Add(st, ExtractFileName(f), zcDeflate);
        finally
          st.Free;
          SafeDelete(f);
        end;
      end;
      Close;
    except
    end;
  finally
    z.Free;
  end;
  }
end;

{ TLogThread }

constructor TLogThread.Create(const Filename: String; List : TLogList;
                              MaxLogSizeMb : Integer);
begin
  FStartedDate := now;
  FDayStarted := GetDay;
  FMaxLogSize := MaxLogSizeMb * 1024 * 1024;
  FOriginalFilename := Filename;
  FFilename := MakeFilename(Filename);
  Flist := List;
  inherited Create(False);
  Priority := tpLower;
end;

function TLogThread.MakeFilename(const Filename: String): String;
begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(Filename)) +
            FormatDateTime('yyyy-mm-dd-hh-nn-ss-', FStartedDate) +
            ExtractFileName(Filename);
end;

function TLogThread.GetArchivePath: String;
begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(FOriginalFilename)) + 'Archives';
  if not DirectoryExists(result) then
    ForceDirectories(result);
end;

function TLogThread.GetDay:Integer;
var
  y, m, d : word;
begin
  DecodeDate(now, y, m, d);
  result := d;
end;

procedure TLogThread.Log(const msg: string);
begin
  FList.AddItem(Format('%s Thread ID : %.8d : %s',
                         [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now),
                          QWord(GetCurrentThreadId()), Msg]));
end;

procedure TLogThread.ZipLog(const aFilename : string);
{
var
  f : string;
  st : TFileStream;
  z : TZipFile;
}
var
  cmd : string;
begin
  cmd := '/usr/bin/zip -9 -m ' + GetArchivePath +
          FormatDateTime('yyyy-mm-dd-', now) +
          ChangeFileExt(ExtractFileName(FFilename), '.zip ') +
          ExtractFilePath(FFilename) + FormatDateTime('yyyy-mm-dd', now) + '*.log';

//  Sysutils.ExecuteProcess('/usr/bin/zip', cmd);
  fpSystem(cmd);
  // zip -9 -m 2019-07-28.zip 2019-07-28*.log
{
  f := GetArchivePath + // IncludeTrailingPathDelimiter(ExtractFilePath(FOriginalFilename)) +
       FormatDateTime('yyyy-mm-dd - ', now) +
       ChangeFileExt(ExtractFileName(FOriginalFilename), '.zip');

  if TFile.Exists(aFilename) then
  begin
    z := TZipFile.Create;
    with z do
    try
      try
        if TFile.Exists(f) then
          Open(f, zmReadWrite)
        else
          Open(f, zmWrite);
        try
          st := SafeOpen(aFilename, fmOpenRead or fmShareDenyWrite);
          if Assigned(st) then
          try
            Add(st, ExtractFileName(aFilename), zcDeflate);
          finally
            st.Free;
            SafeDelete(aFilename);
          end;
        finally
          Close;
        end;
      except
      end;
    finally
      z.Free;
    end;
  end;
}
end;

const
  UTF8Bom : array[0..2] of Byte = (239, 187, 191);

procedure TLogThread.Dump;
var
  f : TFileStream;
begin
  if Flist.GetSize > 0 then
  begin
    if FileExists(FFilename) then
      f := SafeOpen(FFilename, fmOpenReadWrite or fmShareDenyWrite)
    else
    begin
      f := SafeOpen(FFilename, fmCreate or fmShareDenyWrite);
      f.WriteBuffer(UTF8Bom, 3);
    end;

    if Assigned(f) then
    try
      FList.Dump(f);
    finally
      f.Free;
    end;
  end;
end;

procedure TLogThread.Execute;
const
  FiftyKB = 50 * 1024;
var
  LastCheck : TDateTime;
begin
  Log('Logger started.');

  LastCheck := Now;
  while not Terminated do
  begin
    if (Flist.GetSize > FiftyKB) or (SecondsBetween(now, LastCheck) > 5) then
    try
      if (GetFileSize(FFilename) > FMaxLogSize) or (GetDay <> FDayStarted) then
      begin
        ZipLog(FFilename);
        FStartedDate := now;
        FDayStarted := GetDay;
        FFilename := MakeFilename(FOriginalFilename);
        CheckArchivedLogs(FOriginalFilename, GetArchivePath);
      end;

      Dump;
      LastCheck := now;
    except
    end
    else
      Sleep(1000);
  end;

  Log('Logger ended.');
  Dump;
  CheckArchivedLogs(FOriginalFilename, GetArchivePath);
end;


end.

