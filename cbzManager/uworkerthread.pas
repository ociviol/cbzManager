unit uWorkerThread;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface
uses
  SysUtils,
  Classes, Graphics,
  uCbz, Utils.Logger, uDataItem,
  uDataTypes, Utils.Searchfiles
{$if defined(Darwin) or defined(Linux)}
  ,cthreads
{$endif}
  ;

type
  TJobStatus = (jsWaiting, jsProcessing, jsDone);
  TAddFileProc = procedure(const aFileName: string; IsNew : Boolean = False) of object;

  TJobPoolRec = Class
    Filename : String;
    arcType : TArcType;
    Status : TJobStatus;
    Operations : TImgOperations;
  End;

  TJobPool = Class
  private
    FLog : ILog;
    FSync : TThreadList;
    function GetCount: Integer;
    function GetjobFilename(Index: Integer): String;
    function GetJobStatus(Index: Integer): TJobStatus;
    function GetJobFilenames: TStringList;
  protected
    function FileCount:Integer;
    function AvailFileCount: Integer;
    function GetFirstJob:TJobPoolRec;
    function GetJobIndex(const Filename : String):Integer;
    function SetJobStatus(Index : Integer; Status : TJobStatus):TJobPoolRec; overload;
    function SetJobStatus(const Filename : String; Status : TJobStatus):TJobPoolRec; overload;
  public
    constructor Create(Log : ILog);
    destructor Destroy; override;
    procedure AddJob(const Filename : String; arcType : TArcType = arcZip; Operations : TImgOperations = [opConvert]);
    procedure Stats(out nbZip, nbRar, nbPdf : Integer);
    procedure DeleteJob(const Filename : String);
    function FileInQueue(const Filename : String):Boolean;
    property Count:Integer read GetCount;
    property JobFilenames:TStringList read GetJobFilenames;
    property JobFilename[Index:Integer] :String read GetjobFilename;
    property JobStatus[Index:Integer] : TJobStatus read GetJobStatus;
  End;

  TCbzWorkerThread = Class(TCancellableThread)
  private
    FTimeOut: Boolean;
    FProgress : TCbzProgressEvent;
    FAddFile : TAddFileProc;
    FProgressID : QWord;
    FLog : ILog;
    FJobpool : TJobPool;
    FPoolData : TThreadDataItem;
    FResults : TStrings;
    FFilesToProcess : Integer;
    FOnBadFile : TNotifyEvent;
    FCurJob : TJobPoolRec;
    FCbz : TCbz;
    FStartDate : TDateTime;
    FCur,
    FMax : Integer;
    FMsg,
    FFilename : String;
    FSender : TObject;
    FNewFile : String;
    FAddFileBool : Boolean;
    function Convert(const aFilename : String; arcType : TArcType; Operations : TImgOperations):String;
    function GetElapsed(aNow, aThen : TDatetime):String;
    procedure OnPutData;
    function cbzGetTempPath:String;
    function cbzGetTempFileName:String;
    function GetETA(StartDate : TDateTime; Cur, Max : Integer):String;
    procedure DoProgress;
    procedure DoOnBadFile;
    procedure DoDisableButton;
    procedure DoAddFile;
  public
    constructor Create(Jobpool : TJobPool; PoolData : TThreadDataItem;
                       Progress : TCbzProgressEvent; AddFile : TAddFileProc; Log : ILog;
                       Results : TStrings; OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Terminate; reintroduce;
    property CurJob : TJobPoolRec read FCurJob;
  End;

implementation

uses
  StdCtrls,
  DateUtils, Utils.ZipFile,
  Utils.Files,
  uThreadExtract,
  zstream
  //, uStrings
  ;


{ TJobPool }

constructor TJobPool.Create(Log : ILog);
begin
  inherited Create;
  FLog := Log;
  FSync := TThreadList.Create;
end;

destructor TJobPool.Destroy;
begin
  FSync.Free;
  inherited;
end;

procedure TJobPool.AddJob(const Filename : String; arcType : TArcType = arcZip; Operations : TImgOperations = [opConvert]);
var
  aRec : TJobPoolRec;
begin
//  if Tfile.Exists(TCbz.CleanFilename(Filename)) then
//    raise Exception.Create('File already converted.');

  with FSync.LockList do
  try
    if GetJobIndex(Filename) >= 0 then
      Exit;

    aRec := TJobPoolRec.Create;
    aRec.Filename := Filename;
    aRec.arcType := arcType;
    aRec.Status := jsWaiting;
    aRec.Operations := Operations;
    FLog.Log(ClassName + ' Job added : ' + aRec.Filename);
    Add(aRec);
  finally
    FSync.UnlockList;
  end;
end;

procedure TJobPool.Stats(out nbZip, nbRAr, nbPdf : Integer);
var
  i : integer;
begin
  nbZip := 0;
  nbRAr := 0;
  nbPdf := 0;
  with FSync.LockList do
  try
    for i := 0 to Count - 1 do
      case TJobPoolRec(Items[I]).arcType of
        arcZip: inc(nbZip);
        arcRar: inc(nbRar);
        arcPdf: inc(nbPdf);
      end;
  finally
    FSync.UnlockList;
  end;
end;

function TJobPool.AvailFileCount: Integer;
var
  i : Integer;
begin
  with FSync.LockList do
  try
    result := 0;
    for i := 0 to Count - 1 do
      if TJobPoolRec(Items[i]).Status = jsWaiting then
        inc(result);
  finally
    FSync.UnlockList;
  end;
end;

function TJobPool.FileCount: Integer;
begin
  with FSync.LockList do
  try
    result := count;
  finally
    FSync.UnlockList;
  end;
end;

function TJobPool.GetCount: Integer;
begin
  result := FileCount;
end;

function TJobPool.GetFirstJob: TJobPoolRec;
var
  i : integer;
begin
  with FSync.LockList do
  try
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
        if TJobPoolRec(Items[I]).Status = jsWaiting then
          Exit(SetJobStatus(i, jsProcessing));
    end
    else
      result := nil;
  finally
    FSync.UnlockList;
  end;
end;

function TJobPool.GetjobFilename(Index: Integer): String;
begin
   with FSync.LockList do
  try
    result := TJobPoolRec(Items[Index]).Filename;
  finally
    FSync.UnLockList;
  end;
end;

procedure TJobPool.DeleteJob(const Filename : String);
var
  i : integer;
begin
  with FSync.LockList do
  try
    i := GetJobIndex(Filename);
    if (i >= 0) then
      if (TJobPoolRec(Items[i]).Status <> jsProcessing) then
      begin
        TJobPoolRec(Items[i]).Free;
        Delete(i);
      end;
  finally
    FSync.UnLockList;
  end;
end;

function TJobPool.FileInQueue(const Filename : String):Boolean;
begin
  result := GetJobIndex(Filename) >= 0;
end;

function TJobPool.GetJobFilenames: TStringList;
var
  i : integer;
begin
  result := TStringList.Create;
  with FSync.LockList do
  try
    for i := 0 to Count - 1 do
      Result.Add(TJobPoolRec(Items[I]).Filename);
  finally
    FSync.UnLockList;
  end;
end;

function TJobPool.GetJobIndex(const Filename : String):Integer;
var
  i : integer;
begin
  with FSync.LockList do
  try
    for i := 0 to Count - 1 do
      if TJobPoolRec(Items[I]).Filename.ToLower.Equals(Filename.ToLower) then
        Exit(i);
  finally
    FSync.UnLockList;
  end;
  result := -1;
end;

function TJobPool.GetJobStatus(Index: Integer): TJobStatus;
begin
  with FSync.LockList do
  try
    result := TJobPoolRec(Items[Index]).Status;
  finally
    FSync.UnLockList;
  end;
end;

function TJobPool.SetJobStatus(const Filename : String; Status : TJobStatus):TJobPoolRec;
var
  i : integer;
begin
  result := nil;
  with FSync.LockList do
  try
    i := GetJobIndex(Filename);
    if (i >= 0) then
      result := SetJobStatus(i, Status);
  finally
    FSync.UnLockList;
  end;
end;

function TJobPool.SetJobStatus(Index : Integer; Status : TJobStatus):TJobPoolRec;
begin
  with FSync.LockList do
  try
    TJobPoolRec(Items[Index]).Status := Status;
    result := TJobPoolRec(Items[Index]);
  finally
    FSync.UnLockList;
  end;
end;

{ TCbzWorkerThread }

constructor TCbzWorkerThread.Create(Jobpool : TJobPool; PoolData : TThreadDataItem;
                                    Progress : TCbzProgressEvent; AddFile : TAddFileProc; Log : ILog;
                                    Results : TStrings; OnBadFile : TNotifyEvent);
begin
  FjobPool := JobPool;
  FOnBadFile := OnBadFile;
  FPoolData := PoolData;
  FPoolData.OnPutData := @OnPutData;
  FPoolData.Clear;
  FProgress := Progress;
  FreeOnTerminate := False;
  FProgressID := 0;
  FAddFile := AddFile;
  FResults := Results;
  FLog := Log;
  inherited Create(False);
  Log.Log(ClassName + ' Worker thread ID : ' + IntToStr(QWord(ThreadID)) + ' created.');
end;

destructor TCbzWorkerThread.Destroy;
begin
  FLog.Log(ClassName + ' Worker thread ID : ' + IntToStr(QWord(ThreadID)) + ' destroyed.');
  Flog := nil;
  inherited;
end;

procedure TCbzWorkerThread.Terminate;
begin
  if Suspended then
    Resume{%H-};
  inherited;
end;

procedure TCbzWorkerThread.OnPutData;
begin
  if Suspended then
    Resume{%H-};
end;

function TCbzWorkerThread.GetElapsed(aNow, aThen : TDatetime):String;
var
  h, m, s : word;
begin
  s := SecondsBetween(aNow, aThen);
  m := s div 60;
  dec(s, m * 60);
  h := m div 60;
  dec(m, h * 60);
  if h <> 0 then
    Result := Format('%.2d:%.2d:%.2d', [h, m, s])
  else
    Result := Format('%.2d:%.2d', [m, s]);
end;

procedure TCbzWorkerThread.DoProgress;
begin
  FProgress(Self, FProgressID, FCur, FMax, FMsg);
end;

procedure TCbzWorkerThread.DoOnBadFile;
begin
  FOnBadFile(Self);
end;

function TCbzWorkerThread.Convert(const aFilename : String; arcType : TArcType; Operations : TImgOperations):String;

  function CopyStream(Stream : TStream):TMemoryStream;
  begin
    result := TMemoryStream.Create;
    Stream.Position := 0;
    result.CopyFrom(Stream, Stream.Size);
    result.Position := 0;
  end;

var
  s, fname, newf : string;
  i : integer;
  ThreadExtract : TThreadExtract;
  //BeginDate : TDateTime;
  Rec : TDataRec;
  LastAskedID : Integer;
  LastAskedDate : TDateTime;

begin
  //BeginDate := now;
  if Assigned(FProgress) then
  begin
    FCur := 0;
    FMax := 1;
    FMsg := '(Converting : ' + ExtractFilename(aFilename);
    Synchronize(@DoProgress);
  end;

  try
    FFilename := aFilename;
    FPoolData.Clear;
    ThreadExtract := nil;
    try
      case arcType of
        arcZip, //:  ThreadExtract := TThreadZipExtract.Create(Self, aFilename, Operations, FPoolData, FLog, FResults, FProgress, FProgressID, FOnBadFile);
        arc7Zip: ThreadExtract := TThread7ZipExtract.Create(Self, aFilename, Operations, FPoolData, FLog, FResults, FProgress, FProgressID, FOnBadFile);
        arcRar:  ThreadExtract := TThreadRarExtract.Create(Self, aFilename, Operations, FPoolData, FLog, FResults, FProgress, FProgressID, FOnBadFile);
        arcPdf:  ; //ThreadExtract := TThreadPdfExtract.Create(Self, aFilename, Operations, FPoolData, FLog, FResults, FProgress, FProgressID, FOnBadFile);
        arcUnknown: exit;
      end;

      newf := TCbz.CleanFilename(aFilename);
      //MetaData := TDictionary<String, TMemoryStream>.Create;
      FCbz := TCBz.Create(FLog);
      try
        repeat
          sleep(100);
        until (not ThreadExtract.Working) or ThreadExtract.HasError;

        FFilesToProcess := ThreadExtract.NbFiles;
        if ThreadExtract.HasError then
          Exit;

        FJobPool.FSync.LockList;
        try
          fname := cbzGetTempFileName;
          //GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(ThreadID)) + IntToStr(QWord(GetTickCount64)));  //GetTempFileName(GetTempDir, 'Cbz');
          FCbz.Open(fname, zmWrite, nil, IntToStr(FFilesToProcess).Length);
        finally
          FJobPool.FSync.UnLockList;
        end;

        FStartDate := now;

        i := 0;
        Rec := nil;
        LastAskedID := -1;
        while (FCbz.FileCount < FFilesToProcess) or ThreadExtract.Working do
        begin
          if LastAskedID <> i then
          begin
            LastAskedID:=i;
            LastAskedDate:=now;
          end;

          while (not FPoolData.GetOut(i, Rec)) do
          begin
            if (Terminated or FCanceled or ThreadExtract.HasError) then
            begin
              if Assigned(Rec) then
                FreeAndNil(Rec);
              Exit;
            end;

            Sleep(50);
            if not ThreadExtract.Working and (ThreadExtract.NbFiles = 0) then
              break;

            if (i = LastAskedID) and (SecondsBetween(now, LastAskedDate) > 120) then // 2mn timeout
            begin
              FLog.Log('TCbzWorkerThread.Convert : Timeout on item : ' + IntTostr(i));
              FResults.Add('Conversion of file "' + ExtractFileName(aFilename) + '" timed out, job will be run again');
              FCanceled := True;
              FTimeOut:=True;
              Synchronize(@DoOnBadFile);
              raise Exception.Create('Timeout on file : ' + ExtractFileName(aFilename));
            end;
          end;

          if Assigned(Rec) and Assigned(Rec.Stream) then
          try
            if Terminated or FCanceled then
              Exit;

            if Rec.DataType = dtMeta then
              FCbz.AppendStream(Rec.Stream, Rec.Filename, Now, zstream.clmax)
            else
            begin
              s := FCbz.GetNextFilename;
              FCbz.AppendStream(Rec.Stream, s, Now, zstream.clnone);
            end;

            inc(i);
            Sleep(100);
            FMax := FFilesToProcess - 1;
            if Assigned(FProgress) then
            begin
              FCur := i;
              FMsg := 'Writing ' + ExtractFilename(newf) + ' : Adding :' + s +
                      ' (' + GetETA(FStartDate, i, FFilesToProcess) + ')';
              Synchronize(@DoProgress);
            end;
          finally
            Rec.Free;
          end
          else
          begin
            FLog.Log('TCbzWorkerThread ConvertImage ' + IntToStr(i) + ' failed.');
            FCanceled := True;
            Synchronize(@DoOnBadFile);
            raise Exception.Create('Conversion of file "' + ExtractFileName(aFilename) + '" Canceled because could not convert image ' + IntTostr(i));
          end;

          if FPoolData.Empty and not ThreadExtract.Working then
            break;
        end;

        if FCbz.FileCount < FFilesToProcess then
         begin
          FLog.Log('TCbzWorkerThread ConvertFile failed.');
          FResults.Add('Conversion of file "' + ExtractFileName(aFilename) + '" failed, job will be run again');
          FCanceled := True;
          FTimeOut:=True;
          Synchronize(@DoOnBadFile);
          raise Exception.Create(Format('Conversion of file "%s" Canceled because not all images are there. Expected : %d Has : %d',
                                        [ExtractFileName(aFilename), FFilesToProcess, FCbz.FileCount]));
        end;
        //if MetaData.Count > 0 then
        //  for s in MetaData.Keys do
        //  try
        //    FCbz.Add(MetaData[s], s, zcDeflate);
        //  finally
        //    MetaData[s].Free;
        //  end;

        Flog.Log('TCbzWorkerThread Done.');

        if Fcbz.FileCount > 0 then
        begin
          Fcbz.Close;
          newf := TCbz.CleanFilename(aFilename);
          Flog.Log('TCbzWorkerThread Delete ' + aFileName);

          //FileToTrash(aFileName);
          if (opDeleteFile in Operations) then
            FileToTrash(aFileName)
          else
          begin
            s :=  IncludeTrailingPathDelimiter(ExtractFilePath(aFileName)) + 'Done';
            if not DirectoryExists(s) then
              ForceDirectories(s);
            s := IncludeTrailingPathDelimiter(s) + ExtractFileName(aFileName) + '.old';
            while FileExists(s) do
              s := s + '.old';

            RenameFile(aFilename, s);
          end;

          i := 1;
          while FileExists(newf) do
          begin
            newf := ChangeFileExt(newf, ' (' + inttostr(i)+').cbz');
            inc(i);
          end;
          if not FileExists(newf) then
          begin
            result := newf;
            Flog.Log('TCbzWorkerThread Rename ' + fname + ' -> ' + newf);
            try
              CopyFile(fname, newf);
              if FileExists(newf) then
                DeleteFile(fname);
            except
              // restore original file
              RenameFile(s, aFilename);
              FCanceled := True;
              FTimeOut:=True;
              raise;
            end;
          end;
        end
        else
          raise Exception.Create('File ' + aFilename + ' is empty.');
      finally
        //MetaData.Free;
        Fcbz.Free;
        if Terminated or FCanceled then
          if FileExists(fname) then
            DeleteFile(fname);
      end;
    finally
      if Assigned(ThreadExtract) then
      try
        with ThreadExtract do
        begin
          if not Terminated then
          begin
            Terminate;
            WaitFor;
          end;
          Free;
        end;
      except
        on e: Exception do
          Flog.Log('TCbzWorkerThread Error : Terminating ThreadExtract  ' + e.Message);
      end;
      FPoolData.Clear;
    end;
  except
    on e: Exception do
    begin
      result := '';
      s := e.Message;
      FPoolData.Clear;
      Flog.Log('TCbzWorkerThread Error :  ' + e.Message);
      FResults.Add(s);
      Synchronize(@DoOnBadFile);
      try
        if FileExists(fname) then
        begin
          Flog.Log('TCbzWorkerThread Delete :  ' + fname);
          DeleteFile(fname);
        end;
      except
      end;
    end;
  end;
end;

function TCbzWorkerThread.cbzGetTempPath:String;
begin
  result := GetTempDir;
end;

function TCbzWorkerThread.cbzGetTempFileName:String;
begin
  result := GetTempFileName(GetTempDir, 'Cbz' + IntToStr(QWord(GetCurrentThreadId)) + IntToStr(QWord(GetTickCount64)));
end;

procedure TCbzWorkerThread.DoDisableButton;
begin
  TButton(FSender).Enabled := false;
end;

procedure TCbzWorkerThread.DoAddFile;
begin
  FAddfile(FNewFile, FAddFileBool);
end;

procedure TCbzWorkerThread.Execute;
var
  newfile, fname : string;

  procedure ClearProgress;
  begin
    if FProgressID > 0 then
    begin
      if Assigned(FProgress) then
      FCur := 0;
      FMax :=0;
      FMsg := '';
      Synchronize(@DoProgress);
      FProgressID := 0;
    end;
  end;
begin
  while not Terminated do
  begin
    if (FJobPool.AvailFileCount > 0) then
    begin
      if FProgressID = 0 then
        FProgressID := QWord(ThreadID);

      FCurJob := FJobPool.GetFirstJob;
      if Assigned(FCurJob) then
      try
        if FileExists(FCurJob.Filename) then
        begin
          FCanceled := false;
          FTimeOut := False;
          fname := FCurJob.Filename;
          FLog.Log(ClassName + ' Job started : ' + FCurJob.Filename);
          NewFile := Convert(FCurJob.Filename, FCurJob.arcType, FCurJob.Operations);

          // timedout re run job
          if FTimeOut then
            FJobpool.SetJobStatus(FCurJob.Filename, jsWaiting)
          else
          begin
            FJobpool.SetJobStatus(FCurJob.Filename, jsDone);
            FJobpool.DeleteJob(FCurJob.Filename);
          end;

          FCurJob := nil;
          if (not Terminated) and FileExists(NewFile) and
             (not FCanceled or not FTimeOut) then
          begin
            FLog.Log(ClassName + ' Job finished : ' + fname);
            FNewFile := newfile;
            FAddFileBool := True;
            Synchronize(@DoAddFile);
            Sleep(50);
          end;
        end;
      except
        on E: Exception do
          FLog.Log(ClassName + ' Execute : ' + E.Message);
      end;
    end
    else
    begin
      if (FJobPool.AvailFileCount = 0) and (FProgressID > 0) then
        ClearProgress;
      Sleep(100);
    end;
  end;
  ClearProgress;
end;

function TCbzWorkerThread.GetETA(StartDate: TDateTime; Cur,
  Max: Integer): String;
var
  intv : integer;
  h, m, s : integer;
  per : extended;
begin
  result := 'ETA';
  per := ((cur + 1) * 100) / max;
  if per >= 5 then
  begin
    intv := MilliSecondsBetween(now, StartDate);
    intv := intv div (cur + 1);
    s := (intv *  (max - (cur + 1))) div 1000;
    if s < 0 then
      s := 0;
    m := s div 60;
    dec(s, m * 60);
    h := m div 60;
    dec(m, h * 60);
    if h > 0 then
      result := Format('%s %.2d:%.2d:%.2d', [Result, h, m, s])
    else
    if m > 0 then
      result := Format('%s %.2d:%.2d', [Result, m, s])
    else
      result := Format('%s %.2d sec', [Result, s]);
  end
  else
    result := Result + ' Calculating...';
end;



end.
