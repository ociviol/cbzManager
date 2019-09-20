unit uThreadExtract;

interface

uses
  Winapi.Windows, System.SysUtils,
  System.Classes, Graphics,
  System.Generics.Collections,
  uCbz, Utils.Logger, uThreadConvert,
  gtPdfDoc, gtCstPDFDoc, gtPDFClasses,
  uRar, uDataItem, uDataTypes, sevenzip;

type
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
    FProgress : TProgressEvent;
    FProgressID : Cardinal;
    FOnBadFile : TNotifyEvent;
    FOperations: TImgOperations;
    FOwner : TObject;
    function GetWorking: Boolean;
    function GetErrorMsg: String;
    function GetHasError: Boolean;
    function GetNbFiles: Integer;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    property HasError : Boolean read GetHasError;
    property ErrorMsg : String read GetErrorMsg;
    property NbFiles : Integer read GetNbFiles;
    property Working : Boolean read GetWorking;
  End;

  TThreadPdfExtract = Class(TThreadExtract)
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;

  TThreadRarExtract = Class(TThreadExtract)
  private
    FRar : TUnRar;
    function OnTest(Sender : TObject; const Filename : String; const Cur, Max : Integer):Boolean;
    function OnUnrar(Sender : TObject; Index : Integer; Data : TMemoryStream):Boolean;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;

  TThreadZipExtract = Class(TThreadExtract)
  private
    Cbz : TCbz;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;

  TThread7ZipExtract = Class(TThreadExtract)
  private
    arc : I7zInArchive;
  public
    constructor Create(aOwner : TObject; const Filename : String;
                       Operations : TImgOperations;
                       PoolData : TThreadDataItem;
                       Log : ILog; Results : TStrings;
                       Progress : TProgressEvent; ProgressID : Cardinal;
                       OnBadFile : TNotifyEvent); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  End;

implementation

uses
  psAPI, Utils.Zip, uWorkerThread, Character, System.IOUtils,
  Math, Helper.TStringList, FreeImage, uStrings;


const
  MB = 1024 * 1024;

{ TThreadExtract }

constructor TThreadExtract.Create(aOwner : TObject; const Filename: String;
                                  Operations : TImgOperations;
                                  PoolData: TThreadDataItem; Log: ILog;
                                  Results: TStrings;
                                  Progress : TProgressEvent; ProgressID : Cardinal;
                                  OnBadFile : TNotifyEvent);
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
  inherited Create;
end;

destructor TThreadExtract.Destroy;
begin
  FSync.Free;
  inherited;
end;

procedure TThreadExtract.Execute;
begin
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

{ TThreadPdfExtract }

constructor TThreadPdfExtract.Create(aOwner : TObject; const Filename : String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem;
                                     Log: ILog; Results : TStrings;
                                     Progress : TProgressEvent; ProgressID : Cardinal;
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

function ProcessMemory: integer;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: integer;
begin
  result := 0;
  // Get the used memory for the current process
  cb := SizeOf(TProcessMemoryCounters);
  GetMem(pmc, cb);
  try
    pmc^.cb := cb;
    if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
      result := integer(pmc^.WorkingSetSize);
  finally
    FreeMem(pmc);
  end;
end;
{
function MemoryUsage: Cardinal;
var
  statex: TMemoryStatus;
begin
  GlobalMemoryStatus(statex);
  result := Format('Memory:Process:%dMb, System:(used %d%%)/%dMb , Free:%dMB, ' + 'Virtual:%d/%dMb, PageFile:%d/%dMb',
    [ProcessMemory, statex.dwMemoryLoad, statex.dwTotalPhys div _MB, statex.dwAvailPhys div _MB,
    statex.dwAvailVirtual div _MB, statex.dwTotalVirtual div _MB, statex.dwAvailPageFile div _MB,
    statex.dwTotalPageFile div _MB]);
end;
}
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

{ TThreadRarExtract }

constructor TThreadRarExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress : TProgressEvent; ProgressID : Cardinal;
                                     OnBadFile : TNotifyEvent);
begin
  FRar := TUnRar.Create(OnUnrar, OnTest);
  try
    FRar.Open(Filename);
    FNbfiles := FRar.FileCount;
  except
    FRar.Free;
    Raise;
  end;
  inherited;
end;

function TThreadRarExtract.OnTest(Sender : TObject; const Filename : String; const Cur, Max : Integer):Boolean;
begin
  if Assigned(FProgress) then
    Synchronize(procedure begin
                  FProgress(Self, FProgressID, Cur, Max - 1, str_testing +
                            ExtractFilename(TUnrar(Sender).Filename) + ' (' + ExtractFilename(Filename) + ')');
                end);
  Sleep(50);
  result := not Terminated;
end;

function TThreadRarExtract.OnUnrar(Sender : TObject; Index : Integer; Data : TMemoryStream):Boolean;
begin
  if Tcbz.IsMetaFile(ExtractFilename(FRar.FileNames[Index])) then
    FPoolData.AddItem(Data, Index, [], FOperations, dtMeta,
                      FIF_UNKNOWN, ExtractFilename(FRar.FileNames[Index]))
  else
    FPoolData.AddItem(Data, Index, [], [opConvert]);

  Sleep(50);

  result := (not Terminated);
end;

destructor TThreadRarExtract.Destroy;
begin
  FRar.Free;
  inherited;
end;

procedure TThreadRarExtract.Execute;
begin
  try
    while not Terminated do
    begin
      try
        FRar.Test;
        if Assigned(FProgress) then
            Synchronize(procedure begin
                          FProgress(Self, FProgressID, 0, FNbfiles - 1, '(' + str_writing +
                                    TCbz.CleanFilename(ExtractFilename(FFilename)) + ')');
                        end);

        if not Terminated then
          FRar.Extract;

        if Terminated then
        begin
          FPoolData.ClearLists;
          Exit;
        end
        else
          Terminate;
      except
        on e: Exception do
        begin
          Terminate;
          FHasError := True;
          FErrorMsg := e.message;
          Flog.Log('TThreadRarExtract Error :  ' + e.Message);
          Synchronize(procedure begin
                        FResults.Add('RarExtract ' + str_error + ' : ' + FFilename + ' ' + ErrorMsg);
                        if Assigned(FOnBadFile) then
                          FOnBadFile(Self);
                      end);
          TCbzWorkerThread(FOwner).Cancel(nil);
          exit;
        end;
      end;
    end;
  finally
    FWorking := False;
  end;
end;

{ TThreadZipExtract }

constructor TThreadZipExtract.Create(aOwner : TObject; const Filename: String;
                                     Operations : TImgOperations;
                                     PoolData: TThreadDataItem; Log: ILog;
                                     Results: TStrings;
                                     Progress: TProgressEvent; ProgressID: Cardinal;
                                     OnBadFile : TNotifyEvent);
begin
  Cbz := TCbz.Create(Log);
  Cbz.Open(Filename, zmRead);
  FNbFiles := Cbz.AllowedFileCount;
  inherited;
end;

destructor TThreadZipExtract.Destroy;
begin
  Cbz.Free;
  inherited;
end;

procedure TThreadZipExtract.Execute;
var
  Results : TStringList;
  i : integer;
  Filenames : TNaturalSortStringList;
  MetaFiles : TStringlist;
  f : TArray<string>;
  s : string;
  
  procedure AddBlock(const aFilename : String; Index : Integer;
                     DataType : TDataType = dtImage);
  var
    st : TStream;
    info : TZipHeader;
    ms : TMemoryStream;
  begin
    if Index >= 0 then
      Cbz.Read(Index, st, info)
    else
      Cbz.Read(aFilename, st, info);
    try
      ms := TMemoryStream.Create;
      ms.CopyFrom(st, info.UncompressedSize);
      FPoolData.AddItem(ms, [], FOperations, DataType, FIF_UNKNOWN, ExtractFileName(aFilename.Replace('/', '\')));
      Sleep(50);
    finally
      st.Free;
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
          for i := 0 to Cbz.FileCount -1 do
          begin
            s := str_testing + ExtractFileName(FFilename) + ' (' +
                 ExtractFilename(Cbz.Filenames[i].Replace('/', '\')) + ')';

            if Assigned(FProgress) then
              Synchronize(procedure begin
                            FProgress(Self, FProgressID, i, Cbz.FileCount-1, s);
                          end);

            if not Cbz.TestFile(i) then
              raise Exception.Create(FormatDateTime('hh:nn:ss' ,now) +
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
        Filenames := TNaturalSortStringList.Create;
        MetaFiles := TStringlist.Create;
        try
          f := Cbz.FileNames;
          for i := 0 to length(f) - 1 do
            if Tcbz.IsMetaFile(f[i]) then
              MetaFiles.AddObject(f[i], TObject(i))
            else
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
          s := '(' + str_writing + TCbz.CleanFilename(ExtractFilename(FFilename)) + ')';
          if Assigned(FProgress) then
            Synchronize(procedure begin
                          FProgress(Self, FProgressID, 0, Max(0, FNbfiles - 1), s);
                        end);

          for i := 0 to Filenames.Count - 1 do
          begin
            if Terminated then
            begin
              FPoolData.ClearLists;
              Cbz.Close;
              Exit;
            end;

            AddBlock(Filenames[i], Integer(Filenames.Objects[i]));
          end;
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
          Synchronize(procedure begin
                        FResults.Add(FErrorMsg);
                        if Assigned(FOnBadFile) then
                          FOnBadFile(Self);
                      end);
          TCbzWorkerThread(FOwner).Cancel(nil);
          exit;
        end;
      end;
    end;
  finally
    FWorking := False;
  end;
end;

{ TThread7ZipExtract }

constructor TThread7ZipExtract.Create(aOwner: TObject; const Filename: String;
  Operations: TImgOperations; PoolData: TThreadDataItem; Log: ILog;
  Results: TStrings; Progress: TProgressEvent; ProgressID: Cardinal;
  OnBadFile: TNotifyEvent);
begin
  FFilename := Filename;
  arc := CreateInArchive(CLSID_CFormat7z);
  arc.OpenFile(Filename);
  FNbFiles := arc.GetNumberofItems;
  inherited;
end;

destructor TThread7ZipExtract.Destroy;
begin
  arc := nil;
  inherited;
end;

procedure TThread7ZipExtract.Execute;
var
  i: Integer;
  s : string;
  Filenames : TNaturalSortStringList;
  ms : TMemoryStream;
begin
  try
    Filenames := TNaturalSortStringList.Create;
    try
      for i := 0 to FNbFiles - 1 do
      begin
        if Terminated then
        begin
          FPoolData.ClearLists;
          Exit;
        end;

        try
          if not arc.ItemIsFolder[i] then
          begin
            arc.ExtractItem(i, nil, true);
            Filenames.AddObject(ExtractFilename(arc.ItemPath[i]), TObject(i));
          end;

          s := str_testing + ExtractFileName(FFilename) + ' (' +
               ExtractFilename(arc.ItemPath[i].Replace('/', '\')) + ')';

          if Assigned(FProgress) then
            Synchronize(procedure begin
                          FProgress(Self, FProgressID, i, FNbFiles - 1, s);
                        end);
        except
          raise Exception.Create(FormatDateTime('hh:nn:ss' ,now) +
                                 'Cannot process ' + FFileName + ' because it has errors.');
        end;
      end;

      // extract
      FNbFiles := Filenames.Count;
      Filenames.Sort;
      for i := 0 to Filenames.Count - 1 do
      begin
        if Terminated then
        begin
          FPoolData.ClearLists;
          Exit;
        end;

        if not arc.ItemIsFolder[i] then
        begin
          ms := TMemoryStream.Create;
          arc.ExtractItem(Integer(Filenames.Objects[i]), ms, false);
          FPoolData.AddItem(ms, [], FOperations, dtImage, FIF_UNKNOWN, '');
        end;
      end;
    finally
      Filenames.Free;
      FWorking := false;
      arc.Close;
      arc := nil;
    end;
  except
    on e: Exception do
    begin
      arc := nil;
      Terminate;
      FHasError := True;
      FErrorMsg := e.message;
      Flog.Log('TThreadZipExtract Error :  ' + e.Message);
      Synchronize(procedure begin
                    FResults.Add(FErrorMsg);
                    if Assigned(FOnBadFile) then
                      FOnBadFile(Self);
                  end);
      TCbzWorkerThread(FOwner).Cancel(nil);
      exit;
    end;
  end;

  Terminate;
end;

end.
