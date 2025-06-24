unit uThreadScrub;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLibraryClasses, utils.Logger, Utils.Searchfiles, uConfig;

type
  TLibrayAction = (laAdd, laDelete);
  TLibraryNotify = procedure(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil) of object;

  { TThreadScrub }

  TThreadScrub = Class(TThread)
  private
    FVal,
    FCnt,
    FDeleted,
    Fsynced : Integer;
    FFileList: TItemList;
    FProgress : TSearchFileProgressEvent;
    FLog : ILog;
    FNotifier : TLibraryNotify;
    FItem : TFileItem;
    FPaused : Boolean;
    FPauseLock : TThreadList;
    FConfig : TConfig;

    procedure DoProgress;
    procedure DoNotify;
    function GetPaused: Boolean;
    procedure SetPaused(AValue: Boolean);
    procedure UpdateCount;
  public
    constructor Create(aLog : ILog; aFileList : TItemList; aConfig : TConfig;
                       aNotify : TLibraryNotify;
                       aTerminate : TNotifyEvent;
                       aProgress : TSearchFileProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
    property Paused : Boolean read GetPaused write SetPaused;
  end;

implementation

uses
{$if defined(Darwin) or Defined(MsWindows)}
  mysql80conn, SQLDB,
  DB, SQLite3DS,
{$endif}
  Graphics;

{ TThreadScrub }

constructor TThreadScrub.Create(aLog : ILog; aFileList: TItemList; aConfig : TConfig;
                               aNotify : TLibraryNotify;
                               aTerminate : TNotifyEvent;
                               aProgress : TSearchFileProgressEvent = nil);
begin
  FPaused := False;
  Flog := aLog;
  FItem := nil;
  FFileList := aFileList;
  FNotifier:=aNotify;
  FreeOnTerminate:=True;
  FPRogress := aProgress;
  OnTerminate:= aTerminate;
  FPauseLock := TThreadList.Create;
  FConfig := aConfig;
  inherited Create(False);
  Priority:=tpLower;
end;

destructor TThreadScrub.Destroy;
begin
  FLog := nil;
  FPauseLock.Free;
  inherited Destroy;
end;

procedure TThreadScrub.DoProgress;
begin
  if Assigned(FProgress) then
  begin
    if (FVal < FCnt - 1) and (Fcnt > 0) then
      FProgress(Self, 1, 0, 0, Format('Scrub:%s - Albums:%d - Deleted:%d - Synced:%d',
                                     [IntToStr((FVal * 100) div FCnt) + '%',
                                      FCnt, FDeleted, FSynced]))
    //FProgress(Self, 1, 0, 0, 'Scrubing stamps : ' +
      //          IntToStr((FVal * 100) div FFileList.Count) + '%')
    else
      FProgress(Self, 1, 0, 0, 'Scrub Done. (' + TimeToStr(now) + ')');
  end;
end;

procedure TThreadScrub.DoNotify;
begin
  FNotifier(Self, laDelete, FItem);
end;

function TThreadScrub.GetPaused: Boolean;
begin
  with FPauseLock, LockList do
  try
    result := FPaused;
  finally
    UnlockList;
  end;
end;

procedure TThreadScrub.SetPaused(AValue: Boolean);
begin
  with FPauseLock, LockList do
  try
    FPaused := AValue;
  finally
    UnlockList;
  end;
end;

procedure TThreadScrub.UpdateCount;
begin
  FCnt := FFileList.Count;
end;

procedure TThreadScrub.Execute;
var
  r : integer;
{$if defined(Darwin) or Defined(MsWindows)}
    Sqlite3Dataset1: TSqlite3Dataset;
{$endif}

  procedure _DeleteItem;
  begin
    with FItem do
      if not FileExists(Filename) then
      begin
        FItem.Deleted:=True;
        if Assigned(FNotifier) then
          Synchronize(@DoNotify);
        Fitem.SyncFileDelete;
        inc(FDeleted);
        FLog.Log('TThreadScrub.Execute: Deleted:' + Filename);
      end;
  end;

  procedure GetReadStatesFromYAcLib;
  var
    Ylibs : TStringlist;
    i,j : integer;
    s : string;
  begin
{$if defined(Darwin) or Defined(MsWindows)}
    Ylibs := TStringlist.Create;
    Sqlite3Dataset1 := TSqlite3Dataset.Create(nil);
    try
      FConfig.GetYacLibs(Ylibs);
      Sqlite3Dataset1.Name := 'Sqlite3Dataset1';
      for j := 0 to Ylibs.Count - 1 do
      begin
        if Sqlite3Dataset1.Active then
          Sqlite3Dataset1.Close;
        if not FileExists(Ylibs[j]) then
          continue;

        Sqlite3Dataset1.FileName:= Ylibs[j];
        with Sqlite3Dataset1 do
        try
          Sql := 'select c."path", i."read", i.currentPage from comic c ' +
                 'join comic_info i on c.comicInfoId = i.id ' +
                 'where i."read" = 1 or i.hasBeenOpened = 1';

          Open;

          while not eof do
          begin
            {$if defined(Darwin) or defined(Linux)}
            s := RemoveDiacritics(FieldByName('path').AsString);
            {$else}
            s := RemoveDiacritics(FieldByName('path').AsString.Replace('/', '\'));
            {$endif}
            for i := 0 to FFileList.Count - 1 do
              if RemoveDiacritics(FFileList[i]).EndsWith(s) then
                if FileExists(FFileList[i]) then
                   with TFileItem(FFileList.Objects[i]) do
                   begin
                     if not ReadState and FieldByName('read').AsBoolean then
                       ReadState := true;

                     if not ReadState and (CurPage < FieldByName('currentPage').AsInteger) then
                       CurPage := FieldByName('currentPage').AsInteger;

                     break;
                   end;

            Next;
          end;

        finally
          if Active then
            Close;
        end;
      end;
    finally
      Sqlite3Dataset1.Free;
      Ylibs.Free;
    end;
{$ENDIF}
  end;

begin
  Sleep(2000);
  while not Terminated do
  try
    //if (FFileList.StampCount <> FFileList.Count) or
    //   (FFileList.DeletedCount > 0) then
    begin
      FLog.Log('TThreadScrub.Execute: Starting scrub.');
      //Synchronize(@UpdateCount);
      UpdateCount;
      FVal := 0;
      Fsynced := 0;
      FDeleted := 0;

      //FFileList.Cleanup;

      while (FCnt > FVal) do
      begin
        if Terminated then
            Exit;

        if Paused then
        begin
          yield;
          continue;
        end;

        // remove invalid entries
        FItem := TFileItem(FFileList.Objects[FVal]);
        if Assigned(FItem) then
          if not FItem.Deleted then
          begin
            if not FileExists(FItem.Filename) then
              _DeleteItem
    {$ifdef Library}
            else
            // sync needed
            if FFileList.SyncPath.Length > 0 then
            begin
              r := TFileItem(FFileList.Objects[FVal]).CheckSync;
              if r < 0 then
                _DeleteItem
              else
              if r >= 1 then
                inc(FSynced);
            end
    {$endif};

            if Terminated then
              Exit;

            {$if defined(MsWindows) or defined(Linux)}
            // make stamp if needed
            with TFileItem(FFileList.Objects[FVal]) do
              GenerateStamp;
            {$endif}
          end
        else
        // delete sync file
        if FFileList.SyncPath.Length > 0 then
          TFileItem(FFileList.Objects[FVal]).SyncFileDelete;

        inc(FVal);

        UpdateCount;
        //Synchronize(@UpdateCount);

        if (FVal = 1) or ((FVal mod 10) = 0) then
          Synchronize(@DoProgress);
          //yield;

        //Sleep(10);
        if Terminated then
            Exit;

        //
      end;
      // Get read states from yaclibs
      try
        GetReadStatesFromYAcLib;
        UpdateCount;
      except
      end;

      Synchronize(@DoProgress);
      FLog.Log('TThreadScrub.Execute: Scrub done.');
      FFileList.Save;
      Terminate;
    end;
   // else
   //   Sleep(2000);
  except
    on e: Exception do
      FLog.Log('TThreadScrub.Execute Error: ' + E.Message);
  end;
end;


end.

