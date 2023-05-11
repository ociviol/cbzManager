unit uThreadScrub;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uLibraryClasses, utils.Logger, Utils.Searchfiles;

type
  TLibrayAction = (laAdd, laDelete);
  TLibraryNotify = procedure(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil) of object;

  { TThreadScrub }

  TThreadScrub = Class(TThread)
  private
    FVal,
    FCnt,
    FDeleted,
    FStampCount,
    Fsynced : Integer;
    FFileList: TItemList;
    FProgress : TSearchFileProgressEvent;
    FLog : ILog;
    FNotifier : TLibraryNotify;
    FItem : TFileItem;

    procedure DoProgress;
    procedure DoNotify;
    procedure UpdateCount;
  public
    constructor Create(aLog : ILog; aFileList : TItemList;
                       aNotify : TLibraryNotify;
                       aTerminate : TNotifyEvent;
                       aProgress : TSearchFileProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  Graphics;

{ TThreadScrub }

constructor TThreadScrub.Create(aLog : ILog; aFileList: TItemList;
                               aNotify : TLibraryNotify;
                               aTerminate : TNotifyEvent;
                               aProgress : TSearchFileProgressEvent = nil);
begin
  Flog := aLog;
  FItem := nil;
  FFileList := aFileList;
  FNotifier:=aNotify;
  FreeOnTerminate:=True;
  FPRogress := aProgress;
  OnTerminate:= aTerminate;
  inherited Create(False);
  Priority:=tpLower;
end;

destructor TThreadScrub.Destroy;
begin
  FLog := nil;
  inherited Destroy;
end;

procedure TThreadScrub.DoProgress;
begin
  if Assigned(FProgress) then
  begin
    if (FVal < FCnt - 1) and (Fcnt > 0) then
      FProgress(Self, 1, 0, 0, Format('Scrub:%s - Albums:%d - Stamps:%d - Deleted:%d - Synced:%d',
                                     [IntToStr((FVal * 100) div FCnt) + '%',
                                      FCnt, FStampCount, FDeleted, FSynced]))
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

procedure TThreadScrub.UpdateCount;
begin
  FCnt := FFileList.Count;
  FStampCount := FFileList.StampCount;
end;

procedure TThreadScrub.Execute;
var
  r : integer;
  b : TBitmap;

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
              if r = 1 then
                inc(FSynced);
            end
            {$endif};

            if Terminated then
              Exit;

            // make stamp if needed
            with TFileItem(FFileList.Objects[FVal]) do
            begin
              b := GenerateStamp;
              if Assigned(b) then
                b.Free;
            end;
          end;

        inc(FVal);

        UpdateCount;
        //Synchronize(@UpdateCount);

        if (FVal mod 50) = 0 then
          Synchronize(@DoProgress);
          //yield;

        //Sleep(10);
        if Terminated then
            Exit;

        //
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

