unit uThreadFill;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, uLibraryClasses, Utils.Searchfiles, Utils.Logger, Utils.Strings;

type
  TFillSettings = Record
    FFileList : TItemList;
    FVisibleList: TThreadStringlist;
    FCurrentPath: String;
    FLvl: Integer;
    FDisplayFilters: TDisplayFilters;
    FDate : TDateTime;
  end;

  { TThreadFill }

  TThreadFill = Class(TThread)
  private
    FFillSettings : TFillSettings;
    FProgress : TSearchFileProgressEvent;
    FProgressChar : Char;
    FCancelled : Boolean;
    Flog : ILog;

    procedure DoProgress;
  public
    constructor Create(aLog : ILog; aFillSettings : TFillSettings;
                       aOnTerminate : TNotifyEvent;
                       aProgress : TSearchFileProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
    property Cancelled : Boolean read FCancelled;
  end;


implementation

uses
  StrUtils, Utils.Files, DateUtils;


{ TThreadFill }

constructor TThreadFill.Create(aLog : ILog; aFillSettings : TFillSettings;
                               aOnTerminate: TNotifyEvent; aProgress: TSearchFileProgressEvent);
begin
  Flog := aLog;
  FFillSettings := aFillSettings;
  FreeOnTerminate:=True;
  OnTerminate:=aOnTerminate;
  FProgress := aProgress;
  FProgressChar := '|';
  FCancelled:=FAlse;

  inherited Create(False);
end;

destructor TThreadFill.Destroy;
begin
  Flog := nil;
  inherited Destroy;
end;

procedure TThreadFill.DoProgress;
begin
  case FProgressChar of
    '|': FProgressChar := '/';
    '/': FProgressChar := '-';
    '-': FProgressChar := '\';
    '\': FProgressChar := '|';
  end;
  FProgress(Self, 0, 0, 0, ifthen(Terminated, 'Fill done.', 'Loading folder ' + FProgressChar));
end;

procedure TThreadFill.Execute;
var
  i : integer;
  s : string;
  fi : TFileItem;
begin
  while not Terminated do
  try
    with FFillSettings do
      for i := 0 to FFileList.Count - 1 do
      begin
        if Terminated then
        begin
          FCancelled:=True;
          Exit;
        end;

        if FFileList[i].StartsWith(IncludeTrailingPathDelimiter(FCurrentPath)) then
        begin
          s := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileList[i]));
          if Length(s.Split([PathDelim])) > FLvl then
            s := GetFirstPath(s, FLvl)
          else
            s := FFileList[i];

          fi := TFileItem(FFileList.Objects[i]);

          if not FileExists(fi.Filename) then
            fi.Deleted:=True;

          if (dfUnread in FDisplayFilters) then
            if fi.ReadState then
              continue;

          if not fi.Deleted then
          begin
            if FDate > 0 then
              if  Daysbetween(Fdate, fi.DateAdded) <> 0 then
                continue;

            with FVisibleList do
              //if FileExists(s) then //and (GetLevel(ExtractFilePath(s)) <= GetLevel(FCurrentPath) + 1) then
              //  AddObject(s, fi)
              //else
              if (IndexOf(s) < 0) then //and (GetLevel(ExtractFilePath(s)) <= GetLevel(FCurrentPath) + 1) then
                AddObject(s, fi);
          end;

          if (i mod 250) = 0 then
          begin
            Synchronize(@DoProgress);
            //Sleep(10);
          end;
        end;
      end;

    FFillSettings.FVisibleList.Sort;
    Terminate;
    Synchronize(@DoProgress);
  except
    on e: Exception do
    begin
      Flog.Log('TThreadFill.Execute: Error:' + E.Message);
      Terminate;
    end;
  end;
end;


end.

