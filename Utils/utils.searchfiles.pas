unit Utils.Searchfiles;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls
{$if defined(Linux) or defined(Darwin)}
  , cthreads
{$endif}
  ;

type
  TFilterPredicate = function(const Path: string; const SearchRec: TSearchRec): Boolean;
  TFoundFileCallback = function(const Filename : String; IsFolder : Boolean = False):TTreenode of object;
  TProgressEvent = procedure(Sender : TObject; const ProgressID : QWord; const Pos, Max : integer; const Msg : String = '') of object;
  TSearchFileOption = (sfoRecurse, sfoNoFiles, sfoFolders);
  TSearchFileOptions = set of TSearchFileOption;

function ThreadedSearchFiles(const Path, Masks : String;
                             CallBack : TFoundFileCallback;
                             Terminate : TNotifyEvent;
                             OnProgress : TProgressEvent = nil;
                             const str_scanning : string = '';
                             SearchFileOptions : TSearchFileOptions = [sfoRecurse]):TThread;

procedure GetDirectories(const Path : String; var Dirs : TStringArray);
procedure GetFiles(const Path, sMasks : String; Files : TStringList);
procedure GetFiles(const Path : string; Masks : Array of String; Files : TStringList);

implementation

uses
  Utils.Masks, Utils.NaturalSortStringList;

type
  TThreadSearchFiles = Class(TThread)
  private
    FPath,
    FMasks,
    Fstr_scanning : String;
    FCallBack : TFoundFileCallback;
    FOnProgress : TProgressEvent;
    FOptions : TSearchFileOptions;
    ProgressID : QWord;
    FCur, FMax : Integer;
    FMsg : String;
    FFile : String;
    procedure DoProgress;
    procedure DoCallBackTrue;
    procedure DoCallBackFalse;
  public
    constructor Create(const Path, Masks : String;
                       CallBack : TFoundFileCallback;
                       WhenTerminate : TNotifyEvent;
                       OnProgress : TProgressEvent = nil;
                       const str_scanning : string = '';
                       SearchFileOptions : TSearchFileOptions = [sfoRecurse]);
    procedure Execute; override;
  End;



function ThreadedSearchFiles(const Path, Masks : String;
                             CallBack : TFoundFileCallback;
                             Terminate : TNotifyEvent;
                             OnProgress : TProgressEvent = nil;
                             const str_scanning : string = '';
                             SearchFileOptions : TSearchFileOptions = [sfoRecurse]):TThread;
begin
  result := TThreadSearchFiles.Create(Path, Masks, CallBack, Terminate, OnProgress, str_scanning, SearchFileOptions);
end;

{ TThreadSearchFiles }

constructor TThreadSearchFiles.Create(const Path, Masks : String;
                                      CallBack : TFoundFileCallback;
                                      WhenTerminate : TNotifyEvent;
                                      OnProgress : TProgressEvent = nil;
                                      const str_scanning : string = '';
                                      SearchFileOptions : TSearchFileOptions = [sfoRecurse]);
begin
  FPath := Path;
  FMasks := Masks;
  FOnProgress := OnProgress;
  FCallBack := CallBack;
  OnTerminate := WhenTerminate;
  FreeOnTerminate := True;
  Fstr_scanning := str_scanning;
  FOptions := SearchFileOptions;
  inherited Create(False);
end;

procedure GetDirectories(const Path : String; var Dirs : TStringArray);
var
  sr : TRawByteSearchRec;
  sPath : String;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          GetDirectories(IncludeTrailingPathDelimiter(Path) + sr.Name, Dirs);
          SetLength(Dirs, length(Dirs)+1);
          Dirs[length(Dirs)-1] := IncludeTrailingPathDelimiter(Path) + sr.Name;
        end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

procedure GetFiles(const Path, sMasks : String; Files : TStringList);
var
  Masks : TStringArray;
  t : TNaturalSortStringList;
begin
  Masks := sMasks.Split([';']);
  GetFiles(Path, Masks, Files);

  t := TNaturalSortStringList.Create;
  with t do
  try
    Assign(Files);
    Sort;
    Files.Assign(t);
  finally
    Free;
  end;
end;

procedure GetFiles(const Path : string; Masks : Array of String; Files : TStringList);
var
  sr : TRawByteSearchRec;
  spath : string;
  s : string;
begin
  sPath := IncludeTrailingPathDelimiter(Path) + '*';
  if FindFirst (sPath, faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
      begin
         if (sr.Name <> '.') and (sr.Name <> '..') then
            GetFiles(IncludeTrailingPathDelimiter(Path) + sr.Name, Masks, Files);
      end
      else
        for s in Masks do
        begin
          if MatchesMask(sr.Name, s) then
          begin
            Files.Add(IncludeTrailingPathDelimiter(Path) + sr.Name);
            break;
          end;
        end;

    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end
end;

procedure TThreadSearchFiles.DoProgress;
begin
  FOnProgress(nil, ProgressID, FCur, FMax, FMsg);
end;

procedure TThreadSearchFiles.DoCallBackTrue;
begin
  FCallBack(FFile, True);
end;

procedure TThreadSearchFiles.DoCallBackFalse;
begin
  FCallBack(FFile);
end;

procedure TThreadSearchFiles.Execute;
var
  files : TSTringList;
  dirs : TStringArray;
  z : integer;
begin
  while not Terminated do
  try
    try
      ProgressID := QWord(ThreadID) + GetTickCount64;

      FCur := 0;
      FMax := 1;
      FMsg := Fstr_scanning;
      if Assigned(FOnProgress) then
        Synchronize(@DoProgress);

      if sfoFolders in FOptions then
      begin
        SetLength(Dirs, 0);
        GetDirectories(FPath, Dirs);
        for z := Low(Dirs) to High(Dirs) do
        begin
          if Terminated then
            Exit;

          try
            FMsg := Fstr_scanning +' (' + FPath + ')';
            FFile := Dirs[z];
            Synchronize(@DoCallBackTrue);
            Synchronize(@DoProgress);
            Sleep(10);
          except
          end;
        end;
      end;

      if Terminated then
        Exit;

      if not (sfoNoFiles in FOptions) then
      begin
        Files := TSTringList.Create;
        try
          GetFiles(FPath, FMasks, Files);
          FMax := Files.Count-1;

          for z := 0 to files.Count - 1 do
          begin
            if Terminated then
              Exit;

            try
              FCur := z;
              FMsg := Fstr_scanning +' (' + Files[z] + ')';
              FFile := Files[z];
              Synchronize(@DoCallBackFalse);
              Synchronize(@DoProgress);
              Sleep(10);
            except
            end;
          end;
        finally
          Files.FRee;
        end;
      end;
    finally
      FCur := 0;
      FMax := 0;
      if Assigned(FOnProgress) then
        Synchronize(@&DoProgress);
    end;
    Terminate;
  except
    Terminate;
  end;
end;

end.

