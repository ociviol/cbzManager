unit uConfig;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, utils.Json
{$if defined(Linux) or defined(Darwin)}
  ,cthreads
{$endif}
  ;

type

  { TConfig }

  TConfig = Class(TJsonObject)
  private
    FAlbumArt,
    FBlog,
    FDeleteFile,
    FShowStats,
    FLHideRead,
    FOpenLibrary: Boolean;
    FLibCurPath: String;

    FWindowStates: TStringlist;
    FLibPath,
    Fcwebp,
    Funrar,
    FBdPathPath,
    FSyncPath,
    Fp7zip: String;

    FQueueSize,
    FNbThreads,
    FWebpQuality,
    FTreeViewWidth :Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function Load(const aFileName : String):TConfig;
    procedure SaveForm(aOwner : TForm);
    procedure RestoreForm(aOwner : TForm);
  published
    property WindowStates : TStringlist read FWindowStates write FWindowStates;
    property MngrTreeViewWidth : Integer read FTreeViewWidth write FTreeViewWidth;
    property LibraryHideRead : Boolean read FLHideRead write FLHideRead;
    property DoLog : Boolean read FBlog write FBlog;
    property BdPathPath: String read FBdPathPath write FBdPathPath;
    property cwebp: String read Fcwebp write Fcwebp;
    property unrar: String read Funrar write Funrar;
    property p7zip: String read Fp7zip write Fp7zip;
    property QueueSize : Integer read FQueueSize write FQueueSize;
    property NbThreads : Integer read FNbThreads write FNbThreads;
    property WebpQuality : Integer read FWebpQuality write FWebpQuality;
    property DeleteFile : Boolean read FDeleteFile write FDeleteFile;
    property DoAlbumart : Boolean read FAlbumArt write FAlbumart;
    property ShowStats : Boolean read FShowStats write FShowStats;
    property LibPath : String read FLibPath write FLibPath;
    property SyncPath : String read FSyncPath write FSyncPath;
    property OpenLibrary : Boolean read FOpenLibrary write FOpenLibrary;
    property LibCurPath : String read FLibCurPath write FLibCurPath;
  end;

implementation

{ TConfig }

constructor TConfig.Create;
begin
  inherited;
{$if defined(Darwin)}
  Fcwebp := '/usr/local/bin/cwebp';
  Fp7zip := '/usr/local/bin/7z';
  Funrar := '/usr/local/bin/unrar';
{$elseif defined(Linux)}
  Fcwebp := '/usr/bin/cwebp';
  Fp7zip := '/usr/bin/7z';
  Funrar := '/usr/bin/unrar';
{$else}
  Fcwebp := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + {$ifdef DEBUG} 'Bin-Win\' + {$endif}'cwebp.exe';
  Fp7zip := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + {$ifdef DEBUG} 'Bin-Win\' + {$endif}'7z.exe';
  Funrar := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + {$ifdef DEBUG} 'Bin-Win\' + {$endif}'unrar.exe';
{$endif}
  FWindowStates := TStringlist.Create;
  FQueueSize:=2;
  FNbThreads := 8;
  FWebpQuality := 75;
  FDeleteFile := False;
  FAlbumart := False;
  FShowStats := False;
  FOpenLibrary := False;
  FSyncPath := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Library\';
end;

destructor TConfig.Destroy;
begin
  FWindowStates.Free;
  inherited Destroy;
end;

class function TConfig.Load(const aFileName: String): TConfig;
begin
  result := TConfig(TJsonObject.Load(aFilename, TConfig.Create));
end;

procedure TConfig.SaveForm(aOwner: TForm);
begin
  with FWindowStates do
  begin
    Values[Format('%sState', [aOwner.Name])] := WindowStateToStr(aOwner.WindowState);
    Values[Format('%sLeft', [aOwner.Name])] := IntToStr(aOwner.Left);
    Values[Format('%sTop', [aOwner.Name])] := IntToStr(aOwner.Top);
    Values[Format('%sWidth', [aOwner.Name])] := IntToStr(aOwner.Width);
    Values[Format('%sHeight', [aOwner.Name])] := IntToStr(aOwner.Height);
  end;
end;

procedure TConfig.RestoreForm(aOwner: TForm);
var
  s : string;
begin
  with FWindowStates do
  begin
    s := Values[Format('%sState', [aOwner.Name])];
    if s <> '' then aOwner.WindowState:=StrToWindowState(s);

    if aOwner.WindowState <> wsMaximized then
    begin
      s := Values[Format('%sLeft', [aOwner.Name])];
      if s <> '' then aOwner.Left := StrToInt(s);
      s := Values[Format('%sTop', [aOwner.Name])];
      if s <> '' then aOwner.Top := StrToInt(s);
      s := Values[Format('%sWidth', [aOwner.Name])];
      if s <> '' then aOwner.Width := StrToInt(s);
      s := Values[Format('%sHeight', [aOwner.Name])];
      if s <> '' then aOwner.Height := StrToInt(s);
    end;
  end;
end;


end.

