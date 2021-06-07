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

    FWindowState,
    FLibCurPath,
    FLibPath,
    Fcwebp,
    Funrar,
    FBdPathPath,
    FSyncPath,
    Fp7zip: String;

    FQueueSize,
    FNbThreads,
    FWebpQuality,
    FMainLeft,
    FMainTop,
    FMainWidth,
    FMainHeight,
    FTreeViewWidth :Integer;
  public
    constructor Create;
    class function Load(const aFileName : String):TConfig;
    procedure SaveForm(aOwner : TForm);
    procedure RestoreForm(aOwner : TForm);
  published
    property WindowStateStr : String read FWindowState write FWindowState;
    property MainLeft : integer read FMainLeft write FMainLeft;
    property MainTop : integer read FMainTop write FMainTop;
    property MainWidth : integer read FMainWidth write FMainWidth;
    property MainHeight : integer read FMainHeight write FMainHeight;
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

uses
  Math;

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
  FQueueSize:=2;
  FNbThreads := 8;
  FWebpQuality := 75;
  FDeleteFile := False;
  FAlbumart := False;
  FShowStats := False;
  FOpenLibrary := False;
  FSyncPath := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Library\';
end;

class function TConfig.Load(const aFileName: String): TConfig;
begin
  result := TConfig(TJsonObject.Load(aFilename, TConfig.Create));
end;

procedure TConfig.SaveForm(aOwner: TForm);
begin
  with aOwner do
  begin
    FWindowState := WindowStateToStr(aOwner.WindowState);
    if WindowState = wsNormal then
    begin
      FMainLeft := aOwner.Left;
      FMainTop := aOwner.Top;
      FMainWidth := aOwner.Width;
      FMainHeight := aOwner.Height;
    end;
  end;
end;

procedure TConfig.RestoreForm(aOwner: TForm);
var
  s : string;
  r : TRect;
  i : integer;

begin
  with aOwner do
  begin
{$if defined(Darwin) or defined(Linux)}
    left := 100; // ifthen(FMainLeft > 0, FMainLeft, Left);
    top := 100; // ifthen(FMainTop > 0, FMainTop, Top);
    width := Screen.width - 300; // ifthen(FMainWidth > 0, FMainWidth, Width);
    height := Screen.Height - 300; // ifthen(FMainHeight > 0, FMainHeight, Height);
{$else}
    left := ifthen(FMainLeft > 0, FMainLeft, Left);
    top := ifthen(FMainTop > 0, FMainTop, Top);
    width := ifthen(FMainWidth > 0, FMainWidth, Width);
    height := ifthen(FMainHeight > 0, FMainHeight, Height);
{$endif}

    if FWindowState <> '' then
      WindowState := StrToWindowState(FWindowState);
  end;
end;


end.

