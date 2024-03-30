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
    FShowStats: Boolean;

    Fcwebp,
    Funrar,
    FBdPathPath,
    Fp7zip,
    FConfigPath: String;

    FQueueSize,
    FNbThreads,
    FWebpQuality,
    FDefaultColWidth,
    FDefaultRowHeight,
    FTreeViewWidth :Integer;
  public
    constructor Create;
    class function Load(const aFileName : String):TConfig;
    procedure SaveForm(aOwner : TForm);
    procedure RestoreForm(aOwner : TForm);
  published
    property DefaultColWidth : integer read FDefaultColWidth write FDefaultColWidth;
    property DefaultRowHeight : integer read FDefaultRowHeight write FDefaultRowHeight;
    property MngrTreeViewWidth : Integer read FTreeViewWidth write FTreeViewWidth;
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
  end;

implementation

uses
  Math, IniFiles;

const
  SZ_SIZE_INI = '.size.ini';

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
  Fcwebp := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'cwebp.exe';
  Fp7zip := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '7z.exe';
  Funrar := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'unrar.exe';
{$endif}
  FQueueSize:=2;
  FNbThreads := 8;
  FWebpQuality := 75;
  FDeleteFile := False;
  FAlbumart := False;
  FShowStats := False;
  FDefaultColWidth := 140;
  FDefaultRowHeight := 180;
end;

class function TConfig.Load(const aFileName: String): TConfig;
begin
  result := TConfig(TJsonObject.Load(aFilename, TConfig.Create));
  result.FConfigPath := ExtractFilePath(aFileName);
end;

procedure TConfig.SaveForm(aOwner: TForm);
var
  FWindowStateStr : string;
  ini : TIniFile;
begin
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FConfigPath) + aOwner.ClassName + SZ_SIZE_INI);
  try
    ini.WriteString('pos', 'state', WindowStateToStr(aOwner.WindowState));
    if aOwner.WindowState = wsNormal then
    begin
      ini.WriteInteger('pos', 'left', aOwner.Left);
      ini.WriteInteger('pos', 'top', aOwner.Top);
      ini.WriteInteger('pos', 'width', aOwner.Width);
      ini.WriteInteger('pos', 'height', aOwner.Height);
    end;
  finally
    ini.free;
  end;
end;

procedure TConfig.RestoreForm(aOwner: TForm);
var
  FWindowStateStr : string;
  ini : TIniFile;
begin
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FConfigPath) + aOwner.ClassName + SZ_SIZE_INI);
  try
    aOwner.left := ini.ReadInteger('pos', 'left', aOwner.Left);
    aOwner.top := ini.ReadInteger('pos', 'top', aOwner.Left);
    aOwner.width := ini.ReadInteger('pos', 'width', aOwner.Left);
    aOwner.height := ini.ReadInteger('pos', 'height', aOwner.Left);

    FWindowStateStr := ini.ReadString('pos', 'state', '');
    if FWindowStateStr <> '' then
      aOwner.WindowState := StrToWindowState(FWindowStateStr);

  finally
    ini.free;
  end;
end;

end.

