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
    FBlog,
    FLHideRead,
    FAutoSyncYac: Boolean;

    FLibCurPath,
    FLibPath,
    FSyncPath,
    FConfigPath: String;

    FDefaultColWidth,
    FDefaultRowHeight:Integer;
  public
    constructor Create;
    class function Load(const aFileName : String):TConfig;
    procedure SaveForm(aOwner : TForm);
    procedure RestoreForm(aOwner : TForm);
    function GetConfigPath:string;
    procedure SaveYacLibs(Items : TStrings);
    procedure GetYacLibs(Items : TStrings);
  published
    property DefaultColWidth : integer read FDefaultColWidth write FDefaultColWidth;
    property DefaultRowHeight : integer read FDefaultRowHeight write FDefaultRowHeight;
    property HideRead : Boolean read FLHideRead write FLHideRead;
    property DoLog : Boolean read FBlog write FBlog;
    property LibPath : String read FLibPath write FLibPath;
    property SyncPath : String read FSyncPath write FSyncPath;
    property LibCurPath : String read FLibCurPath write FLibCurPath;
    property AutoSyncYac : boolean read FAutoSyncYac write FAutoSyncYac default false;
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

function TConfig.GetConfigPath: string;
begin
  result := FConfigPath;
end;

procedure TConfig.SaveYacLibs(Items: TStrings);
var
  ini : TIniFile;
  cnt, i : integer;
  s : string;
begin
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FConfigPath) + 'yaclibs.ini');
  try
    cnt := Items.Count;
    ini.WriteInteger('libs', 'count', cnt);
    for i := 0 to cnt - 1 do
      ini.WriteString('libs', 'lib'+inttostr(i+1), Items[i]);
  finally
    ini.free;
  end;
end;

procedure TConfig.GetYacLibs(Items: TStrings);
var
  ini : TIniFile;
  cnt, i : integer;
  s : string;
begin
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FConfigPath) + 'yaclibs.ini');
  try
    cnt := ini.ReadInteger('libs', 'count', 0);
    for i := 0 to cnt - 1 do
    begin
      s := ini.ReadString('libs', 'lib'+inttostr(i+1), '');
      Items.Add(s);
    end;
  finally
    ini.free;
  end;
end;

end.

