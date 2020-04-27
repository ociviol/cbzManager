unit uConfig;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils.Json
{$if defined(Linux) or defined(Darwin)}
  ,cthreads
{$endif}
  ;

type

  { TConfig }

  TConfig = Class(TJsonObject)
  private
    FAlbumArt: Boolean;
    FBdPathPath: String;
    FBlog,
    FDeleteFile,
    FShowStats,
    FHighPerf: Boolean;
    FLibPath: String;
    Fcwebp,
    Funrar,
    Fp7zip: String;
    FQueueSize,
    FNbThreads,
    Fleft,
    FTop,
    FWidth,
    FHeight,
    FWebpQuality,
    FLleft,
    FLHeight: Integer;
    FLWidth: Integer;
    FLTop: Integer;
    FTreeViewWidth :Integer;
  public
    constructor Create;
    class function Load(const aFileName : String):TConfig;
  published
    property MngrLeft : Integer read Fleft write Fleft;
    property MngrTop : Integer read FTop write FTop;
    property MngrWidth : Integer read FWidth write FWidth;
    property MngrHeight : Integer read FHeight write FHeight;
    property MngrTreeViewWidth : Integer read FTreeViewWidth write FTreeViewWidth;
    property Libraryleft : Integer read FLleft write FLleft;
    property LibraryTop : Integer read FLTop write FLTop;
    property LibraryWidth : Integer read FLWidth write FLWidth;
    property LibraryHeight : Integer read FLHeight write FLHeight;
    property DoLog : Boolean read FBlog write FBlog;
    property BdPathPath: String read FBdPathPath write FBdPathPath;
    property cwebp: String read Fcwebp write Fcwebp;
    property unrar: String read Funrar write Funrar;
    property p7zip: String read Fp7zip write Fp7zip;
    property QueueSize : Integer read FQueueSize write FQueueSize;
    property HighPerf : Boolean read FHighPerf write FHighPerf;
    property NbThreads : Integer read FNbThreads write FNbThreads;
    property WebpQuality : Integer read FWebpQuality write FWebpQuality;
    property DeleteFile : Boolean read FDeleteFile write FDeleteFile;
    property DoAlbumart : Boolean read FAlbumArt write FAlbumart;
    property ShowStats : Boolean read FShowStats write FShowStats;
    property LibPath : String read FLibPath write FLibPath;
  end;

implementation

{$ifdef Mswindows}
uses
  Forms;
{$endif}

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
  HighPerf:= False;
  FWebpQuality := 75;
  FDeleteFile := False;
  FAlbumart := False;
  FShowStats := False;
end;

class function TConfig.Load(const aFileName: String): TConfig;
begin
  result := TConfig(TJsonObject.Load(aFilename, TConfig.Create));
end;


end.

