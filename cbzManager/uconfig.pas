unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrtti;

type

  { TConfig }

  TConfig = Class(Tpersistent)

  private
    FBdPathPath: String;
    FBlog: Boolean;
    Fcwebp: String;
    FHighPerf: Boolean;
    Fp7zip: String;
    FQueueSize,
    FNbThreads : Integer;
    Funrar: String;
    FWleft,
    FWTop,
    FWWidth,
    FWHeight,
    FWTreeViewWidth :Integer;
  public
    constructor Create;
    class function Load(const aFileName : String):TConfig;
    procedure Save(const aFileName : String);
  published
    property Blog : Boolean read FBlog write FBlog;
    property BdPathPath: String read FBdPathPath write FBdPathPath;
    property cwebp: String read Fcwebp write Fcwebp;
    property unrar: String read Funrar write Funrar;
    property p7zip: String read Fp7zip write Fp7zip;
    property QueueSize : Integer read FQueueSize write FQueueSize;
    property HighPerf : Boolean read FHighPerf write FHighPerf;
    property NbThreads : Integer read FNbThreads write FNbThreads;
    property Wleft : Integer read FWleft write FWleft;
    property WTop : Integer read FWTop write FWTop;
    property WWidth : Integer read FWWidth write FWWidth;
    property WHeight : Integer read FWHeight write FWHeight;
    property WTreeViewWidth : Integer read FWTreeViewWidth write FWTreeViewWidth;
  end;

implementation

{$ifdef Mswindows}
uses
  Forms;
{$endif}

{ TConfig }

constructor TConfig.Create;
begin
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
  FNbThreads := 4;
  HighPerf:= False;
end;

class function TConfig.Load(const aFileName: String): TConfig;
var
  DeStreamer: TJSONDeStreamer;
  t : TStringList;
begin
  result := TConfig.Create;
  try
    DeStreamer := TJSONDeStreamer.Create(nil);
    if FileExists(aFileName) then
    begin
      t := TStringList.Create;
      try
        t.LoadFromFile(aFileName);
        try
          DeStreamer.JSONToObject(t[0], result);
        finally
          DeStreamer.Free;
        end;

      finally
        t.Free;
      end
    end;
  except
  end;
end;

procedure TConfig.Save(const aFileName: String);
var
  Streamer: TJSONStreamer;
  s : String;
  t : TStringList;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoTStringsAsArray]; // Save strings as JSON array
    // JSON convert and output
    s := Streamer.ObjectToJSONString(Self);
    t := TStringList.Create;
    try
      t.add(s);
      t.SaveToFile(aFileName);
    finally
      t.free;
    end;
  finally
    Streamer.Free;
  end;
end;

end.

