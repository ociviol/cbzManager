unit uVersionThread;

{$mode ObjFPC}{$H+}

interface

 uses Classes, utils.Logger;

type
 TProgramVersion = (pvCbzManager, pvCbzLibrary);

 TThreadCheckVersion = Class(TThread)
 protected
   FNeedUpdate : Boolean;
   FUpdateVersion : String;
   FProgramVersion : TProgramVersion;
   Flog : ILog;
 public
   constructor Create(aProgramVersion : TProgramVersion; Log : ILog; aTerminate : TNotifyEvent);
   destructor Destroy; override;
   procedure Execute; override;
   property NeedUpdate:boolean read FNeedUpdate;
 end;


implementation

uses
  fpHttpClient, uLogReader,
  {$if Defined(MsWindows)}
   opensslsockets,
  {$endif}
  {$if defined(Darwin) or defined(Linux)}
   unix,
  {$endif}
  sysutils,
  utils.SoftwareVersion
  ;

{ TThreadCheckVersion }

constructor TThreadCheckVersion.Create(aProgramVersion : TProgramVersion; Log : ILog; aTerminate: TNotifyEvent);
begin
  Flog := Log;
  FNeedUpdate := false;
  FProgramVersion := aProgramVersion;
  OnTerminate := aTerminate;
  FreeOnTerminate:=True;
  inherited Create(false);
end;

destructor TThreadCheckVersion.Destroy;
begin
  Flog := nil;
  inherited Destroy;
end;

procedure TThreadCheckVersion.Execute;
var
  t : TStringList;
  i : integer;
  s : string;

  function GetProgramVersion:string;
  begin
    if FProgramVersion = pvCbzManager then
      result := copy(FUpdateVersion, 1, pos(';', FUpdateVersion)-1)
    else
      result := copy(FUpdateVersion, pos(';', FUpdateVersion)+1, length(FUpdateVersion));
  end;
begin
  try
    with TFPHTTPClient.Create(nil) do
    try
      t := TStringList.Create;
      try
        SimpleGet('https://ollivierciviolsoftware.wordpress.com/version/', t);
        s := '';
        FUpdateVersion := '';

        for i:=0 to t.Count - 1 do
          if t[i].Contains('content="win:x64=') then
          begin
            s := copy(t[i], pos('content="win:x64=', t[i]) + 9, length(t[i]));
            s := copy(s, 1, pos('"', s)-1);
            break;
          end;
          // s = win:x64=1.0.1.14 win:x86=1.0.1.14 linux:1.0.1.38 osx:1.0.1.35 winos:1.0.1.141
      finally
        t.Free;
      end;

      if s <> '' then
      begin
      {$if defined(Darwin)}
        FUpdateVersion := copy(s, pos('osx:', s)+4, length(s));
        if pos(' ', FUpdateVersion) > 0 then
          FUpdateVersion := copy(FUpdateVersion, 1, pos(' ', FUpdateVersion)-1);
      {$ELSEif Defined(Linux)}
        FUpdateVersion := copy(s, pos('linux:', s)+6, length(s));
        if pos(' ', FUpdateVersion) > 0 then
          FUpdateVersion := copy(FUpdateVersion, 1, pos(' ', FUpdateVersion)-1);
      {$ELSEif Defined(MsWindows)}
        FUpdateVersion := Trim(copy(s, pos('winos:', s)+6, length(s)));
      {$ENDIF}
      end;

      if FUpdateVersion <> '' then
      begin
        if pos(';', FUpdateVersion) > 0 then
          FUpdateVersion := GetProgramVersion;

        FNeedUpdate := CompareVersion(GetFileVersion, FUpdateVersion) > 0;
      end;

    finally
      Free;
    end;
  except
    on e: Exception do
      Flog.Log('TThreadCheckVersion.Execute : Error : ' + E.Message);
  end;

  Terminate;
end;

end.

