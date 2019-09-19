// Delphi interface unit for UnRAR.dll
// Translated from unrar.h
// Use Delphi 2.0 and higher to compile this module
//
// Ported to Delphi by Eugene Kotlyarov, fidonet: 2:5058/26.9 ek@oris.ru
// Fixed version by Alexey Torgashin <alextp@mail.ru>, 2:5020/604.24@fidonet
//
// Revisions:
// Aug 2001 - changed call convention for TChangeVolProc and TProcessDataProc
//          - added RARGetDllVersion function, see comment below
//
// Jan 2002 - Added RARSetCallback  // eugene
//
// Oct 2002 - Added RARHeaderDataEx, RAROpenArchiveDataEx // eugene

unit UnRAR;

interface

uses
  Windows, SysUtils;

const
  ERAR_END_ARCHIVE    = 10;
  ERAR_NO_MEMORY      = 11;
  ERAR_BAD_DATA       = 12;
  ERAR_BAD_ARCHIVE    = 13;
  ERAR_UNKNOWN_FORMAT = 14;
  ERAR_EOPEN          = 15;
  ERAR_ECREATE        = 16;
  ERAR_ECLOSE         = 17;
  ERAR_EREAD          = 18;
  ERAR_EWRITE         = 19;
  ERAR_SMALL_BUF      = 20;
  ERAR_UNKNOWN        = 21;

  RAR_OM_LIST         =  0;
  RAR_OM_EXTRACT      =  1;

  RAR_SKIP            =  0;
  RAR_TEST            =  1;
  RAR_EXTRACT         =  2;

  RAR_VOL_ASK         =  0;
  RAR_VOL_NOTIFY      =  1;

  RAR_DLL_VERSION     =  3;

  UCM_CHANGEVOLUME    =  0;
  UCM_PROCESSDATA     =  1;
  UCM_NEEDPASSWORD    =  2;

type
  TUnrarCallback = function (Msg: UINT; UserData, P1, P2: Integer) :Integer; stdcall;

  RARHeaderData = packed record
    ArcName: packed array[0..Pred(260)] of AnsiChar;
    FileName: packed array[0..Pred(260)] of AnsiChar;
    Flags: UINT;
    PackSize: UINT;
    UnpSize: UINT;
    HostOS: UINT;
    FileCRC: UINT;
    FileTime: UINT;
    UnpVer: UINT;
    Method: UINT;
    FileAttr: UINT;
    CmtBuf: PAnsiChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
  end;

  RARHeaderDataEx = packed record
    ArcName: packed array [0..1023] of AnsiChar;
    ArcNameW: packed array [0..1023] of WideChar;
    FileName: packed array [0..1023] of AnsiChar;
    FileNameW: packed array [0..1023] of WideChar;
    Flags: UINT;
    PackSize: UINT;
    PackSizeHigh: UINT;
    UnpSize: UINT;
    UnpSizeHigh: UINT;
    HostOS: UINT;
    FileCRC: UINT;
    FileTime: UINT;
    UnpVer: UINT;
    Method: UINT;
    FileAttr: UINT;
    CmtBuf: PAnsiChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
    Reserved: packed array [0..1023] of UINT;
  end;

  RAROpenArchiveData = packed record
    ArcName: PAnsiChar;
    OpenMode: UINT;
    OpenResult: UINT;
    CmtBuf: PAnsiChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
  end;

  RAROpenArchiveDataEx = packed record
    ArcName: PAnsiChar;
    ArcNameW: PWideChar;
    OpenMode: UINT;
    OpenResult: UINT;
    CmtBuf: PAnsiChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
    Flags: UINT;
    Reserved: packed array [0..31] of UINT;
  end;

  // obsolete functions
  TChangeVolProc = function(ArcName: PAnsiChar; Mode: Integer): Integer; stdcall;
  TProcessDataProc = function(Addr: PUChar; Size: Integer): Integer; stdcall;

var
  RAROpenArchive : function(var ArchiveData: RAROpenArchiveData): THandle; stdcall;
  RAROpenArchiveEx : function(var ArchiveData: RAROpenArchiveDataEx): THandle; stdcall;
  RARCloseArchive : function(hArcData: THandle): Integer; stdcall;
  RARReadHeader : function(hArcData: THandle; var HeaderData: RARHeaderData): Integer; stdcall;
  RARReadHeaderEx : function(hArcData: THandle; var HeaderData: RARHeaderDataEx): Integer; stdcall;
  RARProcessFile : function(hArcData: THandle; Operation: Integer; DestPath, DestName: PAnsiChar): Integer; stdcall;
  RARSetCallback : procedure(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer); stdcall;
  RARSetPassword : procedure(hArcData: THandle; Password: PChar); stdcall;
  RARSetChangeVolProc : procedure(hArcData: THandle; ChangeVolProc: TChangeVolProc);stdcall;
  RARSetProcessDataProc : procedure(hArcData: THandle; ProcessDataProc: TProcessDataProc);stdcall;

// Wrapper for DLL's function - old unrar.dll doesn't export RARGetDllVersion
// Returns: -1 = DLL not found; 0 = old ver. (C-style callbacks); >0 = new ver.
function RARGetDllVersion: integer;


implementation

uses
  Forms, Dialogs;

type
  TRARGetDllVersion = function: integer; stdcall;

var
  DLLHAndle : THAndle;

procedure InitRar;
var
  DllName : String;
begin
  if DllHandle <> 0 then
    Exit;

{$ifdef WIN32}
  DllName := 'unrar.dll';
{$else}
  DllName := 'unrar64.dll';
{$endif}
  SetDllDirectory(PChar(ExtractFilePath(Application.Exename)));
  DllHandle := LoadLibrary(PChar(DllName));
  if DllHandle = 0 then
  begin
    ShowMessage(DllName + ' not found.' + sLineBreak + '(' +
                SysErrorMessage(GetLastError) + ')');
    ExitProcess(1);
  end;

  RAROpenArchive := GetProcAddress(DllHandle, 'RAROpenArchive');
  RAROpenArchiveEx := GetProcAddress(DllHandle, 'RAROpenArchiveEx');
  RARCloseArchive := GetProcAddress(DllHandle, 'RARCloseArchive');
  RARReadHeader := GetProcAddress(DllHandle, 'RARReadHeader');
  RARReadHeaderEx := GetProcAddress(DllHandle, 'RARReadHeaderEx');
  RARProcessFile := GetProcAddress(DllHandle, 'RARProcessFile');
  RARSetCallback := GetProcAddress(DllHandle, 'RARSetCallback');
  RARSetPassword := GetProcAddress(DllHandle, 'RARSetPassword');
  RARSetChangeVolProc := GetProcAddress(DllHandle, 'RARSetChangeVolProc');
  RARSetProcessDataProc := GetProcAddress(DllHandle, 'RARSetProcessDataProc');
end;

function RARGetDllVersion: integer;
var
  f: TRARGetDllVersion;
begin
  InitRar;

  f := GetProcAddress(DLLHAndle, 'RARGetDllVersion');
  if @f = nil then
    Result := 0
  else
    Result := f;
end;

initialization
  InitRar;

finalization
  if DllHandle <> 0 then
    FreeLibrary(DllHandle);

end.
