unit uRar;

interface

uses
  AnsiStrings, Windows, Classes, SysUtils, 
  Helper.TStringList;

type
  TUnrarExtract = function(Sender : TObject; Indeex : Integer; Data : TMemoryStream):Boolean of object;
  TUnrarTest = function(Sender : TObject; const Filename : String; const Cur, Max : Integer):Boolean of object;
  TUnrar = Class
  private
    FOpened : Boolean;
    FFilename : String;
    FFileCount : Integer;
    FFilenames : TNaturalSortStringList;
    FOnExtract : TUnrarExtract;
    FOnTest : TUnrarTest;
    procedure OutOpenArchiveError(Error: Integer; const ArcName: String);
    procedure OutProcessFileError(Error: Integer);
    procedure List;
    procedure ExtractArchive(Mode: Integer; const Filename : String = ''; const Path : String = '');
    function GetFilename(Index: Integer): String;
  public
    constructor Create; overload;
    constructor Create(OnExtract : TUnrarExtract; OnTest : TUnrarTest); overload;
    destructor Destroy; override;
    procedure Open(const Filename : String);
    procedure Extract(const Path : String = '');
    procedure Test;

    property Opened : Boolean read FOpened;
    property FileCount : Integer read FFileCount;
    property FileNames[Index : Integer] : String read GetFilename;
    property Filename : String read FFilename;
  End;

implementation

uses
  UnRAR, uCbz;

function CallbackProc(msg: UINT; UserData, P1, P2: integer) :integer; stdcall;
begin
  if Msg = UCM_PROCESSDATA then
    TMemoryStream(UserData).WriteBuffer(PByte(P1)[0], P2);

  Result := 1;
end;

{ TUnrar }

constructor TUnrar.Create;
begin
  if not Assigned(FOnExtract) or
     not Assigned(FOnTest) then
    raise Exception.Create('Wrong constructor.');

  inherited Create;
end;

constructor TUnrar.Create(OnExtract: TUnrarExtract; OnTest : TUnrarTest);
begin
  FOnExtract := OnExtract;
  FOnTest := OnTest;
  Create;
  FFilenames := TNaturalSortStringList.Create;
  FFilenames.Sorted := False;
  FOpened := False;
end;

destructor TUnrar.Destroy;
begin
  FFilenames.Free;
  inherited;
end;

procedure TUnrar.Open(const Filename: String);
begin
  FFilename := Filename;
  List;
  FOpened := True;
end;

procedure TUnrar.Extract(const Path: String);
begin
  ExtractArchive(RAR_EXTRACT, PAth);
end;

procedure TUnrar.Test;
begin
  ExtractArchive(RAR_TEST);
end;

function TUnrar.GetFilename(Index: Integer): String;
begin
  result := FFilenames[Index];
end;

procedure TUnrar.ExtractArchive(Mode: Integer; const Filename : String = ''; const Path : String = '');
var
  hArcData: THandle;
  FCur, RHCode, PFCode: Integer;
  CmtBuf: array[0..Pred(16384)] of AnsiChar;
  PPath: array[0..255] of AnsiChar;
  HeaderData: RARHeaderDataEx;
  OpenArchiveData: RAROpenArchiveDataEx;
  fname : string;
  ms, ms2 : TMemoryStream;
begin
  try
    OpenArchiveData := Default(RAROpenArchiveDataEx);
    OpenArchiveData.ArcName := PansiChar(AnsiString(FFilename));
    OpenArchiveData.ArcNameW:= PChar(FFilename);
    OpenArchiveData.CmtBufSize := SizeOf(CmtBuf);
    OpenArchiveData.CmtBuf := @CmtBuf;
    OpenArchiveData.OpenMode := RAR_OM_EXTRACT;
    hArcData := RAROpenArchiveEx(OpenArchiveData);

    if Path = '' then
      AnsiStrings.StrPCopy(PPAth, AnsiString(Path));

    if (OpenArchiveData.OpenResult <> 0) then
      OutOpenArchiveError(OpenArchiveData.OpenResult, FFilename);

    try
      ms := TMemoryStream.Create;
      try
        RARSetCallback (hArcData, CallbackProc, Integer(ms));

        HeaderData.CmtBuf := nil;
        FCur := 0;
        repeat
          RHCode := RARReadHeaderEx(hArcData, HeaderData);
          if (RHCode <> 0) or (Fcur >= FFileCount) then
            Break;

          fname := String(HeaderData.FileNameW);
          if fname = '' then
            fname := String(HeaderData.FileName);

          if (Filename = '') or (Filename = fname) then
          begin
            if TCbz.AllowedFile(ExtractFileName(String(HeaderData.FileNameW))) then
            begin
              if Mode = RAR_EXTRACT then
                ms.Clear;

              if Path = '' then
                PFCode := RARProcessFile(hArcData, RAR_TEST, nil, nil)
              else
                PFCode := RARProcessFile(hArcData, RAR_EXTRACT, nil, nil);

              if (PFCode = 0) then
              begin
                case Mode of
                  RAR_EXTRACT :
                    if Assigned(FOnExtract) then
                    begin
                      ms.Position := 0;
                      ms2 := TMemoryStream.Create;
                      ms2.CopyFrom(ms, ms.Size);
                      ms2.Position := 0;
                      if not FOnExtract(Self, FFilenames.IndexOf(fname), ms2) then
                        Break;
                    end;

                  RAR_TEST:
                    if Assigned(FOnTest) then
                      if not FOnTest(Self, fname, FCur, FFileCount) then
                        Break;
                end;
              end
              else
                OutProcessFileError(PFCode);

              inc(FCur);
              if (Filename <> '') then
                break;
            end
            else
            begin
              PFCode:= RARProcessFile(hArcData, RAR_SKIP, nil, nil);
              if (PFCode <> 0) then
                OutProcessFileError(PFCode);
            end;
          end
          else
          begin
            PFCode := RARProcessFile(hArcData, RAR_SKIP, nil, nil);
            if (PFCode <> 0) then
                OutProcessFileError(PFCode);
          end;
        until False;

        if (RHCode = ERAR_BAD_DATA) then
          raise Exception.Create('File header broken');
      finally
        ms.Free;
      end;
    finally
      RARCloseArchive(hArcData);
    end;
  except
  end;
end;


procedure TUnrar.List;
var
  hArcData: THandle;
  RHCode, PFCode: Integer;
  CmtBuf: array[0..Pred(16384)] of AnsiChar;
  HeaderData: RARHeaderDataEx;
  OpenArchiveData: RAROpenArchiveDataEx;
begin
  OpenArchiveData.ArcName := PansiChar(AnsiString(FFilename));
  OpenArchiveData.ArcNameW:= PChar(FFilename);
  OpenArchiveData.CmtBuf := @CmtBuf;
  OpenArchiveData.CmtBufSize := SizeOf(CmtBuf);
  OpenArchiveData.OpenMode := RAR_OM_LIST;
  hArcData := RAROpenArchiveEx(OpenArchiveData);

  if (OpenArchiveData.OpenResult <> 0) then
    OutOpenArchiveError(OpenArchiveData.OpenResult, FFilename);

  try
    FFileCount := 0;
    FFilenames.Clear;
    RARSetCallback (hArcData, CallbackProc, 0);

    HeaderData.CmtBuf := @CmtBuf;
    HeaderData.CmtBufSize := SizeOf(CmtBuf);

    repeat
      RHCode := RARReadHeaderEx(hArcData, HeaderData);
      if RHCode <> 0 then
        Break;

      if TCbz.AllowedFile(ExtractFileName(String(HeaderData.FileNameW))) then
      begin
        FFilenames.Add(String(HeaderData.FileNameW));
        inc(FFileCount);
      end;

      PFCode:= RARProcessFile(hArcData, RAR_SKIP, nil, nil);
      if (PFCode <> 0) then
        OutProcessFileError(PFCode);
    until False;

    if (RHCode = ERAR_BAD_DATA) then
      raise Exception.Create('File header broken');
  finally
    RARCloseArchive(hArcData);
  end;
  FFilenames.Sort;
end;

procedure TUnrar.OutOpenArchiveError(Error: Integer; const ArcName: String);
begin
  case Error of
    ERAR_NO_MEMORY:   Raise Exception.Create('Not enough memory');
    ERAR_EOPEN:       Raise Exception.Create('Cannot open ' + ArcName);
    ERAR_BAD_ARCHIVE: Raise Exception.Create(ArcName + ' is not RAR archive');
    ERAR_BAD_DATA:    Raise Exception.Create(ArcName + ': archive header broken');
    ERAR_UNKNOWN:     Raise Exception.Create('Unknown error');
  end;
end;

procedure TUnrar.OutProcessFileError(Error: Integer);
begin
  case Error of
    ERAR_UNKNOWN_FORMAT: Raise Exception.Create('Unknown archive format');
    ERAR_BAD_ARCHIVE:    Raise Exception.Create('Bad volume');
    ERAR_ECREATE:        Raise Exception.Create('File create error');
    ERAR_EOPEN:          Raise Exception.Create('Volume open error');
    ERAR_ECLOSE:         Raise Exception.Create('File close error');
    ERAR_EREAD:          Raise Exception.Create('Read error');
    ERAR_EWRITE:         Raise Exception.Create('Write error');
    ERAR_BAD_DATA:       Raise Exception.Create('CRC error');
    ERAR_UNKNOWN:        Raise Exception.Create('Unknown error');
  end;
end;



end.
