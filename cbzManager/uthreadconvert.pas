unit uThreadConvert;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, uDataPool, Utils.Logger, Graphics, uDataTypes
{$if defined(Darwin) or defined(Linux)}
  ,cthreads
{$endif}
  ;

type
  TThreadConvert = Class(TThread)
  private
    FWebpQualityFactor : PInteger;
    FDataPool : TThreadDataPool;
    FLog : ILog;
    function CopyBlock(const aSrc : TMemoryStream):TMemoryStream;
  public
    constructor Create(DataPool : TThreadDataPool; Log : ILog; aWebpQualityFactor : PInteger);
    procedure Execute; override;
    function IsTerminated:Boolean;
  End;

implementation

uses
  Forms, Sysutils,
  uDataItem,
  uCbz, Utils.Arrays;

{ ThreadConvert }

constructor TThreadConvert.Create(DataPool: TThreadDataPool; Log : ILog; aWebpQualityFactor : PInteger);
begin
  FWebpQualityFactor := aWebpQualityFactor;
  FDataPool := DataPool;
  FLog := Log;
  inherited Create(False);
end;

procedure TThreadConvert.Execute;
var
  i : Integer;
  StOut : TMemoryStream;
  Rec : TDataRec;
begin
  while not Terminated do
  try
    for i := 0 to FDataPool.PoolSize - 1 do
    begin
      if Terminated then
        Break;

      if i < FDataPool.PoolSize then
      begin
        stOut := nil;
        Rec := FDataPool.Pool[i].GetIn;
        if Assigned(Rec)  {or (Rec.DataType = dtPdf)} then
        begin
          if (opConvert in Rec.Operations) and
             (InIntArray(Rec.Index, Rec.Indexes) or (length(Rec.Indexes) = 0)) then
          begin
            try
              case Rec.DataType of
                dtImage:
                  if (Rec.ImgType = FIF_WEBP) and Assigned(Rec.Stream) then
                     StOut := CopyBlock(Rec.Stream)
                  else
                  begin
                    if not Assigned(Rec.Stream) then
                    begin
                      Rec.Stream := TMemoryStream.Create;
                      Rec.Stream.LoadFromFile(Rec.Filename);
                      Rec.Filename := '';
                    end;
                    try
                      Stout := TCbz.ConvertImageToStream(Rec.Stream, FLog, FWebpQualityFactor^);
                    except
                      Stout := nil;
                    end;
                  end;

                dtPdf: ; // StOut := PdfToStream(Rec.Index, Rec.Filename);
                dtMeta : StOut := CopyBlock(Rec.Stream);
              end;
            finally
              if Assigned(Rec.Stream) then
                Rec.Stream.Free;
            end;

            Rec.Stream := StOut;
          end;

          try
            if not Assigned(stOut) then
              Flog.Log('ThreadConvert skipped image (null stream) : ' + IntTostr(Rec.Index));

            FDataPool.Pool[i].Put(Rec);
            Sleep(50);
          except
            on e: Exception do
            begin
              FLog.Log(Format('Exception in %s :  %s', [ClassName, e.Message]));
              if Assigned(stOut) then
                stOut.Free;
            end;
          end;
        end;

        Sleep(100);
      end;

      if i >= FDataPool.PoolSize then
        break;
    end;
  except
    on e: Exception do
      FLog.Log(Format('Exception in %s :  %s', [ClassName, e.Message]));
  end;
end;

function TThreadConvert.IsTerminated: Boolean;
begin
  result := Terminated;
end;

function TThreadConvert.CopyBlock(const aSrc : TMemoryStream):TMemoryStream;
begin
  result := TMemoryStream.Create;
  result.CopyFrom(aSrc, aSrc.Size);
  result.Position := 0;
end;

{
function TThreadConvert.PdfToStream(Page : Integer; const aFilename : String):TMemoryStream;
var
  cmd,
  tmpfile,
  tmpfldr : string;
  ms : TMemoryStream;
  retry : integer;

  function Pdf2Bmp:String; inline;
  begin
    result := ExtractFilePath(Application.ExeName) + 'Pdf2Bmp.exe';
  end;

  function GetTmpFldr:String;
  begin
    repeat
      result := IncludeTrailingPathDelimiter(FTempPath) +
                'Cbz' + IntToStr(ThreadId + GetTickCount) + '.tmp'
    until not SysUtils.FileExists(result)
  end;

begin//  function ConvertPage(const fimg : String):String;
  FLog.Log(ClassName + '.ConvertPdfPage : ' + ExtractFilename(aFilename) + ' Page:' + IntToStr(Page));

  result := nil;
  if SysUtils.FileExists(aFilename) then
  begin
    tmpfldr := GetTmpFldr;
    CreateDir(TmpFldr);
    try
      tmpfile := IncludeTrailingPathDelimiter(TmpFldr) + 'Page'+ IntToStr(Page+1)+'.bmp';
      cmd := Pdf2Bmp + ' "' + aFilename + '" ' + IntToStr(Page+1) + ' "' + TmpFldr + '"';
      FLog.Log(ClassName + '.ConvertPdfPage Executing : ' + cmd);
      ExecuteAndWait(cmd);

      if not SysUtils.FileExists(tmpfile) then
        Exit;

      ms := TMemoryStream.Create;
      try
        ms.LoadFromFile(tmpfile);
        ms.position := 0;
        result := TCbz.ConvertImageToStream(ms, FTempPath, FLog);
        Sleep(50);
      finally
        ms.free;
      end;
      FLog.Log(ClassName + '.ConvertPdfPage done: ');
    finally
      retry := 0;
      repeat
        try
          TDirectory.Delete(tmpfldr, True);
          retry := -1;
        except
          inc(retry);
          Sleep(200);
        end;
        until (retry < 0) or (retry > 4);
    end;
  end
  else
    FLog.Log(ClassName + '.ConvertPdfPage File not found : ' + aFilename);
end;
}

end.
