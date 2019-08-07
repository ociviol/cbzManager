unit Utils.Files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetFileSize(const FileName : string) : Int64;
procedure CopyFile(const aSrc, aDest : String);

implementation

function GetFileSize(const FileName : string) : Int64;
var
  sr : TRawByteSearchRec;
begin
  if FindFirst (Filename, faAnyFile, sr) = 0 then
  begin
    FindClose(sr);
    result := sr.Size;
  end
  else
    result := 0;
end;

procedure CopyFile(const aSrc, aDest: String);
var
  sin, sout : TFileStream;
begin
  sin := TFileStream.Create(aSrc, fmOpenRead);
  try
    sout := TFileStream.Create(aDest, fmCreate);
    try
       sout.CopyFrom(sin, sin.Size);
    finally
      sout.free;
    end;
  finally
    sin.Free;
  end;
end;

end.

