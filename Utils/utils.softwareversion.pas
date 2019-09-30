unit Utils.SoftwareVersion;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetFileVersion:String;
function GetFileVersionInternalName:String;
function GetFileVersionCopyright:String;
function CompareVersion(const v1, v2 : String):Integer;

implementation

uses
  fileinfo, elfreader;

function CompareVersion(const v1, v2 : String):Integer;
var
  ar1, ar2 : Array of String;
begin
  ar1 := v1.Split(['.']);
  ar2 := v2.Split(['.']);

  // major
  if StrToInt(ar2[0]) > StrToInt(ar1[0]) then Exit(1);
  if StrToInt(ar2[0]) < StrToInt(ar1[0]) then Exit(-1);
  // minor
  if StrToInt(ar2[1]) > StrToInt(ar1[1]) then Exit(1);
  if StrToInt(ar2[1]) < StrToInt(ar1[1]) then Exit(-1);
  // release
  if StrToInt(ar2[2]) > StrToInt(ar1[2]) then Exit(1);
  if StrToInt(ar2[2]) < StrToInt(ar1[2]) then Exit(-1);
  // build
  if StrToInt(ar2[3]) > StrToInt(ar1[3]) then Exit(1);
  if StrToInt(ar2[3]) < StrToInt(ar1[3]) then Exit(-1);
  result := 0;
end;

function GetFileVersion:String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    result := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function GetFileVersionInternalName:String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    result := FileVerInfo.VersionStrings.Values['InternalName'];
  finally
    FileVerInfo.Free;
  end;
end;

function GetFileVersionCopyright:String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    result := FileVerInfo.VersionStrings.Values['LegalCopyright'];
  finally
    FileVerInfo.Free;
  end;
end;


end.

