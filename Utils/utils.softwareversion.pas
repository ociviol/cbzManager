unit Utils.SoftwareVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetFileVersion:String;
function GetFileVersionInternalName:String;
function GetFileVersionCopyright:String;

implementation

uses
  fileinfo, elfreader;

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

