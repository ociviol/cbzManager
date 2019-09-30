unit Utils.Arrays;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringArray = Array of String;
  TIntArray = Array of QWord;
  TObjArray = Array of TObject;
  TStreamArray = Array of TMemoryStream;

function InIntArray(value : QWord; const arr: TIntArray):Boolean; inline;
function InStringArray(const Value : String; arr : TStringArray):Boolean;
function GreateThanInIntArray(value : QWord; const arr: TIntArray):Boolean; inline;
function PosInIntArray(value : QWord; const arr: TIntArray):Integer; inline;

implementation

function InIntArray(value : QWord; const arr: TIntArray):Boolean; inline;
var
  j : QWord;
begin
  for j in Arr do
    if value = j then
     exit(True);
   exit(false);
end;

function InStringArray(const Value : String; arr : TStringArray):Boolean;
var
  s : string;
begin
  for s in Arr do
    if value.ToLower = s.ToLower then
     exit(True);
   exit(false);
end;

function PosInIntArray(value : QWord; const arr: TIntArray):Integer; inline;
var
  i : integer;
begin
  for i := low(arr) to high(arr) do
    if value = arr[i] then
     exit(i);
   exit(-1);
end;

function GreateThanInIntArray(value : QWord; const arr: TIntArray):Boolean; inline;
var
  j : QWord;
begin
  for j in Arr do
    if value > j then
     exit(True);
   exit(false);
end;

end.

