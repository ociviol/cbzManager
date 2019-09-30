unit Utils.Strings;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


//Functions

function AnsiNaturalCompareStrings(str1, str2: string; vCaseSensitive: boolean = True): integer;

implementation


function AnsiNaturalCompareStrings(str1, str2: string; vCaseSensitive: boolean = True): integer;
var
  l1, l2: integer; //Str length
  n1, n2: integer; //numrical part
  i1, i2: integer; //index in Str
  d: integer;
begin
  if not vCaseSensitive then
  begin
    str1 := UpperCase(str1);
    str2 := UpperCase(str2);
  end;

  l1 := Length(str1);
  l2 := Length(str2);

  i1 := 1;
  i2 := 1;
  while i1 <= l1 do
  begin
    //Compare non-numbers
    d := Ord(str1[i1]) - Ord(str2[i2]);
    if not (str1[i1] in ['0'..'9']) then
    begin
      if (d <> 0) then
      begin
        Result := d;
        exit;
      end;
    end
    else
    begin
      //Convert a section of str1 to a number
      n1 := 0;
      repeat
        n1 := 10 * n1 + Ord(str1[i1]) - Ord('0');
        Inc(i1);
      until (i1 > l1) or not (str1[i1] in ['0'..'9']);

      //Convert a section of str2 to a number
      n2 := 0;
      repeat
        n2 := 10 * n2 + Ord(str2[i2]) - Ord('0');
        Inc(i2);
      until (i2 > l2) or not (str2[i2] in ['0'..'9']);

      //Compare numbers naturally
      d := n1 - n2;
      if d <> 0 then
      begin
        Result := d;
        exit;
      end
      else
        Continue;
    end;
    Inc(i1);
    Inc(i2);
  end;
  Result := (i1 - l1) - (i2 - l2);
end;

end.

