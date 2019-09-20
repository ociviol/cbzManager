unit Utils.NaturalSortStringList;

interface

uses
  CLasses;

type
  TNaturalSortStringList = Class(TStringList)
  private
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  End;

implementation

{$ifdef MsWindows}
function StrCmpLogicalW(P1, P2: PChar): Integer;  stdcall; external 'Shlwapi.dll';
{$else}
function NaturalCompareStr(Str1,Str2: string):Integer;
var 
  Num1,Num2:Double;
  pStr1,pStr2:PChar;
  Len1,Len2:Integer;
  TextLen1, TextLen2 :integer;
  TextStr1 :string = '';
  TextStr2 :string = '';
  i, j :Integer;
  
  Function IsNumber(ch:Char):Boolean;
  begin
     Result:=ch in ['0'..'9'];
  end;
  
  Function GetNumber(var pch:PChar;var Len:Integer):Double;
  var 
	FoundPeriod:Boolean;
    Count:Integer;
  begin
    FoundPeriod:=False;
    Result:=0;
    While (pch^<>#0) and
          (IsNumber(pch^) or ((not FoundPeriod) and (pch^='.'))) do
    begin
      if pch^='.' then
	  begin
	    FoundPeriod:=True;
	    Count:=0;
	  end
      else
      begin
        if FoundPeriod then
        begin
          Inc(Count);
          Result:=Result+(ord(pch^)-ord('0'))*Power(10,-Count);
        end
        else Result:=Result*10+ord(pch^)-ord('0');
      end;
      inc(Len);
      Inc(pch);
    end;
  end;
  procedure GetChars;
  begin
    TextLen1 := 0;
    while not((pStr1 + TextLen1)^ in ['0'..'9']) and ((pStr1 + TextLen1)^ <> #0) do
      Inc(TextLen1);

    SetLength(TextStr1, TextLen1);
    i := 1; 
	j := 0;
    while i <= TextLen1 do
    begin
      TextStr1[i] := (pStr1 + j)^;
      Inc(i); 
	  Inc(j);
    end;

    TextLen2 := 0;
    while not((pStr2 + TextLen2)^ in ['0'..'9']) and ((pStr2 + TextLen2)^ <> #0) do
      Inc(TextLen2);

    SetLength(TextStr2, TextLen2);
    i := 1; 
	j := 0;
    while i <= TextLen2 do
    begin
      TextStr2[i] := (pStr2 + j)^;
      Inc(i); 
	  Inc(j);
    end;
  end;

begin
  if (Str1<>'') and (Str2<>'') then
  begin
    pStr1:=@Str1[1]; 
	pStr2:=@Str2[1];
    Result:=0;
    While not ((pStr1^=#0) or (pStr2^=#0)) do
    begin
      TextLen1 := 1;
      TextLen2 := 1;
      Len1:=0; 
	  Len2:=0;
      while (pStr1^=' ') do 
	  begin 
		Inc(pStr1); 
		Inc(Len1);
	  end;
      while (pStr2^=' ') do 
	  begin 
	    Inc(pStr2); 
		Inc(Len2); 
	  end;
      if IsNumber(pStr1^) and IsNumber(pStr2^) then
      begin
        Num1 := GetNumber(pStr1, Len1);
        Num2 := GetNumber(pStr2, Len2);
        if Num1 < Num2 then Result := -1
        else if Num1 > Num2 then 
		  Result := 1
        else begin
           if Len1 < Len2 then 
		     Result := -1
           else if Len1 > Len2 then 
		     Result := 1;
        end;
        Dec(pStr1);
        Dec(pStr2);
      end
      else
      begin
        GetChars;
        Result := WideCompareText(UTF8Decode(TextStr1),UTF8Decode(TextStr2));
      end;
      if Result <> 0 then Break;
      Inc(pStr1, TextLen1);
      Inc(pStr2, TextLen2);
    end;
  end;
  Num1 := Length(Str1);
  Num2 := Length(Str2);
  if (Result = 0) and (Num1 <> Num2) then
  begin
    if Num1 < Num2 then
      Result := -1
    else
      Result := 1;
  end;
end;        
{$endif}

{ TNaturalSortStringList }

function TNaturalSortStringList.CompareStrings(const S1, S2: string): Integer;
begin
{$ifdef MsWindows}
  Result:= StrCmpLogicalW(PChar(S1), PChar(S2));
{$else}
  result := NaturalCompareStr(S1,S2):Integer;  
{$endif}  
end;

end.
