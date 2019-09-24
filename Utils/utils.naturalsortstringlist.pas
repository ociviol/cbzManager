unit Utils.NaturalSortStringList;

interface

uses
{$if Defined(MsWindows)}
  Windows,
{$endif}
  Classes, SysUtils, LazUTF8, LconvEncoding;


type

  { TNaturalSortStringList }

  TNaturalSortStringList = Class(TStringList)
  private
  public
    procedure Sort; override;
  End;

implementation

var
  CodePage: integer;
  CodePageString: string;

function RemoveDiacritics(const S: string): string;
// by SilvioProg
var
  F: Boolean;
  I: SizeInt;
  PS, PD: PChar;
begin
  SetLength(Result, Length(S));
  PS := PChar(S);
  PD := PChar(Result);
  I := 0;
  while PS^ <> #0 do
  begin
    F := PS^ = #195;
    if F then
      case PS[1] of
        #128..#132: PD^ := #65;
        #135: PD^ := #67;        // letra Ç
        #136..#139: PD^ := #69;
        #140..#143: PD^ := #73;
        #145: PD^ := #78;        // letra Ñ
        #146..#150: PD^ := #79;
        #153..#156: PD^ := #85;
        #157: PD^ := #89;
        #160..#164: PD^ := #97;
        #167: PD^ := #99;        // letra ç
        #168..#171: PD^ := #101;
        #172..#175: PD^ := #105;
        #177: PD^ := #110;
        #178..#182: PD^ := #111;  // letra o
        #185..#188: PD^ := #117;
        #189..#191: PD^ := #121;
      else
        F := False;
      end;
    if F then
      Inc(PS)
    else
      PD^ := PS^;
    Inc(I);
    Inc(PD);
    Inc(PS);
  end;
  SetLength(Result, I);
end;

function EvsCompareNatural_A(const S1, S2 :AnsiString; aCaseSensitive : Boolean = True):integer;
// by taazz
var
  vChr1, vChr2 : PChar;
  vCmp1, vCmp2 : string;

  function Sign(aNo:integer):integer; inline;
  begin
    Result := 0;
    if aNo > 0 then Result := 1
    else if aNo < 0 then Result := -1;
  end;

  function NumCharLen(const aStart:PAnsiChar):Integer;
  var
    vNext : PAnsiChar;
  begin
    vNext := aStart;
    repeat
      inc(vNext);
    until (vNext^ = #0) or (not (vNext^ in ['0'..'9']));
    Result := vNext-aStart;
  end;

  function CompToChar(var aStr:PAnsiChar; const aChar:Char; aCount:Integer):Integer;inline;
  begin            // compares the next aCount characters of aStr with aChar and returns 0 if they are all the same
    Result := 0;   // or <>0 if they differ. It is used to avoid padding a string with zeros.
    repeat
      Result := sign(Ord(aStr^) - ord(aChar));
      Dec(aCount); Inc(aStr);
    until (Result <> 0) or (aStr^=#0) or (aCount = 0);
  end; //when checking numeric characters[0..9] against zero it always returns a positive number.

  function NumComp:Integer;
  var
    vNl : Integer;
  begin
    Result := -2;
    vNl := NumCharLen(vChr1) - NumCharLen(vChr2);
    if vNl < 0 then Result := CompToChar(vChr2, '0', abs(vNl))
    else if vNl > 0 then Result := CompToChar(vChr1, '0', vNl);
    if (Result > 0) then begin
      Result := Sign(vNl);
      Exit;
    end;
    repeat
      Result := sign(ord(vChr1^) - ord(vChr2^));
      inc(vChr1); inc(vChr2);
    until ((vChr1^=#0) or (vChr2^=#0))   //end of string has been reached
       or (Result <> 0)                  //conclusion has been reached
       or (not (vChr1^ in ['0'..'9']))   //numeric characters end here
       or (not (vChr2^ in ['0'..'9']));  //numeric characters end here

    if Result = 0 then begin
      if vChr1^ in ['0'..'9'] then Result := 1
      else if vChr2^ in ['0'..'9'] then Result := -1;
    end;
  end;

begin
  //s1<s2 = -1, S1=S2 =0, S1>S2 = 1;
  if aCaseSensitive then begin
    vChr1 := @S1[1]; vChr2 := @S2[1]
  end else begin
    vCmp1 := LowerCase(S1); vCmp2 := LowerCase(S2);
    vChr1 := @vCmp1[1];     vChr2 := @vCmp2[1];
  end;

  repeat
    if (vChr1^ in ['0'..'9']) and (vChr2^ in ['0'..'9']) then
      Result := NumComp // it exits ready in the next position
    else begin
      Result := Sign(ord(vChr1^)- ord(vChr2^));
      if vChr1^ <> #0 then inc(vChr1);
      if vChr2^ <> #0 then inc(vChr2);
    end;
  until (vChr1^=#0) or (vChr2^=#0) or (Result <> 0);
  if (Result = 0) then Result := Sign(ord(vChr1^) - Ord(vChr2^));
end;

function NaturalSortCompare(aList: TStringList;  Index1, Index2: Integer): Integer;
var
  Str1, Str2 :string;
{$IFDEF LINUX}
  lang :string;
{$endif}
begin
  Str1 := RemoveDiacritics(aList[Index1]);
  Str2 := RemoveDiacritics(aList[Index2]);

  // Case insensitive.
  {$IFDEF MsWindows}
  if CodePage = 1252 then  // Latin chars
  begin
    Result := EvsCompareNatural_A(Str1, Str2, False);
    // Places unsigned words (without diacritics) before the
    // signed ones (with diacritics) when they are equal.
    // The order among signed ones is preserved.
    if Str1 = Str2 then
      Result := EvsCompareNatural_A(aList[Index1], aList[Index2], False);
  end
  else
    Result := EvsCompareNatural_A(aList[Index1], aList[Index2], False);
  {$ELSE}
  {$IFDEF LINUX}
  if CodePageString <> '' then
  begin
    lang := CodePageString[1] + CodePageString[2];
    if (lang = 'pt') or (lang = 'es') or (lang = 'fr') or (lang = 'it')
       or (lang = 'gl' {galician}) or (lang = 'gn' {guarani}) or (lang = 'ay' {aymará})
       then
    begin
      Result := EvsCompareNatural_A(Str1, Str2, False);
      if Str1 = Str2 then
        Result := EvsCompareNatural_A(aList[Index1], aList[Index2], False);
    end
    else
      Result := EvsCompareNatural_A(aList[Index1], aList[Index2], False)
  end;
  {$ELSE}
  Result := EvsCompareNatural_A(aList[Index1], aList[Index2], False)
  {$ENDIF}
  {$ENDIF}
end;

{ TNaturalSortStringList }

procedure TNaturalSortStringList.Sort;
begin
  CustomSort(@NaturalSortCompare);
end;

initialization
  CodePage := 0;
  CodePageString := '';
  {$IFDEF MsWindows}
  CodePage :=  GetACP;
  {$ELSE}
  {$IFDEF LINUX}
    CodePageString := sysutils.GetEnvironmentVariable('LC_ALL');
  if CodePageString = '' then
    CodePageString := sysutils.GetEnvironmentVariable('LC_CTYPE');
  if CodePageString = '' then
    CodePageString := sysutils.GetEnvironmentVariable('LANG');
  {$ENDIF}
  {$ENDIF}

end.
