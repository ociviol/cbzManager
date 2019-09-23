unit Utils.NaturalSortStringList;

interface

uses
  CLasses, naturalsortunit;

type

  { TNaturalSortStringList }

  TNaturalSortStringList = Class(TStringList)
  private
  protected
//{$ifndef MsWindows}
    procedure Sort; override;
//{$endif}
    function CompareStrings(const S1, S2: string): Integer; override;
  End;

implementation

{$ifdef MsWindows}
function StrCmpLogicalW(P1, P2: PChar): Integer;  stdcall; external 'Shlwapi.dll';
{$endif}

{ TNaturalSortStringList }


function WideStrComp(const Str1, Str2 : WideString): PtrInt;
 var
  counter: SizeInt = 0;
  pstr1, pstr2: PWideChar;
 Begin
   pstr1 := PWideChar(Str1);
   pstr2 := PWideChar(Str2);
   While pstr1[counter] = pstr2[counter] do
   Begin
     if (pstr2[counter] = #0) or (pstr1[counter] = #0) then
        break;
     Inc(counter);
   end;
   Result := ord(pstr1[counter]) - ord(pstr2[counter]);
 end;

function StrFloatCmpW(str1, str2: PWideChar): Integer;
var
  is_digit1, is_digit2: boolean;
  string_result: ptrint = 0;
  number_result: ptrint = 0;
  number1_size: ptrint = 0;
  number2_size: ptrint = 0;
  str_cmp: function(const s1, s2: WideString): PtrInt;

  function is_digit(c: widechar): boolean; inline;
  begin
    result:= (c in ['0'..'9']);
  end;

  function is_point(c: widechar): boolean; inline;
  begin
    result:= (c in [',', '.']);
  end;

begin
  // Set up compare function
  str_cmp:= @WideStrComp;

  while (true) do
  begin
    // compare string part
    while (true) do
    begin
      if str1^ = #0 then
      begin
        if str2^ <> #0 then
          exit(-1)
        else
          exit(0);
      end;

      if str2^ = #0 then
      begin
        if str1^ <> #0 then
          exit(+1)
        else
          exit(0);
      end;

      is_digit1 := is_digit(str1^);
      is_digit2 := is_digit(str2^);

      if (is_digit1 and is_digit2) then break;

      if (is_digit1 and not is_digit2) then
        exit(-1);

      if (is_digit2 and not is_digit1) then
        exit(+1);

      string_result:= str_cmp(str1^, str2^);

      if (string_result <> 0) then exit(string_result);

      inc(str1);
      inc(str2);
    end;

    // skip leading zeroes for number
    while (str1^ = '0') do
      inc(str1);
    while (str2^ = '0') do
      inc(str2);

    // compare number before decimal point
    while (true) do
    begin
      is_digit1 := is_digit(str1^);
      is_digit2 := is_digit(str2^);

      if (not is_digit1 and not is_digit2) then
        break;

      if ((number_result = 0) and is_digit1 and is_digit2) then
      begin
        if (str1^ > str2^) then
          number_result := +1
        else if (str1^ < str2^) then
          number_result := -1
        else
          number_result := 0;
      end;

      if (is_digit1) then
      begin
        inc(str1);
        inc(number1_size);
      end;

      if (is_digit2) then
      begin
        inc(str2);
        inc(number2_size);
      end;
    end;

    if (number1_size <> number2_size) then
      exit(number1_size - number2_size);

    if (number_result <> 0) then
      exit(number_result);

    // if there is a decimal point, compare number after one
    if (is_point(str1^) or is_point(str2^)) then
    begin
      if (is_point(str1^)) then
        inc(str1);

      if (is_point(str2^)) then
        inc(str2);

      while (true) do
      begin
        is_digit1 := is_digit(str1^);
        is_digit2 := is_digit(str2^);

        if (not is_digit1 and not is_digit2) then
          break;

        if (is_digit1 and not is_digit2) then
        begin
          while (str1^ = '0') do
            inc(str1);

          if (is_digit(str1^)) then
            exit(+1)
          else
            break;
        end;

        if (is_digit2 and not is_digit1) then
        begin
          while (str2^ = '0') do
            inc(str2);

          if (is_digit(str2^)) then
            exit(-1)
          else
            break;
        end;

        if (str1^ > str2^) then
          exit(+1)
        else if (str1^ < str2^) then
          exit(-1);

        inc(str1);
        inc(str2);
      end;
    end;
  end;
end;

function Compare(List : TStringlist; Index1, Index2 : longint):integer;
begin
  Result := TNaturalSortStringList(List).CompareStrings(List[Index1],
                                                        List[Index2]);
end;

//{$ifndef MsWindows}
procedure TNaturalSortStringList.Sort;
begin
  //NaturalSort(Self, stNatural);
  CustomSort(@Compare);
end;
//{$endif}

function TNaturalSortStringList.CompareStrings(const S1, S2: string): Integer;
begin
{$ifdef MsWindows}
  Result:= StrCmpLogicalW(PChar(S1), PChar(S2));
{$endif}
  Result := WideStrComp(s1, s2);
end;

end.
