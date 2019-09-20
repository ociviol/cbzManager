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
//    function CompareStrings(const S1, S2: string): Integer; override;
  End;

implementation

{$ifdef MsWindows}
function StrCmpLogicalW(P1, P2: PChar): Integer;  stdcall; external 'Shlwapi.dll';
{$endif}

{ TNaturalSortStringList }

//{$ifndef MsWindows}
procedure TNaturalSortStringList.Sort;
begin
  NaturalSort(Self, stNatural);
end;
//{$endif}

//function TNaturalSortStringList.CompareStrings(const S1, S2: string): Integer;
//begin
//{$ifdef MsWindows}
//  Result:= StrCmpLogicalW(PChar(S1), PChar(S2));
//{$endif}
//end;

end.
