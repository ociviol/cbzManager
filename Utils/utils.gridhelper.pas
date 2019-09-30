unit Utils.Gridhelper;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type
  TStampView = (svVert, svHoriz);
  TDrawGrid = Class(Grids.TDrawGrid)
  private
    FSelections: Array of Boolean;
    FOrientation : TStampView;

    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; const Value: Boolean);
    function Getposition:Integer;
    procedure Setposition(const Value: Integer);
    function GetMax: Integer;
    procedure SetMax(const Value: Integer);
    procedure SetOrientation(const Value: TStampView);
  public
    property Orientation : TStampView read FOrientation write SetOrientation;
    procedure ClearSelection;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property Position:Integer read Getposition write Setposition;
    property Max:Integer read GetMax write SetMax;
  End;

implementation

{ TDrawGrid }



procedure TDrawGrid.ClearSelection;
var
  i: Integer;
begin
  for i := 0 to Max - 1 do
    FSelections[i] := False;
  Invalidate;
end;

function TDrawGrid.GetSelected(Index: Integer): Boolean;
begin
  if Index < Length(FSelections) then
    Result := FSelections[Index]
  else
    Result := False;
end;

procedure TDrawGrid.SetSelected(Index: Integer; const Value: Boolean);
begin
  if (Index >= 0) and (Index < Length(FSelections)) then
    FSelections[Index] := Value;
end;

function TDrawGrid.GetMax: Integer;
begin
  if Orientation = svVert then
    result := RowCount
  else
    result := ColCount;
end;

function TDrawGrid.Getposition: Integer;
begin
  if Orientation = svVert then
    result := Row
  else
    result := Col;
end;

procedure TDrawGrid.SetMax(const Value: Integer);
begin
  if Orientation = svVert then
    RowCount := Value
  else
    ColCount := Value;

  SetLength(FSelections, Value);
  ClearSelection;
end;

procedure TDrawGrid.SetOrientation(const Value: TStampView);
begin
  FOrientation := Value;
end;

procedure TDrawGrid.Setposition(const Value: Integer);
begin
  if Orientation = svVert then
    Row := Value
  else
    Col := Value;
end;

end.

