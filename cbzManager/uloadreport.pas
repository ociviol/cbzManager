unit uLoadReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmReport }

  TfrmReport = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
  private

  public
    constructor Create(aOwner : TComponent; const Lst : TStringlist); reintroduce;
  end;


implementation

{$R *.lfm}

{ TfrmReport }

constructor TfrmReport.Create(aOwner: TComponent; const Lst: TStringlist);
begin
  inherited Create(aOwner);
  Memo1.Lines.Assign(Lst);
end;

end.

