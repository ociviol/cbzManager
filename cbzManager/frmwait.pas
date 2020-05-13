unit frmwait;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TFormWait }

  TFormWait = class(TForm)
    Panel1: TPanel;
  private
    function GetText: String;
    procedure SetText(AValue: String);

  public
    property Text : String read GetText write SetText;
  end;


implementation

{$R *.lfm}

{ TFormWait }

function TFormWait.GetText: String;
begin
  result := Panel1.Caption;
end;

procedure TFormWait.SetText(AValue: String);
begin
  Panel1.Caption := AValue;
  PAnel1.Refresh;
end;

end.

