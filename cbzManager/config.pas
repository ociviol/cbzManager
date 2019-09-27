unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Spin;

type

  { TConfigFrm }

  TConfigFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cblogging: TCheckBox;
    edtcwebp: TEdit;
    edtunrar: TEdit;
    edtp7zip: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    sbCwebp: TSpeedButton;
    sbUnrar: TSpeedButton;
    sb7zip: TSpeedButton;
    speNbThreads: TSpinEdit;
    speWebpQuality: TSpinEdit;
    speQueues: TSpinEdit;
    procedure sb7zipClick(Sender: TObject);
    procedure sbCwebpClick(Sender: TObject);
    procedure sbUnrarClick(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}

{ TConfigFrm }

procedure TConfigFrm.sbCwebpClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Title := 'select cwebp location';
    if Execute then
      edtcwebp.Text:=Filename;
  end;
end;

procedure TConfigFrm.sb7zipClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Title := 'select 7z location';
    if Execute then
      edtp7zip.Text:=Filename;
  end;
end;

procedure TConfigFrm.sbUnrarClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Title := 'select unrar location';
    if Execute then
      edtunrar.Text:=Filename;
  end;
end;


end.

