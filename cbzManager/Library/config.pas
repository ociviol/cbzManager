unit config;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  uConfig;

type

  { TConfigFrm }

  TConfigFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cblogging: TCheckBox;
    edtSyncPath: TEdit;
    edtLibPath: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    sbSyncPath: TSpeedButton;
    sbLibPath: TSpeedButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbLibPathClick(Sender: TObject);
    procedure sbSyncPathClick(Sender: TObject);
  private
  public
    Config : TConfig;
  end;


implementation

{$R *.lfm}

uses
  utils.vcl, uCbzLibrary;

{ TConfigFrm }

procedure TConfigFrm.sbLibPathClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Application) do
  try
    Title := 'select Library location';
    if Execute then
      edtLibPath.Text:=Filename;
  finally
    Free;
  end;
end;

procedure TConfigFrm.sbSyncPathClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Application) do
  try
    Title := 'select sync location';
    if Execute then
      edtSyncPath.Text:=Filename;
  finally
    Free;
  end;
end;

procedure TConfigFrm.FormShow(Sender: TObject);
begin
  edtLibPath.Text:=Config.LibPath;
  cblogging.Checked:=Config.DoLog;
  edtSyncPath.Text:=Config.SyncPath;
end;

procedure TConfigFrm.BitBtn1Click(Sender: TObject);
begin
  Config.LibPath:=edtLibPath.Text;
  Config.DoLog := cblogging.Checked;
  Config.SyncPath := edtSyncPath.Text;
end;


end.

