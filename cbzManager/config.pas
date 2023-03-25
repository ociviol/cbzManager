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
  Spin, ExtCtrls, uConfig;

type

  { TConfigFrm }

  TConfigFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbAlbumArt: TCheckBox;
    cbDeleteFile: TCheckBox;
    cblogging: TCheckBox;
    edtcwebp: TEdit;
    edtunrar: TEdit;
    edtp7zip: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    sbCwebp: TSpeedButton;
    sbUnrar: TSpeedButton;
    sb7zip: TSpeedButton;
    speNbThreads: TSpinEdit;
    speStampWidth: TSpinEdit;
    speStampHeight: TSpinEdit;
    speWebpQuality: TSpinEdit;
    speQueues: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sb7zipClick(Sender: TObject);
    procedure sbCwebpClick(Sender: TObject);
    procedure sbUnrarClick(Sender: TObject);
  private
  public
    Config : TConfig;
  end;


implementation

{$R *.lfm}

uses
  utils.vcl;

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

procedure TConfigFrm.FormShow(Sender: TObject);
begin
  edtcwebp.Text:=Config.cwebp;
  edtunrar.Text:=Config.unrar;
  edtp7zip.Text:=Config.p7zip;
  speNbThreads.Value:= Config.NbThreads;
  speQueues.Value:=Config.QueueSize;
  cblogging.Checked:=Config.DoLog;
  speWebpQuality.Value:=Config.WebpQuality;
  cbDeleteFile.Checked := Config.DeleteFile;
  cbAlbumArt.Checked := Config.DoAlbumart;
  speStampWidth.Value:= Config.DefaultColWidth;
  speStampHeight.Value:=Config.DefaultRowHeight;
end;

procedure TConfigFrm.BitBtn1Click(Sender: TObject);
begin
  Config.cwebp := edtcwebp.Text;
  Config.unrar := edtunrar.Text;
  Config.p7zip := edtp7zip.Text;
  Config.DoLog := cblogging.Checked;
  Config.QueueSize := speQueues.Value;
  Config.NbThreads:=speNbThreads.Value;
  Config.WebpQuality := speWebpQuality.Value;
  Config.DeleteFile := cbDeleteFile.Checked;
  Config.DoAlbumart := cbAlbumArt.Checked;
  Config.DefaultColWidth := speStampWidth.Value;
  Config.DefaultRowHeight := speStampHeight.Value;
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

