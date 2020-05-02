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
  Spin, uConfig;

type

  { TConfigFrm }

  TConfigFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cblogging: TCheckBox;
    cbDeleteFile: TCheckBox;
    cbAlbumArt: TCheckBox;
    edtcwebp: TEdit;
    edtSyncPath: TEdit;
    edtLibPath: TEdit;
    edtunrar: TEdit;
    edtp7zip: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    sbSyncPath: TSpeedButton;
    sbCwebp: TSpeedButton;
    sbLibPath: TSpeedButton;
    sbUnrar: TSpeedButton;
    sb7zip: TSpeedButton;
    speNbThreads: TSpinEdit;
    speWebpQuality: TSpinEdit;
    speQueues: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sb7zipClick(Sender: TObject);
    procedure sbCwebpClick(Sender: TObject);
    procedure sbLibPathClick(Sender: TObject);
    procedure sbSyncPathClick(Sender: TObject);
    procedure sbUnrarClick(Sender: TObject);
  private
  public
    Config : TConfig;
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
  edtLibPath.Text:=Config.LibPath;
  edtcwebp.Text:=Config.cwebp;
  edtunrar.Text:=Config.unrar;
  edtp7zip.Text:=Config.p7zip;
  speNbThreads.Value:= Config.NbThreads;
  speQueues.Value:=Config.QueueSize;
  cblogging.Checked:=Config.DoLog;
  speWebpQuality.Value:=Config.WebpQuality;
  cbDeleteFile.Checked := Config.DeleteFile;
  cbAlbumArt.Checked := Config.DoAlbumart;
  edtSyncPath.Text:=Config.SyncPath;
end;

procedure TConfigFrm.BitBtn1Click(Sender: TObject);
begin
  Config.LibPath:=edtLibPath.Text;
  Config.cwebp := edtcwebp.Text;
  Config.unrar := edtunrar.Text;
  Config.p7zip := edtp7zip.Text;
  Config.DoLog := cblogging.Checked;
  Config.QueueSize := speQueues.Value;
  Config.NbThreads:=speNbThreads.Value;
  Config.WebpQuality := speWebpQuality.Value;
  Config.DeleteFile := cbDeleteFile.Checked;
  Config.DoAlbumart := cbAlbumArt.Checked;
  Config.SyncPath := edtSyncPath.Text;
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

