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
    btnOk: TBitBtn;
    BitBtn2: TBitBtn;
    btnRemovelib: TButton;
    btnAddlib: TButton;
    cblogging: TCheckBox;
    cbAutoImportReadStates: TCheckBox;
    edtSyncPath: TEdit;
    edtLibPath: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    lbYacLibs: TListBox;
    OpenDialog1: TOpenDialog;
    sbSyncPath: TSpeedButton;
    sbLibPath: TSpeedButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnAddlibClick(Sender: TObject);
    procedure btnRemovelibClick(Sender: TObject);
    procedure cbAutoImportReadStatesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbLibPathClick(Sender: TObject);
    procedure sbSyncPathClick(Sender: TObject);
  private
    procedure LoadYacLibraries;
    procedure SaveYacLibraries;
  public
    Config : TConfig;
  end;


implementation

{$R *.lfm}

uses
  utils.vcl, uCbzLibrary, inifiles;

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
  cbAutoImportReadStates.Checked := Config.AutoSyncYac;
  // save yaclibraries
  if cbAutoImportReadStates.Checked then
    LoadYacLibraries;
end;

procedure TConfigFrm.btnOkClick(Sender: TObject);
begin
  Config.LibPath:=edtLibPath.Text;
  Config.DoLog := cblogging.Checked;
  Config.SyncPath := edtSyncPath.Text;
  Config.AutoSyncYac := cbAutoImportReadStates.Checked;
  // save yaclibraries
  if cbAutoImportReadStates.Checked then
    SaveYacLibraries;
end;

procedure TConfigFrm.LoadYacLibraries;
begin
  Config.GetYacLibs(lbYacLibs.Items);
end;

procedure TConfigFrm.SaveYacLibraries;
begin
  Config.SaveYacLibs(lbYacLibs.Items);
end;

procedure TConfigFrm.btnAddlibClick(Sender: TObject);
var
  s : string;
begin
  with TSelectDirectoryDialog.Create(Application) do
  try
    Title := 'select Yac lib location';
    if Execute then
      begin
        s := IncludeTrailingPathDelimiter(filename) + 'library.ydb';
        if FileExists(s) and (lbYacLibs.Items.IndexOf(s) < 0) then
          lbYacLibs.Items.Add(s);
      end;
  finally
    Free;
  end;
end;

procedure TConfigFrm.btnRemovelibClick(Sender: TObject);
begin
  if lbYacLibs.ItemIndex >= 0 then
    lbYacLibs.Items.Delete(lbYacLibs.ItemIndex);
end;

procedure TConfigFrm.cbAutoImportReadStatesClick(Sender: TObject);
begin
  lbYacLibs.Enabled := cbAutoImportReadStates.Checked;
  btnAddlib.Enabled := cbAutoImportReadStates.Checked;
  btnRemovelib.Enabled := cbAutoImportReadStates.Checked;
  if not cbAutoImportReadStates.Checked then
    lbYacLibs.Clear;
end;


end.

