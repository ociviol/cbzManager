unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}

uses
  Webp,
  Utils.SoftwareVersion;

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label1.Caption := GetFileVersionInternalName + ' ' +
                    GetFileVersion + ' © ' + GetFileVersionCopyright;
  if not InternalcWebpAvail then
    Listbox1.Items.Add('Internal Webp Encoder unavailable.')
  else
    Listbox1.Items.Add('Webp Encoder Version : ' + DoWebPGetEncoderVersion);

  if not InternaldWebpAvail then
    Listbox1.Items.Add('Internal Webp Decoder unavailable.')
  else
    Listbox1.Items.Add('Webp Decoder Version : ' + DoWebPGetDecoderVersion);
end;

end.

