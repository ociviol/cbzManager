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
  StrUtils,
  Utils.SoftwareVersion;

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  dv, ev : integer;
begin
  dv := DoWebPGetDecoderVersion;
  ev := DoWebPGetEncoderVersion;

  Label1.Caption := GetFileVersionInternalName + ' ' +
                    GetFileVersion + ' © ' + GetFileVersionCopyright;
  if not InternalcWebpAvail then
    Listbox1.Items.Add('Internal Webp Encoder unavailable.')
  else
    Listbox1.Items.Add('Webp Encoder Version : ' + ifthen(ev > 0, IntToStr(ev), 'Unavailable'));

  if not InternaldWebpAvail then
    Listbox1.Items.Add('Internal Webp Decoder unavailable.')
  else
    Listbox1.Items.Add('Webp Decoder Version : ' + ifthen(dv > 0, IntToStr(dv), 'Unavailable'));
end;

end.

