unit uFileCleaner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

  { TTFormFilenameCleanerConfig }

  TTFormFilenameCleanerConfig = class(TForm)
    btnAdd: TSpeedButton;
    btnCancel: TButton;
    btnOk: TButton;
    btnRemove: TSpeedButton;
    cbRmDashes: TCheckBox;
    cbRmDots: TCheckBox;
    cbRmParenthesis: TCheckBox;
    cbRmUnderscores: TCheckBox;
    lbKeywords: TListBox;
    lblKeywords: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbKeywordsClick(Sender: TObject);
  private
    //FLngFile : String;
  public
  end;


implementation

{$R *.lfm}

uses
  uXmldoc, uCbz;

{ TTFormFilenameCleanerConfig }

procedure TTFormFilenameCleanerConfig.btnAddClick(Sender: TObject);
var
  s : string;
begin
  s := '';
  if InputQuery('New keyword to remove', 'New keyword', s) then
    lbKeywords.Items.Add(s);
end;

procedure TTFormFilenameCleanerConfig.btnOkClick(Sender: TObject);
var
  Node : TXmlElement;
  i : integer;
begin
  with TXmlDoc.Create do
  try
    with DocumentElement.GetNode('Config') do
    begin
      SetAttributeBool('RemoveDots', cbRmDots.Checked);
      SetAttributeBool('RemoveDashes', cbRmDashes.Checked);
      SetAttributeBool('Value', cbRmUnderscores.Checked);
      SetAttributeBool('Value', cbRmParenthesis.Checked);
      Node := GetNode('Keywords');
      for i := 0 to lbKeywords.Items.Count - 1 do
        Node.AddChildNode('Keyword').Text := lbKeywords.Items[i];
    end;
    SaveToFile(TCbz.FilenameCleaningConfigFile);
  finally
    Free;
  end;
end;

procedure TTFormFilenameCleanerConfig.btnRemoveClick(Sender: TObject);
begin
  with lbKeywords do
  begin
    Items.Delete(ItemIndex);
    btnRemove.Enabled := false;
  end;
end;

procedure TTFormFilenameCleanerConfig.FormCreate(Sender: TObject);
var
  Node : TXmlElement;
  i : integer;
  XDoc : TXmlDoc;
begin
  caption := 'Filename Cleaner Configuration';

  XDoc := TXmlDoc.Create;
  with XDoc do
  try
    if FileExists(TCbz.FilenameCleaningConfigFile) then
      LoadFromFile(TCbz.FilenameCleaningConfigFile);

    with DocumentElement.GetNode('Config') do
    begin
      cbRmDots.Checked := GetAttributeBool('RemoveDots', True);
      cbRmDashes.Checked := GetAttributeBool('RemoveDashes', True);
      cbRmUnderscores.Checked := GetAttributeBool('Value', True);
      cbRmParenthesis.Checked := GetAttributeBool('Value', True);
      Node := GetNode('Keywords');
      for i := 0 to Node.NbElements - 1 do
        lbKeywords.Items.Add(Node.Elements[i].Text);
    end;
  finally
    Xdoc.Free;
  end;
end;

procedure TTFormFilenameCleanerConfig.lbKeywordsClick(Sender: TObject);
begin
  btnRemove.Enabled := lbKeywords.ItemIndex >= 0;
end;

end.

