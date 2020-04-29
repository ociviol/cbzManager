unit uCbzViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  uCbzViewerFrame, utils.Logger;

type

  { TFrmCbzViewer }

  TFrmCbzViewer = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    CbzViewerFrame : TCbzViewerFrame;
  public

  end;


procedure ShowComics(aLog : ILog; const aFilename : String);

implementation

procedure ShowComics(aLog : ILog; const aFilename: String);
var
  f : TFrmCbzViewer;
begin
  f := TFrmCbzViewer.Create(Application);
  with f do
  begin
    Caption := 'CbzViewer : ' + ExtractFilename(aFilename);
    CbzViewerFrame := TCbzViewerFrame.Create(f, aLog);
    CbzViewerFrame.Parent := f;
    ActiveControl := CbzViewerFrame.DrawGrid1;
    Show;
    CbzViewerFrame.Filename:=aFilename;
  end;
end;

{$R *.lfm}

{ TFrmCbzViewer }

procedure TFrmCbzViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CbzViewerFrame.Clear;
end;

procedure TFrmCbzViewer.FormCreate(Sender: TObject);
begin
  Height := Screen.Height - 100;
  Width := round(Height * 0.80);
end;

end.

