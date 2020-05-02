unit uCbzViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  uCbzViewerFrame, utils.Logger, uConfig;

type

  { TFrmCbzViewer }

  TFrmCbzViewer = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CbzViewerFrame : TCbzViewerFrame;
  protected
    FConfig : TConfig;
  public

  end;


procedure ShowComics(aLog : ILog; const aFilename : String; aConfig : TConfig);

implementation

procedure ShowComics(aLog : ILog; const aFilename: String; aConfig : TConfig);
var
  f : TFrmCbzViewer;
begin
  f := TFrmCbzViewer.Create(Application);
  with f do
  begin
    FConfig := aConfig;
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
  FConfig.SaveForm(Self);
  CloseAction:=caFree;
end;

procedure TFrmCbzViewer.FormCreate(Sender: TObject);
begin
  Height := Screen.Height - 100;
  Width := round(Height * 0.80);
end;

procedure TFrmCbzViewer.FormShow(Sender: TObject);
begin
  FConfig.RestoreForm(Self);
end;

end.

