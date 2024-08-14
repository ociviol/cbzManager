unit uCbzViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  uCbzViewerFrame, uLibraryClasses, utils.Logger, uConfig;

type

  { TFrmCbzViewer }

  TFrmCbzViewer = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CbzViewerFrame : TCbzViewerFrame;
    FItem : TFileItem;
  protected
    FConfig : TConfig;
  public

  end;


procedure ShowComics(aLog : ILog; aItem : TFileItem; aConfig : TConfig);

implementation

procedure ShowComics(aLog : ILog; aItem : TFileItem; aConfig : TConfig);
var
  f : TFrmCbzViewer;
begin
  f := TFrmCbzViewer.Create(Application);
  with f do
  try
    FConfig := aConfig;
    FItem := aItem;
    Caption := ExtractFilename(aItem.Filename);
    CbzViewerFrame := TCbzViewerFrame.Create(f, aConfig, aLog);
    CbzViewerFrame.Align:=alClient;
    CbzViewerFrame.Parent := f;
    ActiveControl := CbzViewerFrame.DrawGrid1;
    CbzViewerFrame.Filename:=aItem.Filename;
    Show;
  except
    On E: Exception do
      ShowMessage('Unable to open file : ' + E.Message);
  end;
end;

{$R *.lfm}

{ TFrmCbzViewer }

procedure TFrmCbzViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  try
    FConfig.SaveForm(Self);
    FItem.CurPage:=CbzViewerFrame.DrawGrid1.Position;
    if FItem.CurPage>=CbzViewerFrame.cbz.ImageCount-1 then
      FItem.ReadState:=True;
    CbzViewerFrame.Cbz.Close;
  finally
    CloseAction:=caFree;
  end;
end;

procedure TFrmCbzViewer.FormCreate(Sender: TObject);
begin
  Height := Screen.Height - 100;
  Width := round(Height * 0.80);
end;

procedure TFrmCbzViewer.FormShow(Sender: TObject);
begin
//  FConfig.RestoreForm(Self);
  CbzViewerFrame.SelectImage(FItem.CurPage);
end;

end.

