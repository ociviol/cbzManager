unit uLogReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  {$if defined(Linux) or defined(Darwin)}
    cthreads,
  {$endif}
  Utils.Logger, Utils.Searchfiles, Utils.treeview;

type

  { TFrmLogReader }

  TFrmLogReader = class(TForm)
    Memo1: TMemo;
    TreeView1: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    FLog : ILog;
    FThreadSearchFiles : TThread;

    procedure AfterShow(data : int64);
    procedure Search;
    procedure SearchEnded(Sender: TObject);
    function FoundFile(const aFileName: string;
                       IsNew: Boolean = False): TTreeNode;
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
  public
    constructor Create(AOwner : TComponent; aLog : ILog);
    destructor Destroy; override;
  end;

var
  FrmLogReader: TFrmLogReader;

implementation

{$R *.lfm}

uses
  Utils.zipfile;

{ TFrmLogReader }

constructor TFrmLogReader.Create(AOwner: TComponent; aLog: ILog);
begin
  FLog := aLog;
  inherited Create(AOwner);
end;

destructor TFrmLogReader.Destroy;
begin
  FLog := nil;
  inherited Destroy;
end;

procedure TFrmLogReader.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@AfterShow, 0);
end;

procedure TFrmLogReader.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  m : TMemoryStream;
begin
  if Node.Level > 0 then
  begin
    Memo1.Clear;
    with TZipFile.Create do
    try
      FileName:= IncludeTrailingPathDelimiter(FLog.ArchivePath) + TreeView1.Selected.Parent.Text;
      Active := True;
      m := GetFileStream(Integer(Node.Data));
      try
        Memo1.Lines.LoadFromStream(m);
      finally
        m.free;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFrmLogReader.AfterShow(data: int64);
begin
  Search;
end;

procedure TFrmLogReader.Search;
begin
  TreeView1.Items.BeginUpdate;
  FThreadSearchFiles := ThreadedSearchFiles(FLog.ArchivePath, '*.zip', @FoundFile, @SearchEnded,
                                            nil, 'scanning : ', [sfoRecurse]);
end;

procedure TFrmLogReader.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  TreeView1.Items.EndUpdate;
end;

function TFrmLogReader.FoundFile(const aFileName: string; IsNew: Boolean
  ): TTreeNode;
var
  i : integer;
begin
  result := TreeView1.Items.AddChild(nil, ExtractFileName(aFilename));
  with TZipFile.Create do
  try
    FileName:=aFilename;
    Active := True;
    for i:=0 to FileCount-1 do
      TreeView1.Items.AddChildObject(Result, FileNames[i], pointer(i));
  finally
    Free;
  end;
  //TreeView1.AddFilePath(aFilename);
  result := nil;
end;

procedure TFrmLogReader.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
begin

end;



end.

