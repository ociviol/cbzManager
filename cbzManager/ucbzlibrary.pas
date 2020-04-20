unit uCbzLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Types,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Utils.SearchFiles, utils.Logger;

const
{$if defined(Linux)}
  CS_Path = '/media/psf/Livres/bds/Star Trek';
{$elseif defined(Darwin)}
{$elseif defined(sWindows)}
  CS_Path = 'V:\bds';
{$endif}

type

  { TFileItem }

  TFileItem = Class
  private
    FFilename : String;
    FImg : TBitmap;
    FLog : ILog;
    function GetCacheFilename: String;
    function GetImg: TBitmap;
  public
    constructor Create;
    constructor Create(aLog : ILog; const aFilename : String);
    destructor Destroy; override;
    property Img:TBitmap read GetImg;
    property Filename : String read FFilename;
    property CacheFilename : String read GetCacheFilename;
  end;

  { TItemList }

  TItemList = Class(TStringList)
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TCbzLibrary }

  TCbzLibrary = class(TForm)
    btnTopPath: TButton;
    dgLibrary: TDrawGrid;
    pnlbtns: TPanel;
    pnlPath: TPanel;
    StatusBar1: TStatusBar;
    procedure btnTopPathClick(Sender: TObject);
    procedure dgLibraryDblClick(Sender: TObject);
    procedure dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetCacheFileName: String;
  private
    Flog : ILog;
    FFileList : TItemList;
    FVisibleList : TStringlist;
    FThreadSearchFiles : TThread;
    FBtnList : TList;
    FRootPath,
    FCurrentPath : String;
    FLvl : Integer;

    procedure btnletterclick(sender : Tobject);
    procedure AfterShow(data : int64);
    procedure SizeGrid;
    procedure DefaultBtnClick(Sender: TObject);
    procedure FillGrid(bAddButton : Boolean = True);
    procedure SearchEnded(Sender: TObject);
    function FoundFile(const aFileName: string;
                            IsNew: Boolean = False): TTreeNode;
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    property CacheFileName : String read GetCacheFileName;
  public

  end;


var
  CbzLibrary: TCbzLibrary;

implementation

uses
  utils.zipfile, ucbz, uCbzViewer;

{$R *.lfm}

const
  CS_StampWidth = 120;
  CS_StampHeight = 160;


{ TFileItem }

function TFileItem.GetImg: TBitmap;
begin
  if not Assigned(FImg) then
    if FileExists(CacheFilename) then
    begin
      Fimg := TBitmap.Create;
      FImg.LoadFromFile(CacheFilename);
    end
  else
    with TCbz.Create(FLog) do
    try
      Open(FFilename, zmRead);
      Fimg := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
      FImg.SaveToFile(CacheFilename);
    finally
      free;
    end;

  result := FImg;
end;

function TFileItem.GetCacheFilename: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/Library/';
{$else}
    IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library\';
{$endif}
  ForceDirectories(result);
  result :=  result + Filename.Replace(':','-').Replace(PathDelim, '-');
  result := ChangeFileExt(result, '.bmp');
end;

constructor TFileItem.Create;
begin
  inherited;
  FImg := nil;
end;

constructor TFileItem.Create(aLog : ILog; const aFilename: String);
begin
  Create;
  FLog := aLog;
  FFilename := aFilename;
end;

destructor TFileItem.Destroy;
begin
  Flog := nil;
  if Assigned(FImg) then
    FImg.Free;
  inherited Destroy;
end;

{ TItemList }


constructor TItemList.Create;
begin
  inherited;
end;

destructor TItemList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TItemList.Clear;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    TFileItem(Objects[i]).free;
  inherited Clear;
end;


{ TCbzLibrary }

function GetFirstPath(const aPath : String; Lvl : integer = 2):String;
var
  a : TStringArray;
  i : integer;
begin
  a := aPath.Split([PathDelim]);
  result := '';
  for i:=0 to lvl do
    result := result + a[i] + PathDelim;
end;

function GetLastPath(const aPath : String):string;
var
  a : TStringArray;
begin
  a := aPath.Split([PathDelim]);
  result := a[High(a)];
end;

procedure TCbzLibrary.dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  p, x, y : integer;
  b : TBitmap;
  s : string;
  r : TRect;
  ts : TTextStyle;
begin
  p := (dgLibrary.ColCount * aRow) + aCol;
  if p < FVisibleList.Count then
    with dgLibrary, Canvas do
    begin
      FillRect(aRect);
      b := TFileItem(FVisibleList.Objects[p]).Img;
      s :=  GetLastPath(FVisibleList[p]); // GetLastPath(ExtractFilePath(TFileItem(FVisibleList.Objects[p]).Filename));
      X := (DefaultColWidth - b.Width) div 2;
      Y := 2; //(DefaultRowHeight - b.Height) div 2;
      Draw(aRect.Left + X, aRect.Top + Y, b);

      r := aRect;
      r.top := r.Bottom - (TextHeight(s) * 3);
      ts.Wordbreak:=True;
      ts.SingleLine:=False;
      ts.Alignment := taCenter;
      ts.Opaque:=False;
      ts.Layout := tlCenter;
      TextRect(r, 0, 0, s, ts);

      if gdFocused in aState then
        DrawFocusRect(aRect);
    end;
end;

procedure TCbzLibrary.dgLibraryDblClick(Sender: TObject);
var
  s : String;
begin
  s := FVisibleList[(dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col];
  if DirectoryExists(s) then
  begin
    FCurrentPath := s;
    FillGrid;
  end
  else
  if FileExists(s) then
    ShowComics(FLog, s)
  else
  ;
end;

procedure TCbzLibrary.btnTopPathClick(Sender: TObject);
begin
  while FBtnList.Count > 0 do
  begin
    TButton(FBtnList[0]).Free;
    FBtnList.Delete(0);
  end;
  FCurrentPath := btnTopPath.Caption;
  FillGrid(False);
end;

procedure TCbzLibrary.FormCreate(Sender: TObject);
var
  i : integer;
  c : char;
begin
  // start logger
  FLog := GetILog(
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/' +
{$else}
    IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\' +
{$endif}
    'cbzLibrary.log', True);

  Flog.Log('cbzLibrary started.');
  FRootPath := CS_Path;
  FCurrentPath := FRootPath;
  btnTopPath.Caption:=FRootPath;
  FLvl := length(FCurrentPath.Split([PathDelim]));

  FFileList := TItemList.Create;
  FBtnList := TList.Create;
  FVisibleList := TStringlist.Create;
  FVisibleList.Sorted:=True;

  if not FileExists(CacheFileName) then
    FThreadSearchFiles := ThreadedSearchFiles(FRootPath, '*.cbz', @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);

  for c := 'Z' downto 'A' do
    with TSpeedButton.Create(self) do
    begin
      Caption := c;
      align := altop;
      parent := pnlbtns;
      enabled := false;
      onclick := @btnletterclick;
    end;

end;

procedure TCbzLibrary.FormDestroy(Sender: TObject);
begin
  if Assigned(FThreadSearchFiles) then
    begin
      FThreadSearchFiles.Terminate;
      FThreadSearchFiles.WaitFor;
    end;
  Sleep(250);

  Flog := nil;
  FVisibleList.Free;
  FFileList.Free;
  FBtnList.Free;
end;

procedure TCbzLibrary.FormResize(Sender: TObject);
begin
  SizeGrid;
end;

procedure TCbzLibrary.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@AfterShow, 0);
end;

function TCbzLibrary.GetCacheFileName: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
  expandfilename('~/') + CS_CONFIG_PATH + '/Library/' +
{$else}
  IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library\' +
{$endif}
  'cbzLibrary.db';
  ForceDirectories(ExtractFilePath(result));
end;

procedure TCbzLibrary.btnletterclick(sender: Tobject);
begin

end;

procedure TCbzLibrary.AfterShow(data : int64);
var
  i : integer;
begin
  if FileExists(CacheFileName) then
  begin
    with TStringlist.CReate do
    try
      LoadFromFile(CacheFileName);
      for i:= 0 to Count - 1 do
        FoundFile(Strings[i]);
    finally
      Free;
    end;
  end;
end;


procedure TCbzLibrary.SizeGrid;
var
  c,r : integer;
begin
  with dgLibrary do
  begin
    c := ClientWidth div DefaultColWidth;
    r := ClientHeight div DefaultRowHeight;
    if ColCount <> c then
      ColCount:=c;
    if RowCount <> (FVisibleList.Count div c) + 1 then
      RowCount:=(FVisibleList.Count div c) + 1;
    Invalidate;
    Update;
  end;
end;

procedure TCbzLibrary.DefaultBtnClick(Sender: TObject);
var
  i : integer;
  s : string;
begin
  i := FBtnList.IndexOf(Pointer(Sender));
  if (i >= 0) and (i + 1 < FBtnList.Count) then
  begin
    s := IncludeTrailingPathDelimiter(FRootPath) + TButton(FBtnList[i]).Caption;
    FCurrentPath := s;
    while (i + 1 < FBtnList.Count) do
    begin
      TButton(FBtnList[FBtnList.Count-1]).Free;
      FBtnList.Delete(FBtnList.Count-1);
    end;
    FillGrid(False);
  end;
end;

procedure TCbzLibrary.FillGrid(bAddButton : Boolean = True);
var
  s : string;
  i : integer;
begin
  Flvl := length(FCurrentPath.Split([PathDelim]));
  FVisibleList.Clear;
  SizeGrid;

  if bAddButton then
  begin
    FBtnList.Add(TButton.Create(self));
    with tButton(FBtnList[FBtnList.Count-1]) do
    begin
      AutoSize := True;
      Caption := GetLastPath(FCurrentPath);
      Align := alLeft;
      Left := 10000;
      Parent := pnlPath;
      OnClick := @DefaultBtnClick;
    end;
  end;

  for i := 0 to FFileList.Count - 1 do
    if FFileList[i].StartsWith(FCurrentPath) then
    begin
      s := ExtractFilePath(FFileList[i]);
      if Length(s.Split([PathDelim])) > FLvl then
        s := GetFirstPath(s, FLvl)
      else
        s := FFileList[i];

      with FVisibleList do
        if IndexOf(s) < 0 then
        begin
          AddObject(s, FFileList.Objects[i]);
          SizeGrid;
          Application.ProcessMessages;
        end;
    end;

  SizeGrid;
end;

procedure TCbzLibrary.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  StatusBar1.SimpleText := 'Done.';
  //FFileList.SaveToFile(GetCacheFileName);
end;

function TCbzLibrary.FoundFile(const aFileName: string; IsNew: Boolean
  ): TTreeNode;
var
  fi : TFileItem;
  acol, arow : integer;
  s : string;
begin
  if (not FileExists(aFilename)) then
    exit;

  fi := TFileItem.Create(FLog, aFilename);
  FFileList.AddObject(aFilename, fi);

  if FCurrentPath = FRootPath then
  begin
    aRow := (FVisibleList.Count div dgLibrary.ColCount);
    aCol := FVisibleList.Count - (aRow * dgLibrary.ColCount);

    s := ExtractFilePath(aFilename);
    if Length(s.Split([PathDelim])) > FLvl then
      s := GetFirstPath(s, FLvl)
    else
      s := aFilename;

    //s := GetFirstPath(ExtractFilePath(aFilename));
    with FVisibleList do
      if IndexOf(s) < 0 then
      begin
        AddObject(s, fi);
        fi.Img.Modified:=false;
      end;

    if dgLibrary.RowCount = aRow then
      dgLibrary.RowCount := aRow + 1;
    if dgLibrary.IsCellVisible(aCol, aRow) then
      dgLibrary.InvalidateCell(aCol, aRow);
  end;
end;

procedure TCbzLibrary.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
begin
  StatusBar1.SimpleText := Msg;
end;

end.

