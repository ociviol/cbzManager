unit uCbzLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Types,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Utils.SearchFiles, utils.Logger, uxmldoc, uConfig;


type

  { TThreadStringList }

  TThreadStringList = Class(TStringList)
  private
    FLock : TThreadList;
  protected
    function Get(Index: Integer): string; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TFileItem }

  TFileItem = Class
  private
    FFilename : String;
    FImg : TPicture;
    FLog : ILog;
    FText: String;
    FReadState : Boolean;
    function GetCacheFilename: String; inline;
    function GetImg: TPicture;
  protected
    procedure SaveToXml(aNode : TXmlElement);
    procedure LoadFromXml(aNode : TXmlElement);
  public
    constructor Create;
    constructor Create(aLog : ILog; const aFilename : String);
    destructor Destroy; override;
    property Img:TPicture read GetImg;
    property Filename : String read FFilename;
    property CacheFilename : String read GetCacheFilename;
    property Text : String Read FText write FText;
    property ReadState : Boolean read FReadState;
  end;

  { TItemList }

  TItemList = Class(TThreadStringList)
  private
    Flog : ILog;
  protected
  public
    constructor Create(alog : ILog);
    destructor Destroy; override;
    procedure Clear; override;
    procedure LoadFromFile(const aFilename : String); override;
    procedure SaveToFile(const aFilename : String); override;
  end;


  { TThreadFill }

  TThreadFill = Class(TThread)
    private
      FFileList : TItemList;
      FVisibleList : TStringlist;
      FCurrentPath : String;
      FLvl : Integer;
      FProgress : TProgressEvent;
      procedure SizeGrid;
  public
    constructor Create(aFileList : TItemList; aVisibleList : TStringlist;
                       const aCurrentPath : String; aLvl : Integer;
                       aOnTerminate : TNotifyEvent;
                       aProgress : TProgressEvent = nil);
    procedure Execute; override;
  end;

  { TCbzLibrary }

  TCbzLibrary = class(TForm)
    btnReturn: TSpeedButton;
    btnTopPath: TButton;
    btnRefresh: TButton;
    dgLibrary: TDrawGrid;
    pnlbtns: TPanel;
    pnlPath: TPanel;
    StatusBar1: TStatusBar;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnTopPathClick(Sender: TObject);
    procedure dgLibraryDblClick(Sender: TObject);
    procedure dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
  private
    function GetCacheFileName: String;
  private
    Flog : ILog;
    FFileList : TItemList;
    FVisibleList : TThreadStringList;
    FThreadSearchFiles : TThread;
    FBtnList : TList;
    FRootPath,
    FCurrentPath : String;
    FLvl : Integer;
    Fconfig : TConfig;
    FPathPos : array of TPoint;
    FFillThread : TThreadFill;

    procedure btnletterclick(sender : Tobject);
    procedure MakeStamp(data : int64);
    procedure AfterShow(data : int64);
    procedure DoSizegrid(data : int64);
    procedure SizeGrid;
    procedure DefaultBtnClick(Sender: TObject);
    procedure FillGrid(bAddButton : Boolean = True);
    procedure SearchEnded(Sender: TObject);
    function FoundFile(const aFileName: string;
                            IsNew: Boolean = False): TTreeNode;
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    property CacheFileName : String read GetCacheFileName;
    procedure VisibleListChanged(Sender : TObject);
    procedure ThreadFillTerminate(Sender : TObject);
  public
    constructor Create(aOwner : TComponent; aConfig : TConfig);
    property RootPath : String read FRootPath write FRootPath;
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
  result := '';
  a := aPath.Split([PathDelim]);
  result := a[High(a)];
end;

{ TThreadStringList }

function TThreadStringList.Get(Index: Integer): string;
begin
  Flock.LockList;
  try
    Result:=inherited Get(Index);
  finally
    FLock.UnlockList;
  end;
end;

function TThreadStringList.GetObject(Index: Integer): TObject;
begin
  Flock.LockList;
  try
    Result:=inherited GetObject(Index);
  finally
    FLock.UnlockList;
  end;
end;

procedure TThreadStringList.Put(Index: Integer; const S: string);
begin
  Flock.LockList;
  try
    inherited Put(Index, S);
  finally
    FLock.UnlockList;
  end;
end;

procedure TThreadStringList.PutObject(Index: Integer; AObject: TObject);
begin
  Flock.LockList;
  try
    inherited PutObject(Index, AObject);
  finally
    FLock.UnlockList;
  end;
end;

function TThreadStringList.GetCount: Integer;
begin
  Flock.LockList;
  try
    Result:=inherited GetCount;
  finally
    FLock.UnlockList;
  end;
end;

constructor TThreadStringList.Create;
begin
  Flock := TThreadList.Create;
  inherited Create;
end;

destructor TThreadStringList.Destroy;
begin
  flock.Free;
  inherited Destroy;
end;


{ TThreadFill }

procedure TThreadFill.SizeGrid;
begin

end;

constructor TThreadFill.Create(aFileList: TItemList; aVisibleList: TStringlist;
                               const aCurrentPath : String; aLvl : Integer;
                               aOnTerminate : TNotifyEvent;
                               aProgress : TProgressEvent = nil);
begin
  FFileList:= aFileList;
  FVisibleList := aVisibleList;
  FCurrentPath := aCurrentPath;
  FLvl := aLvl;
  FreeOnTerminate:=True;
  OnTerminate:=aOnTerminate;
  FProgress := aProgress;
  inherited Create(False);
end;

procedure TThreadFill.Execute;
var
  i : integer;
  s : string;
begin
  while not Terminated do
  try
    for i := 0 to FFileList.Count - 1 do
    begin
      if Terminated then
        Exit;

      if FFileList[i].StartsWith(FCurrentPath) then
      begin
        s := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileList[i]));
        if Length(s.Split([PathDelim])) > FLvl then
          s := GetFirstPath(s, FLvl)
        else
          s := FFileList[i];

        with FVisibleList do
          if IndexOf(s) < 0 then
            AddObject(s, FFileList.Objects[i]);

        if (i mod 25) = 0 then
          Sleep(25);
      end;
    end;
    Terminate;
  except
    Terminate;
  end;
end;


{ TFileItem }

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

function TFileItem.GetImg: TPicture;
var
  b : TBitmap;
begin
  if not Assigned(FImg) then
  begin
    Fimg := TPicture.Create;
    if FileExists(CacheFilename) then
      FImg.LoadFromFile(CacheFilename)
  else
    with TCbz.Create(FLog) do
    try
      Open(FFilename, zmRead);
      b := GenerateStamp(0, CS_StampWidth, CS_StampHeight);
      try
        Fimg.Bitmap.Assign(b);
      finally
        b.Free;
      end;
      FImg.SaveToFile(CacheFilename, 'jpg');
    finally
      free;
    end;
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
  result := ChangeFileExt(result, '.jpg');
end;

procedure TFileItem.SaveToXml(aNode: TXmlElement);
begin
  aNode.SetAttribute('Filename', FFilename);
  aNode.SetAttributeBool('ReadState', ReadState);
end;

procedure TFileItem.LoadFromXml(aNode: TXmlElement);
begin
  FReadState := aNode.GetAttributeBool('ReadState');
  FFilename:= aNode.GetAttribute('Filename');
end;


{ TItemList }

constructor TItemList.Create(alog : ILog);
begin
  Flog := alog;
  inherited Create;
end;

destructor TItemList.Destroy;
begin
  Clear;
  Flog := nil;
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

procedure TItemList.SaveToFile(const aFilename: String);
var
  i : integer;
  el, root : TXmlElement;
begin
  with TXMLDoc.Create do
  try
    root := CreateNewDocumentElement('Library');
    for i := 0 to Count - 1 do
   begin
     el := root.AddChildNode('Comic');
     TFileItem(Objects[i]).SaveToXml(el);
   end;
    SaveToFile(aFilename);
  finally
    Free;
  end;
end;

procedure TItemList.LoadFromFile(const aFilename: String);
var
  i : integer;
  fi : TFileItem;
begin
  Clear;
  with TXMLDoc.Create do
  try
    LoadFromFile(aFilename);
    with DocumentElement do
      for i := 0 to NbElements - 1 do
      begin
        fi := TFileItem.Create(Flog, '');
        fi.LoadFromXml(Elements[i]);
        AddObject(fi.Filename, fi);
      end;
  finally
    Free;
  end;
end;


{ TCbzLibrary }

constructor TCbzLibrary.Create(aOwner: TComponent; aConfig: TConfig);
begin
  FConfig := aConfig;
  FFillThread := nil;
  inherited Create(aOwner);
end;

procedure TCbzLibrary.FormCreate(Sender: TObject);
var
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

  if FConfig.Libraryleft <> 0 then
    left := FConfig.Libraryleft;
  if FConfig.LibraryTop <> 0 then
    top := FConfig.LibraryTop;
  if FConfig.LibraryWidth <> 0 then
    Width := FConfig.LibraryWidth;
  if FConfig.LibraryHeight <> 0 then
    Height := FConfig.LibraryHeight;

  Flog.Log('cbzLibrary started.');
  FFileList := TItemList.Create(Flog);
  FBtnList := TList.Create;
  FVisibleList := TThreadStringList.Create;
  FVisibleList.OnChanging := @VisibleListChanged;
  FVisibleList.Sorted:=True;

  for c := 'Z' downto 'A' do
    with TSpeedButton.Create(self) do
    begin
      Caption := c;
      align := altop;
      parent := pnlbtns;
      //enabled := false;
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

  FVisibleList.Free;
  FFileList.Free;
  FBtnList.Free;
  Flog.Log('cbzLibrary destroyed.');
  Flog := nil;
end;

procedure TCbzLibrary.dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  p, x, y : integer;
  pic : TPicture;
  s : string;
  r : TRect;
  ts : TTextStyle;
begin
  p := (dgLibrary.ColCount * aRow) + aCol;
  if p < FVisibleList.Count then
    with dgLibrary, Canvas do
    begin
      FillRect(aRect);
      if Assigned(FVisibleList.Objects[p]) then
      begin
        pic := TFileItem(FVisibleList.Objects[p]).Img;
        s := GetLastPath(ExcludeTrailingPathDelimiter(FVisibleList[p]));
        //showmessage('s='+s);
        X := (DefaultColWidth - pic.Graphic.Width) div 2;
        Y := 2; //(DefaultRowHeight - b.Height) div 2;
        Draw(aRect.Left + X, aRect.Top + Y, pic.Graphic);

        r := aRect;
        r.top := r.Bottom - (TextHeight(s) * 3);
        ts.Wordbreak:=True;
        ts.SingleLine:=False;
        ts.Alignment := taCenter;
        ts.Opaque:=False;
        ts.Layout := tlCenter;
        TextRect(r, 0, 0, s, ts);
      end;

      if gdFocused in aState then
        DrawFocusRect(aRect);
    end;
end;

procedure TCbzLibrary.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FConfig.Libraryleft := Left;
  FConfig.LibraryTop := Top;
  FConfig.LibraryWidth := Width;
  FConfig.LibraryHeight := Height;

  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;
end;

procedure TCbzLibrary.dgLibraryDblClick(Sender: TObject);
var
  s : String;
  c : integer;
begin
  c := (dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col;
  if c >= FVisibleList.Count then
    Exit;
  s := FVisibleList[c];
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

procedure TCbzLibrary.btnRefreshClick(Sender: TObject);
begin
  if not Assigned(FThreadSearchFiles) then
  begin
    btnRefresh.enabled := False;
    FFileList.Clear;
    FVisibleList.Clear;
    SizeGrid;
    FThreadSearchFiles := ThreadedSearchFiles(FRootPath, '*.cbz', @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
end;

procedure TCbzLibrary.FormResize(Sender: TObject);
begin
  SizeGrid;
end;

procedure TCbzLibrary.FormShow(Sender: TObject);
begin
  FCurrentPath := FRootPath;
  btnTopPath.Caption:=FCurrentPath;
  FLvl := length(FCurrentPath.Split([PathDelim]));

  if not FileExists(CacheFileName) then
    FThreadSearchFiles := ThreadedSearchFiles(FCurrentPath, '*.cbz', @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  Application.QueueAsyncCall(@AfterShow, 0);
end;

procedure TCbzLibrary.btnReturnClick(Sender: TObject);
begin
  if FLvl > 1 then
    TButton(FBtnList[FBtnList.Count-2]).Click
  else
    btnReturn.Enabled := False;
end;

function TCbzLibrary.GetCacheFileName: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
  expandfilename('~/') + CS_CONFIG_PATH + '/Library/' +
{$else}
  IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library\' +
{$endif}
  'cbzLibrary.xml';
  ForceDirectories(ExtractFilePath(result));
end;

procedure TCbzLibrary.btnletterclick(sender: Tobject);
var
  p, c, r, i : integer;
  s, ltr : string;
begin
  p := -1;
  Screen.Cursor := crHourGlass;
  try
    for i:= 0 to FVisibleList.Count - 1 do
    begin
      ltr := TSpeedButton(Sender).Caption[1];
      s := GetLastPath(ExcludeTrailingPathDelimiter(FVisibleList[i])).ToUpper;
      if s.StartsWith(ltr) then
        with dgLibrary do
        begin
          p := i;
          r := p div ColCount;
          c := p - (r * ColCount);
          TopRow := r;
          col := c;
          break;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCbzLibrary.MakeStamp(data: int64);
var
  img : TPicture;
begin
  img := TFileItem(data).Img;
end;

procedure TCbzLibrary.AfterShow(data : int64);
begin
  if FileExists(CacheFileName) then
  try
    FFileList.LoadFromFile(CacheFileName);
    btnTopPathClick(Self);
  finally
    btnRefresh.Enabled:=True;
  end;
end;

procedure TCbzLibrary.SizeGrid;
var
  c : integer;
begin
  with dgLibrary do
  begin
    BeginUpdate;
    try
      c := ClientWidth div DefaultColWidth;
      if ColCount <> c then
        ColCount:=c;
      if RowCount <> (FVisibleList.Count div c) + 1 then
        RowCount:=(FVisibleList.Count div c) + 1;
    finally
      EndUpdate;
    end;
    //Invalidate;
    //Update;
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
  z, oldtoprow, oldrow : longint;
begin
  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;
  Progress(Self, 0, 0, 0, 'Loading folder...');
  btnRefresh.Enabled:=False;
  try
    if Length(FPathPos) <= 0 then
    begin
      SetLength(FPathPos, 1);
    end;

    FPathPos[FLvl-1].x := dgLibrary.Col;
    z := dgLibrary.TopRow shl 16;
    inc(z, dgLibrary.Row);
    FPathPos[FLvl-1].y := z;

    Flvl := length(FCurrentPath.Split([PathDelim]));
    FVisibleList.Clear;
    SizeGrid;

    if Flvl > Length(FPathPos) then
      SetLength(FPathPos, Flvl);

    if bAddButton then
    begin
      FBtnList.Add(TButton.Create(self));
      with tButton(FBtnList[FBtnList.Count-1]) do
      begin
        AutoSize := True;
        Caption := GetLastPath(ExcludeTrailingPathDelimiter(FCurrentPath));
        Align := alLeft;
        Left := 10000;
        Parent := pnlPath;
        OnClick := @DefaultBtnClick;
      end;
    end;

    if (FPathPos[FLvl-1].x <> 0) or (FPathPos[FLvl-1].y <> 0) then
    begin
      dgLibrary.Col := FPathPos[FLvl-1].x;
      oldtoprow := (FPathPos[FLvl-1].y shr 16);
      oldrow := (FPathPos[FLvl-1].y shl 16);
      oldrow := oldrow shr 16;
    end;

    FFillThread := TThreadFill.Create(FFileList, FVisibleList, FCurrentPath, FLvl, @ThreadFillTerminate);
    Exit;

    for i := 0 to FFileList.Count - 1 do
    begin
      if FFileList[i].StartsWith(FCurrentPath) then
      begin
        s := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileList[i]));
        if Length(s.Split([PathDelim])) > FLvl then
          s := GetFirstPath(s, FLvl)
        else
          s := FFileList[i];

        with FVisibleList do
          if IndexOf(s) < 0 then
          begin
            AddObject(s, FFileList.Objects[i]);
            SizeGrid;

            with dgLibrary do
            begin
              if (RowCount >= oldrow) and (RowCount > 0) then
                if (FPathPos[FLvl-1].y <> 0) and
                   ((TopRow <> oldtoprow) or (Row <> oldrow) or (Col <> FPathPos[FLvl-1].x)) then
                begin
                  Col := FPathPos[FLvl-1].x;
                  TopRow := oldtoprow;
                  Row := oldrow;
                  Invalidate;
                end;
              update;
            end;

            if (i mod 10) = 0 then
              Application.ProcessMessages;
          end;
      end;
    end;
    SizeGrid;

  finally
    btnRefresh.Enabled:=True;
  end;
end;

procedure TCbzLibrary.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  StatusBar1.SimpleText := 'Done.';

  FFileList.SaveToFile(GetCacheFileName);
  btnRefresh.Enabled:=True;
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
  fi.Text := GetLastPath(aFilename);

  if FCurrentPath = FRootPath then
  begin
    aRow := (FVisibleList.Count div dgLibrary.ColCount);
    aCol := FVisibleList.Count - (aRow * dgLibrary.ColCount);

    s := ExcludeTrailingPathDelimiter(ExtractFilePath(aFilename));
    if Length(s.Split([PathDelim])) > FLvl then
      s := GetFirstPath(s, FLvl)
    else
      s := aFilename;

    //s := GetFirstPath(ExtractFilePath(aFilename));
    with FVisibleList do
      if IndexOf(s) < 0 then
      begin
        AddObject(s, fi);
        Application.QueueAsyncCall(@MakeStamp, int64(fi));
      end;

    if dgLibrary.RowCount = aRow then
      dgLibrary.RowCount := aRow + 1;
    if dgLibrary.IsCellVisible(aCol, aRow) then
      dgLibrary.InvalidateCell(aCol, aRow);
    //Application.ProcessMessages;
  end;
end;

procedure TCbzLibrary.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
begin
  StatusBar1.SimpleText := Msg;
end;

procedure TCbzLibrary.DoSizegrid(data : int64);
begin
  SizeGrid;
end;

procedure TCbzLibrary.VisibleListChanged(Sender: TObject);
begin
  if FVisibleList.Count > 0 then
    Application.QueueAsyncCall(@DoSizegrid, 0);
end;

procedure TCbzLibrary.ThreadFillTerminate(Sender: TObject);
begin
  Progress(Self, 0, 0, 0, 'Ready.');
  FFillThread := nil;
  SizeGrid;
end;



end.

