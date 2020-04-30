unit uCbzLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Menus, Types,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Utils.SearchFiles, utils.Logger, uConfig,
  Utils.Strings, uLibraryClasses;


type


  { TThreadFill }

  TThreadFill = Class(TThread)
  private
    FFileList : TItemList;
    FVisibleList : TStringlist;
    FCurrentPath : String;
    FLvl : Integer;
    FProgress : TProgressEvent;
    FProgressChar : Char;
    FCancelled : Boolean;
    FDisplayFilters : TDisplayFilters;

    procedure DoProgress;
  public
    constructor Create(aFileList : TItemList; aVisibleList : TStringlist;
                       const aCurrentPath : String; aLvl : Integer;
                       aDisplayFilters : TDisplayFilters;
                       aOnTerminate : TNotifyEvent;
                       aProgress : TProgressEvent = nil);
    procedure Execute; override;
    property Cancelled : Boolean read FCancelled;
  end;

  { TThreadConv }

  TThreadConv = Class(TThread)
  private
    FVal : Integer;
    FFileList: TItemList;
    FProgress : TProgressEvent;
    FLog : ILog;

    procedure DoProgress;
  public
    constructor Create(aLog : ILog; aFileList : TItemList; aProgress : TProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TCbzLibrary }

  TCbzLibrary = class(TForm)
    btnReturn: TSpeedButton;
    btnTopPath: TButton;
    btnRefresh: TButton;
    cbHideRead: TCheckBox;
    cbVisibleDates: TComboBox;
    dgLibrary: TDrawGrid;
    edtSearch: TEdit;
    Label1: TLabel;
    mnuReadStatus: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlbtns: TPanel;
    pnlPath: TPanel;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnTopPathClick(Sender: TObject);
    procedure cbHideReadClick(Sender: TObject);
    procedure cbVisibleDatesChange(Sender: TObject);
    procedure dgLibraryDblClick(Sender: TObject);
    procedure dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgLibraryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgLibraryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure mnuReadStatusClick(Sender: TObject);
  private
    function GetCacheFileName: String;
  private
    Flog : ILog;
    FFileList : TItemList;
    FVisibleList : TThreadStringList;
    FThreadSearchFiles : TThread;
    FBtnList : TList;
    FCurrentPath : String;
    FLvl : Integer;
    Fconfig : TConfig;
    FPathPos : array of TPoint;
    FFillThread : TThreadFill;
    FDisplayFilters : TDisplayFilters;
    FInQueue : Integer;
    FThreadConv : TThreadConv;

    procedure UpdateNbItems;
    procedure SwitchPath(const aLibPath : String);
    procedure CheckModified;
    procedure btnletterclick(sender : Tobject);
    procedure AfterShow(data : int64);
    procedure DoSizegrid(data : int64);
    procedure SizeGrid;
    procedure DefaultBtnClick(Sender: TObject);
    procedure DoFillGrid(data : int64);
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
    constructor Create(aOwner : TComponent; aConfig : TConfig); reintroduce;
    //property RootPath : String read FRootPath write FRootPath;
  end;


var
  CbzLibrary: TCbzLibrary;

implementation

uses
  Math, StrUtils, utils.zipfile, ucbz, uCbzViewer;

{$R *.lfm}


{ TThreadConv }

constructor TThreadConv.Create(aLog : ILog; aFileList: TItemList; aProgress : TProgressEvent = nil);
begin
  Flog := aLog;
  FFileList := aFileList;
  FreeOnTerminate:=True;
  FPRogress := aProgress;
  inherited Create(False);
end;

destructor TThreadConv.Destroy;
begin
  FLog := nil;
  inherited Destroy;
end;

procedure TThreadConv.DoProgress;
begin
  if Assigned(FProgress) then
    if FVal < FFileList.Count - 1 then
      FProgress(Self, 1, 0, 0, 'Scrubing stamps : ' +
                IntToStr((FVal * 100) div FFileList.Count) + '%')
    else
      FProgress(Self, 1, 0, 0, 'Scrub Done.')
end;

procedure TThreadConv.Execute;
var
  cnt : integer;
  b : TBitmap;
begin
  Sleep(1000);
  while not Terminated do
  begin
    if (FFileList.StampLessCount > 0) or
       (FFileList.DeletedCount > 0) then
    begin
      FLog.Log('TThreadConv.Execute: Starting scrub.');
      cnt := FFileList.Count;
      FVal := 0;
      while (cnt > FVal) do
      begin
        if Terminated then
            Exit;

        // remove invalid entries
        with TFileItem(FFileList.Objects[FVal]) do
          if not FileExists(Filename) then
          begin
            FFileList.Delete(FVal);
            FLog.Log('TThreadConv.Execute: Deleted:' + Filename);
            cnt := FFileList.Count;
          end;

        // make stamp if needed
        with TFileItem(FFileList.Objects[FVal]) do
        begin
          b := GenerateStamp;
          if Assigned(b) then
            b.Free;
        end;

        inc(FVal);
        if (FVal mod 50) = 0 then
          Synchronize(@DoProgress);
          //yield;
        Sleep(25);

        cnt := FFileList.Count;
      end;
      Synchronize(@DoProgress);
      FLog.Log('TThreadConv.Execute: Scrub done.');
    end
    else
      Sleep(5000);
  end;
end;

{ TThreadFill }

constructor TThreadFill.Create(aFileList: TItemList; aVisibleList: TStringlist;
                               const aCurrentPath : String; aLvl : Integer;
                               aDisplayFilters : TDisplayFilters;
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
  FProgressChar := '|';
  FCancelled:=FAlse;
  FDisplayFilters:=aDisplayFilters;
  inherited Create(False);
end;

procedure TThreadFill.DoProgress;
begin
  case FProgressChar of
    '|': FProgressChar := '/';
    '/': FProgressChar := '-';
    '-': FProgressChar := '\';
    '\': FProgressChar := '|';
  end;
  FProgress(Self, 0, 0, 0, 'Loading folder ' + FProgressChar);
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
      begin
        FCancelled:=True;
        Exit;
      end;

      if FFileList[i].StartsWith(FCurrentPath) then
      begin
        s := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileList[i]));
        if Length(s.Split([PathDelim])) > FLvl then
          s := GetFirstPath(s, FLvl)
        else
          s := FFileList[i];

        if (dfUnread in FDisplayFilters) then
          if TFileItem(FFileList.Objects[i]).ReadState then
            continue;

        with FVisibleList do
          if FileExists(s) then
            AddObject(s, FFileList.Objects[i])
          else
          if IndexOf(s) < 0 then
            AddObject(s, FFileList.Objects[i]);

        if (i mod 250) = 0 then
        begin
          Synchronize(@DoProgress);
          Sleep(10);
        end;
      end;
    end;
    Terminate;
  except
    Terminate;
  end;
end;




{ TCbzLibrary }

constructor TCbzLibrary.Create(aOwner: TComponent; aConfig: TConfig);
begin
  FConfig := aConfig;
  FFillThread := nil;
  FThreadConv:=nil;
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
    'cbzLibrary.log', Fconfig.DoLog);

  if FConfig.Libraryleft <> 0 then
    left := FConfig.Libraryleft;
  if FConfig.LibraryTop <> 0 then
    top := FConfig.LibraryTop;
  if FConfig.LibraryWidth <> 0 then
    Width := FConfig.LibraryWidth;
  if FConfig.LibraryHeight <> 0 then
    Height := FConfig.LibraryHeight;

  Flog.Log('cbzLibrary started.');
  FInQueue := 0;
  FDisplayFilters := [dfAll];
  cbHideRead.Checked := Fconfig.LibraryHideRead;
  FFileList := TItemList.Create(Flog);
  FFileList.RootPath:=Fconfig.LibPath;
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

procedure TCbzLibrary.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FConfig.Libraryleft := Left;
  FConfig.LibraryTop := Top;
  FConfig.LibraryWidth := Width;
  FConfig.LibraryHeight := Height;

  CheckModified;

  FThreadConv.Terminate;

  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;
end;

procedure TCbzLibrary.FormResize(Sender: TObject);
begin
  SizeGrid;
  StatusBar1.Panels[0].Width := (StatusBar1.ClientWidth div 3) * 2;
end;

procedure TCbzLibrary.FormShow(Sender: TObject);
begin
  dgLibrary.ColCount:=1;
  dgLibrary.RowCount:=1;
  Application.QueueAsyncCall(@AfterShow, 0);
end;

procedure TCbzLibrary.AfterShow(data : int64);
begin
  if FileExists(CacheFileName) then
  try
    FFileList.LoadFromFile(CacheFileName);
    FCurrentPath := FFileList.RootPath;
    FLvl := length(FCurrentPath.Split([PathDelim]));
    btnTopPath.Caption:=FCurrentPath;
    btnTopPath.Click;
  finally
    btnRefresh.Enabled:=True;
  end
  else
  begin
    FCurrentPath := FFileList.RootPath;
    btnTopPath.Caption:=FCurrentPath;
    FLvl := length(FCurrentPath.Split([PathDelim]));
    btnRefresh.Enabled:=false;
    Progress(Self, 0, 0, 0, 'Scanning...');
    FThreadSearchFiles := ThreadedSearchFiles(FCurrentPath, '*.cbz', @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
  FThreadConv := TThreadConv.Create(FLog, FFileList, @Progress);
end;

procedure TCbzLibrary.dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  p, x, y : integer;
  pic : TBitmap;
  s : string;
  r : TRect;
  ts : TTextStyle;
begin
  try
    p := (dgLibrary.ColCount * aRow) + aCol;
    if p < FVisibleList.Count then
      with dgLibrary, Canvas do
      begin
        {$ifdef Darwin}
        if DirectoryExists(FVisibleList[p]) then
          Brush.Color := clLtGray
        else
          Brush.Color:=clBlack;

        if TFileItem(FVisibleList.Objects[p]).ReadState then
          Brush.Color := clGray;
        {$else}
        if FileExists(FVisibleList[p]) and
           (not TFileItem(FVisibleList.Objects[p]).ReadState) then
           Brush.color := clWhite
        else
        if TFileItem(FVisibleList.Objects[p]).ReadState then
          Brush.Color := clGray
        else
          Brush.color := clSilver; //clLime // clYellow
        {$endif}
          //FillRect(aRect);
          r := aRect;
          r.Inflate(-2, -2, -2, -2);
          FillRect(r);

          if Assigned(FVisibleList.Objects[p]) then
          begin
            pic := TFileItem(FVisibleList.Objects[p]).Img;
            if assigned(pic) then
            begin
              s := GetLastPath(ExcludeTrailingPathDelimiter(FVisibleList[p]));
              //showmessage('s='+s);
              X := (DefaultColWidth - pic.Width) div 2;
              Y := 3; //(DefaultRowHeight - b.Height) div 2;
              Draw(aRect.Left + X, aRect.Top + Y, pic);

              r := aRect;
              r.top := r.Bottom - (TextHeight(s) * 3);
              r.Bottom:=r.Bottom-3;
              ts.ShowPrefix:=False;
              ts.Wordbreak:=True;
              ts.SingleLine:=False;
              ts.Alignment := taCenter;
              ts.RightToLeft := FAlse;
              ts.Opaque:=False;
              ts.Layout := tlBottom; //tlCenter;
              //TextOut(r.Left, r.top, s);
              TextRect(r, 0, 0, s, ts);
            end;
          end;
          if gdFocused in aState then
            DrawFocusRect(aRect);
      end;
  except
  end;
end;

procedure TCbzLibrary.dgLibraryMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  arow, acol : integer;
begin
  if Button = mbRight then
  begin
    with dgLibrary do
    begin
      MouseToCell(x,y, aCol, aRow);
      Row := aRow;
      Col := aCol;
    end;
  end;
end;

procedure TCbzLibrary.dgLibraryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p : integer;
begin
  if Button = mbRight then
  begin
    p := (dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col;
    //mnuReadStatus.Enabled := FileExists(FVisibleList[p]);
    mnuReadStatus.Caption :=
            ifThen(TFileItem(FVisibleList.Objects[p]).ReadState,
                   'Mark as Unread', 'Mark as Read');

    PopupMenu1.PopUp(dgLibrary.ClientOrigin.x + X, dgLibrary.ClientOrigin.y + Y);
  end;
end;

procedure TCbzLibrary.mnuReadStatusClick(Sender: TObject);
var
  p, i : integer;
begin
  p := (dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col;
  if FileExists(FVisibleList[p]) then
    with TFileItem(FVisibleList.Objects[p]) do
      ReadState := not ReadState
  else
  for i := 0 to FFileList.Count - 1 do
    if FFileList[i].StartsWith(FVisibleList[p]) then
      if FileExists(FFileList[i]) then
         with TFileItem(FFileList.Objects[i]) do
           ReadState := not ReadState;

  if not cbHideRead.Checked then
  begin
    dgLibrary.InvalidateCell(dgLibrary.Col, dgLibrary.Row);
    UpdateNbItems;
  end
  else
    FillGrid(False);
  FFileList.SaveToFile(GetCacheFileName);
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
    FCurrentPath := ExcludeTrailingPathDelimiter(s);
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
  FCurrentPath := FFileList.RootPath;
  FillGrid(False);
end;

procedure TCbzLibrary.cbHideReadClick(Sender: TObject);
begin
  Fconfig.LibraryHideRead:=cbHideRead.Checked;
  if cbHideRead.Checked then
    FDisplayFilters := [dfUnread]
  else
    FDisplayFilters := [dfAll];

  if Assigned(FVisibleList) then
    FillGrid(False);
end;

procedure TCbzLibrary.cbVisibleDatesChange(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoFillGrid, 0);
end;

procedure TCbzLibrary.btnRefreshClick(Sender: TObject);
begin
  if not Assigned(FThreadSearchFiles) then
  begin
    btnRefresh.enabled := False;
    FFileList.ResetStampState;
    //FFileList.Clear;
    FLog.Log('TCbzLibrary.btnRefreshClick : Refresh started.');
    FThreadSearchFiles := ThreadedSearchFiles(FFileList.RootPath, '*.cbz', @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
end;

procedure TCbzLibrary.btnReturnClick(Sender: TObject);
begin
  if FLvl > 1 then
    TButton(FBtnList[FBtnList.Count-2]).Click
  else
  begin
    if not DirectoryExists(FConfig.LibPath) then
      with TSelectDirectoryDialog.Create(Self) do
      try
        if Execute then
          SwitchPath(Filename);
      finally
      end;
  end;
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

procedure TCbzLibrary.UpdateNbItems;
begin
  StatusBar1.Panels[2].Text := IntToStr(FFileList.Count) + ' Albums, ' +
                               IntToStr(FFileList.ReadCount) + ' Read';
end;

procedure TCbzLibrary.SwitchPath(const aLibPath: String);
begin
  Fconfig.LibPath:=aLibPath;
  FVisibleList.Clear;
  FThreadConv.Terminate;
  FThreadConv.WaitFor;
  FFileList.Clear;
  FFileList.RootPath:=aLibPath;
  FCurrentPath:=aLibPath;
  btnTopPath.Caption:=aLibPath;
  FillGrid(false);
end;

procedure TCbzLibrary.CheckModified;
begin
  if FFileList.Modified then
    FFileList.SaveToFile(GetCacheFileName);
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

procedure TCbzLibrary.edtSearchChange(Sender: TObject);
var
  p, c, r, i : integer;
  s, ltr : string;
begin
  p := -1;
  Screen.Cursor := crHourGlass;
  try
    for i:= 0 to FVisibleList.Count - 1 do
    begin
      ltr := UpperCase(edtSearch.Text);
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
  end;
end;

procedure TCbzLibrary.DefaultBtnClick(Sender: TObject);
var
  i, j : integer;
  s : string;
begin
  i := FBtnList.IndexOf(Pointer(Sender));
  j := i;
  if (i >= 0) and (i + 1 < FBtnList.Count) then
  begin
    s := TButton(FBtnList[i]).Caption;
    dec(j);
    while j >= 0 do
    begin
      s := IncludeTrailingPathDelimiter(TButton(FBtnList[j]).Caption) + s;
      dec(j);
    end;
    s := IncludeTrailingPathDelimiter(FFileList.RootPath) + s;
    FCurrentPath := s;
    while (i + 1 < FBtnList.Count) do
    begin
      TButton(FBtnList[FBtnList.Count-1]).Free;
      FBtnList.Delete(FBtnList.Count-1);
    end;
    FillGrid(False);
  end;
end;

procedure TCbzLibrary.DoFillGrid(data: int64);
begin
  FillGrid(Boolean(data));
end;

procedure TCbzLibrary.FillGrid(bAddButton : Boolean = True);
begin
  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;
  Progress(Self, 0, 0, 0, 'Loading folder...');
  btnRefresh.Enabled:=False;

  if Length(FPathPos) <= 0 then
    SetLength(FPathPos, 1);

  FPathPos[FLvl-1].x := dgLibrary.Col;
  FPathPos[FLvl-1].y := MakeLong(dgLibrary.TopRow, dgLibrary.Row);

  Flvl := length(FCurrentPath.Split([PathDelim]));

  //SizeGrid;

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

  FVisibleList.Clear;
  cbVisibleDates.Enabled:=False;
  FFillThread := TThreadFill.Create(FFileList, FVisibleList, FCurrentPath, FLvl, FDisplayFilters,@ThreadFillTerminate, @Progress);
end;

procedure TCbzLibrary.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  StatusBar1.Panels[0].Text := 'Done.';

  FFileList.SaveToFile(GetCacheFileName);
  btnRefresh.Enabled:=True;
  UpdateNbItems;
  FLog.Log('TCbzLibrary.SearchEnded : Refresh ended.');
end;

function TCbzLibrary.FoundFile(const aFileName: string; IsNew: Boolean): TTreeNode;
var
  fi : TFileItem;
  acol, arow : integer;
  s : string;
begin
  if (not FileExists(aFilename)) then
    exit;

  if (FFileList.IndexOf(aFilename) < 0) then
  begin
    fi := TFileItem.Create(FLog, aFilename);
    fi.Text := GetLastPath(aFilename);
    FFileList.AddObject(aFilename, fi);
    FLog.Log('TCbzLibrary.FoundFile: Added:' + aFilename);
  end
  else
    Exit;

  if (FCurrentPath = FFileList.RootPath) then
  begin
    aRow := (FVisibleList.Count div dgLibrary.ColCount);
    aCol := FVisibleList.Count - (aRow * dgLibrary.ColCount);

    s := ExcludeTrailingPathDelimiter(ExtractFilePath(aFilename));
    if Length(s.Split([PathDelim])) > FLvl then
      s := GetFirstPath(s, FLvl)
    else
      s := aFilename;

    with FVisibleList do
      if IndexOf(s) < 0 then
        AddObject(s, fi);
  end;
  result := nil;
end;

procedure TCbzLibrary.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
var
  ind : integer;
begin
  ind := ifthen(ProgressID = 1, 1, 0);
  StatusBar1.Panels[ind].Text := Msg;
end;

procedure TCbzLibrary.DoSizegrid(data : int64);
var
  oldtoprow,
  oldrow : integer;
begin
  try
    SizeGrid;

    if Length(FPathPos) >= Flvl then
      if (FPathPos[FLvl-1].x <> 0) or (FPathPos[FLvl-1].y <> 0) then
      begin
        dgLibrary.Col := FPathPos[FLvl-1].x;
        oldtoprow := HighWord(FPathPos[FLvl-1].y);
        oldrow := LowWord(FPathPos[FLvl-1].y);

        with dgLibrary do
          if (RowCount >= oldrow) and (RowCount > 0) then
            if (FPathPos[FLvl-1].y <> 0) and
               ((TopRow <> oldtoprow) or (Row <> oldrow) or (Col <> FPathPos[FLvl-1].x)) then
            begin
              Col := FPathPos[FLvl-1].x;
              TopRow := oldtoprow;
              Row := oldrow;
            end;
      end;
  finally
    dec(FInQueue);
  end;
end;

procedure TCbzLibrary.VisibleListChanged(Sender: TObject);
begin
  if (FVisibleList.Count > 0) and (FInQueue = 0) then
  begin
    inc(FInQueue);
    Application.QueueAsyncCall(@DoSizegrid, 0);
  end;
end;

procedure TCbzLibrary.ThreadFillTerminate(Sender: TObject);
var
  s : string;
  i, oldtoprow, oldrow : Integer;
begin
  Progress(Self, 0, 0, 0, 'Ready.');
  FFillThread := nil;
  btnRefresh.Enabled:=True;

  cbVisibleDates.Clear;
  cbVisibleDates.Items.Add('');
  for i := 0 to FVisibleList.Count - 1 do
    if Assigned(TFileItem(FVisibleList.Objects[i])) then
    begin
      s := DateToStr(TFileItem(FVisibleList.Objects[i]).DateAdded);
      if cbVisibleDates.Items.IndexOf(s) < 0 then
        cbVisibleDates.Items.Add(s);
      cbVisibleDates.Enabled:=True;
    end;

  if not TThreadFill(Sender).Cancelled then
    if (FPathPos[FLvl-1].x <> 0) or (FPathPos[FLvl-1].y <> 0) then
    begin
      dgLibrary.Col := FPathPos[FLvl-1].x;
      oldtoprow := HighWord(FPathPos[FLvl-1].y);
      oldrow := LowWord(FPathPos[FLvl-1].y);

      with dgLibrary do
        if (RowCount >= oldrow) and (RowCount > 0) then
          if (FPathPos[FLvl-1].y <> 0) and
             ((TopRow <> oldtoprow) or (Row <> oldrow) or (Col <> FPathPos[FLvl-1].x)) then
          begin
            Col := FPathPos[FLvl-1].x;
            TopRow := oldtoprow;
            Row := oldrow;
          end;
    end;
  UpdateNbItems;
end;



end.

