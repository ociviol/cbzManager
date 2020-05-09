unit uCbzLibrary;

{$mode objfpc}{$H+}

{$define Library}

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
  TLibrayAction = (laAdd, laDelete);
  TLibraryNotify = procedure(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil) of object;

  TFillSettings = Record
    FFileList : TItemList;
    FVisibleList: TThreadStringlist;
    FCurrentPath: String;
    FLvl: Integer;
    FDisplayFilters: TDisplayFilters;
    FDate : TDateTime;
  end;

  { TThreadFill }

  TThreadFill = Class(TThread)
  private
    FFillSettings : TFillSettings;
    FProgress : TProgressEvent;
    FProgressChar : Char;
    FCancelled : Boolean;
    Flog : ILog;

    procedure DoProgress;
  public
    constructor Create(aLog : ILog; aFillSettings : TFillSettings;
                       aOnTerminate : TNotifyEvent;
                       aProgress : TProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
    property Cancelled : Boolean read FCancelled;
  end;

  { TThreadScrub }

  TThreadScrub = Class(TThread)
  private
    FVal,
    FCnt,
    FsyncedIn,
    FsyncedOut : Integer;
    FFileList: TItemList;
    FProgress : TProgressEvent;
    FLog : ILog;
    FNotifier : TLibraryNotify;
    FItem : TFileItem;
    procedure DoProgress;
    procedure DoNotify;
    procedure UpdateCount;
  public
    constructor Create(aLog : ILog; aFileList : TItemList;
                       aNotify : TLibraryNotify;
                       aProgress : TProgressEvent = nil);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TcbzLibrary }

  TcbzLibrary = class(TForm)
    btnReturn: TSpeedButton;
    btnTopPath: TSpeedButton;
    btnRefresh: TButton;
    cbHideRead: TCheckBox;
    cbVisibleDates: TComboBox;
    cbSearch: TComboBox;
    dgLibrary: TDrawGrid;
    mnuCreateFolder: TMenuItem;
    mnuCut: TMenuItem;
    mnuPaste: TMenuItem;
    mnuDelete: TMenuItem;
    mnuMoveTocbzManager: TMenuItem;
    N1: TMenuItem;
    mnuReadStatus: TMenuItem;
    N2: TMenuItem;
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
    procedure cbSearchChange(Sender: TObject);
    procedure cbSearchCloseUp(Sender: TObject);
    procedure cbVisibleDatesChange(Sender: TObject);
    procedure dgLibraryDblClick(Sender: TObject);
    procedure dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgLibraryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgLibraryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure mnuCreateFolderClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuMoveTocbzManagerClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuReadStatusClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    function GetCacheFileName: String;
    procedure SetCurrentPath(AValue: String);
  private
    Flog : ILog;
    FFileList : TItemList;
    FVisibleList : TThreadStringList;
    FThreadSearchFiles : TThread;
    //FBtnList : TList;
    FCurrentPath : String;
    FLvl : Integer;
    Fconfig : TConfig;
    FPathPos : array of TPoint;
    FFillThread : TThreadFill;
    FDisplayFilters : TDisplayFilters;
    FInQueue : Integer;
    FThreadScrub : TThreadScrub;
    FFileToCopy : TFileItem;

    procedure UpdateNbItems;
    procedure SetGridPos(aCol, aRow : Integer); inline;
    procedure SetGridTopPos(aCol, aRow : Integer); inline;
    procedure MoveIntoFolder;
    procedure SwitchPath(const aLibPath : String);
    procedure CheckModified;
    procedure btnletterclick(sender : Tobject);
    procedure AfterShow(data : int64);
    procedure DoSizegrid(data : int64);
    procedure SizeGrid;
    procedure DefaultBtnClick(Sender: TObject);
    procedure DoFillGrid(data : int64);
    procedure FillGrid;
    procedure SearchEnded(Sender: TObject);
    procedure UpdateVisibleDates;
    function FoundFile(const aFileName: string;
                            IsNew: Boolean = False): TTreeNode;
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    property CacheFileName : String read GetCacheFileName;
    procedure VisibleListChanged(Sender : TObject);
    procedure ThreadFillTerminate(Sender : TObject);
    procedure ThreadScrubNotify(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil);
    function SelectedStr : String;
    function SelectedObj : TFileItem;

    property CurrentPath : String read FCurrentPath write SetCurrentPath;
  public
    constructor Create(aOwner : TComponent; aConfig : TConfig); reintroduce;
    //property RootPath : String read FRootPath write FRootPath;
    function ImportFile(const aFilename : String):Boolean;
  end;


implementation

uses
  Math, StrUtils, DateUtils, utils.files, ucbz, uCbzViewer;


{$R *.lfm}


{ TThreadScrub }

constructor TThreadScrub.Create(aLog : ILog; aFileList: TItemList;
                               aNotify : TLibraryNotify;
                               aProgress : TProgressEvent = nil);
begin
  Flog := aLog;
  FItem := nil;
  FFileList := aFileList;
  FNotifier:=aNotify;
  FreeOnTerminate:=False;
  FPRogress := aProgress;
  inherited Create(False);
  Priority:=tpLower;
end;

destructor TThreadScrub.Destroy;
begin
  FLog := nil;
  inherited Destroy;
end;

procedure TThreadScrub.DoProgress;
var
  cnt, del : integer;
begin
  if Assigned(FProgress) then
  begin
    cnt := FFileList.Count;
    del := FFileList.DeletedCount;
//    dec(cnt, del);
    if (FVal < FCnt - 1) and (Fcnt > 0) then
      FProgress(Self, 1, 0, 0, Format('Scrub:%s - Albums:%d - Stamps:%d - Deleted:%d - Synced:In:%d,Out:%d',
                                     [IntToStr((FVal * 100) div cnt) + '%',
                                      cnt, FFileList.StampCount, del,
                                      FSyncedIn, FSyncedOut]))
    //FProgress(Self, 1, 0, 0, 'Scrubing stamps : ' +
      //          IntToStr((FVal * 100) div FFileList.Count) + '%')
    else
      FProgress(Self, 1, 0, 0, 'Scrub Done. (' + TimeToStr(now) + ')');
  end;
end;

procedure TThreadScrub.DoNotify;
begin
  FNotifier(Self, laDelete, FItem);
end;

procedure TThreadScrub.UpdateCount;
begin
  FCnt := FFileList.Count;
end;

procedure TThreadScrub.Execute;
var
  r : integer;
  b : TBitmap;

  procedure _DeleteItem;
  begin
    with FItem do
      if not FileExists(Filename) then
      begin
        FItem.Deleted:=True;
        if Assigned(FNotifier) then
          Synchronize(@DoNotify);
        Fitem.SyncFileDelete;
        //FFileList.Delete(FVal);
        FLog.Log('TThreadScrub.Execute: Deleted:' + Filename);
      end;
  end;

begin
  Sleep(1000);
  while not Terminated do
  try
    //if (FFileList.StampCount <> FFileList.Count) or
    //   (FFileList.DeletedCount > 0) then
    begin
      FLog.Log('TThreadScrub.Execute: Starting scrub.');
      Synchronize(@UpdateCount);
      FVal := 0;
      FsyncedIn := 0;
      FsyncedOut := 0;

      while (FCnt > FVal) do
      begin
        if Terminated then
            Exit;

        // remove invalid entries
        FItem := TFileItem(FFileList.Objects[FVal]);
        if not FItem.Deleted then
        begin
          if not FileExists(FItem.Filename) then
            _DeleteItem
            {$ifdef Library}
          else
          // sync needed
          if FFileList.SyncPath.Length > 0 then
          begin
            r := TFileItem(FFileList.Objects[FVal]).CheckSync;
            if r < 0 then
              _DeleteItem
            else
            if r = 1 then
              inc(FSyncedIn)
            else
            if r = 1 then
              inc(FSyncedOut);
          end
          {$endif};

          if Terminated then
            Exit;

          // make stamp if needed
          with TFileItem(FFileList.Objects[FVal]) do
          begin
            b := GenerateStamp;
            if Assigned(b) then
              b.Free;
          end;
        end;

        inc(FVal);

        if (FVal mod 50) = 0 then
          Synchronize(@DoProgress);
          //yield;

        Sleep(50);
        if Terminated then
            Exit;

        Synchronize(@UpdateCount);
      end;
      Synchronize(@DoProgress);
      FLog.Log('TThreadScrub.Execute: Scrub done.');
      Sleep(15000);
    end;
   // else
   //   Sleep(2000);
  except
    on e: Exception do
      FLog.Log('TThreadScrub.Execute Error: ' + E.Message);
  end;
end;

{ TThreadFill }

constructor TThreadFill.Create(aLog : ILog; aFillSettings : TFillSettings;
                               aOnTerminate: TNotifyEvent; aProgress: TProgressEvent);
begin
  Flog := aLog;
  FFillSettings := aFillSettings;
  FreeOnTerminate:=True;
  OnTerminate:=aOnTerminate;
  FProgress := aProgress;
  FProgressChar := '|';
  FCancelled:=FAlse;

  inherited Create(False);
end;

destructor TThreadFill.Destroy;
begin
  Flog := nil;
  inherited Destroy;
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
  fi : TFileItem;
begin
  while not Terminated do
  try
    with FFillSettings do
      for i := 0 to FFileList.Count - 1 do
      begin
        if Terminated then
        begin
          FCancelled:=True;
          Exit;
        end;

        if FFileList[i].StartsWith(IncludeTrailingPathDelimiter(FCurrentPath)) then
        begin
          s := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileList[i]));
          if Length(s.Split([PathDelim])) > FLvl then
            s := GetFirstPath(s, FLvl)
          else
            s := FFileList[i];

          fi := TFileItem(FFileList.Objects[i]);
          if (dfUnread in FDisplayFilters) then
            if fi.ReadState then
              continue;

          if not fi.Deleted then
          begin
            if FDate > 0 then
              if  Daysbetween(Fdate, fi.DateAdded) <> 0 then
                continue;

            with FVisibleList do
              if FileExists(s) then
                AddObject(s, fi)
              else
              if IndexOf(s) < 0 then
                AddObject(s, fi);
          end;

          if (i mod 250) = 0 then
          begin
            Synchronize(@DoProgress);
            Sleep(10);
          end;
        end;
      end;

    Terminate;
  except
    on e: Exception do
    begin
      Flog.Log('TThreadFill.Execute: Error:' + E.Message);
      Terminate;
    end;
  end;
end;


{ TcbzLibrary }

constructor TcbzLibrary.Create(aOwner: TComponent; aConfig: TConfig);
begin
  FConfig := aConfig;
  FFillThread := nil;
  FThreadScrub:=nil;
  FFileToCopy := nil;
  inherited Create(aOwner);
end;

function TcbzLibrary.ImportFile(const aFilename: String):Boolean;
var
  dest : string;
begin
  if CurrentPath <> '' then
  try
    dest := IncludeTrailingPathDelimiter(CurrentPath) + ExtractFileName(aFilename);
    if FileExists(dest) then
      if MessageDlg('Conflict', 'File already exists, overwrite ?', mtInformation, mbYesNo, 0) = mrno then
        exit(false);


    CopyFile(aFilename, dest);
    FoundFile(dest);
    FillGrid;
    result := true;
  except
    result := false;
  end;
end;

procedure TcbzLibrary.FormCreate(Sender: TObject);
var
  c : char;
  s : string;
begin
  // start logger
  FLog := GetILog(
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/' +
{$else}
    IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Logs\' +
{$endif}
    'cbzLibrary.log', Fconfig.DoLog);

  StatusBar1.Font.Size := 9;
  {$if defined(Linux)}
  StatusBar1.Panels[0].Width:=200;
  StatusBar1.Panels[1].Width:=550;
  {$endif}
  Flog.Log('cbzLibrary started.');
  FInQueue := 0;
  FDisplayFilters := [dfAll];
  cbHideRead.Checked := Fconfig.LibraryHideRead;
  FFileList := TItemList.Create(Flog);
  FFileList.RootPath:=Fconfig.LibPath;
  FFileList.SyncPath:=Fconfig.SyncPath;
  //FBtnList := TList.Create;
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

procedure TcbzLibrary.FormDestroy(Sender: TObject);
begin
  if Assigned(FThreadSearchFiles) then
    begin
      FThreadSearchFiles.Terminate;
      FThreadSearchFiles.WaitFor;
    end;

  FVisibleList.Free;
  FFileList.Free;
  //FBtnList.Free;
  Flog.Log('cbzLibrary destroyed.');
  Flog := nil;
end;

procedure TcbzLibrary.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FConfig.LibCurPath:=CurrentPath;
  FConfig.SaveForm(Self);
  CheckModified;

  FThreadScrub.Terminate;
  FThreadScrub.Waitfor;
  FThreadScrub.Free;

  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;

  CloseAction := caFree;
end;

procedure TcbzLibrary.FormResize(Sender: TObject);
begin
  SizeGrid;
end;

procedure TcbzLibrary.FormShow(Sender: TObject);
begin
  FConfig.RestoreForm(Self);
  dgLibrary.ColCount:=1;
  dgLibrary.RowCount:=1;
  Application.QueueAsyncCall(@AfterShow, 0);
end;

procedure TcbzLibrary.AfterShow(data : int64);
begin
  if FileExists(CacheFileName) then
  try
    FFileList.LoadFromFile(CacheFileName);
    CurrentPAth := FFileList.RootPath;
    //cbzLibrary.Caption:=CurrentPath;
    UpdateVisibleDates;
    if Fconfig.LibCurPath <> '' then
    begin
      CurrentPath:=Fconfig.LibCurPath;
      Flvl := 1;
      FillGrid;
    end
    else
      btnTopPath.Click;
  finally
    btnRefresh.Enabled:=True;
  end
  else
  begin
    CurrentPath := FFileList.RootPath;
    FLvl := length(CurrentPath.Split([PathDelim]));
    btnRefresh.Enabled:=false;
    Progress(Self, 0, 0, 0, 'Scanning...');
    FThreadSearchFiles := ThreadedSearchFiles(CurrentPath, ['*.cbz'], @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
  FThreadScrub := TThreadScrub.Create(FLog, FFileList, @ThreadScrubNotify, @Progress);
end;

procedure TcbzLibrary.dgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
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
        if gdFocused in aState then
          Brush.Color := clActiveCaption
        else
        if not FileExists(TFileItem(FVisibleList.Objects[p]).Filename) then
        begin
          Brush.Color := clGray;
          FLog.Log('TCbzLibrary.dgLibraryDrawCell:File not found:'+TFileItem(FVisibleList.Objects[p]).Filename);
        end
        else
        begin
          Font.Color := clWhite;
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
        begin
           Brush.color := clWhite;
           Font.Color := clBlack;
        end
        else
        if TFileItem(FVisibleList.Objects[p]).ReadState then
          Brush.Color := clGray
        else
          Brush.color := clSilver; //clLime // clYellow
        {$endif}
        end;

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
            inc(r.Left, 3);
            dec(r.Right, 3);
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

procedure TcbzLibrary.SetGridTopPos(aCol, aRow : Integer);
begin
  with dgLibrary do
  begin
    BeginUpdate;
    try
      TopRow := aRow;
      Col := aCol;
      if length(FPathPos) >= FLvl then
      begin
        FPathPos[FLvl-1].x := 0;
        FPathPos[FLvl-1].y := 0;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcbzLibrary.SetGridPos(aCol, aRow : Integer);
begin
  with dgLibrary do
  begin
    BeginUpdate;
    try
      Row := aRow;
      Col := aCol;
      if length(FPathPos) >= FLvl then
      begin
        FPathPos[FLvl-1].x := 0;
        FPathPos[FLvl-1].y := 0;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcbzLibrary.dgLibraryMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  arow, acol : integer;
begin
  if Button = mbRight then
  begin
    with dgLibrary do
    begin
      MouseToCell(x,y, aCol, aRow);
      SetGridPos(aCol, aRow);
    end;
  end;
end;

procedure TcbzLibrary.dgLibraryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    mnuReadStatus.Caption :=
            ifThen(SelectedObj.ReadState,
                   'Mark as Unread', 'Mark as Read');

    PopupMenu1.PopUp(dgLibrary.ClientOrigin.x + X, dgLibrary.ClientOrigin.y + Y);
  end;
end;

procedure TcbzLibrary.mnuReadStatusClick(Sender: TObject);
var
  i : integer;
begin
  if FileExists(SelectedStr) then
    with TFileItem(SelectedObj) do
      ReadState := not ReadState
  else
  for i := 0 to FFileList.Count - 1 do
    if FFileList[i].StartsWith(SelectedStr) then
      if FileExists(FFileList[i]) then
         with TFileItem(FFileList.Objects[i]) do
           ReadState := not ReadState;

  if not cbHideRead.Checked then
  begin
    dgLibrary.InvalidateCell(dgLibrary.Col, dgLibrary.Row);
    UpdateNbItems;
  end
  else
    FillGrid;
  FFileList.SaveToFile(GetCacheFileName);
end;

procedure TcbzLibrary.PopupMenu1Popup(Sender: TObject);
begin
  mnuDelete.Enabled:= not Assigned(FThreadSearchFiles);
  mnuCut.Enabled := not FileExists(SelectedStr);
  mnuPaste.Enabled:= Assigned(FFileToCopy);
end;

procedure TcbzLibrary.dgLibraryDblClick(Sender: TObject);
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
    CurrentPath := ExcludeTrailingPathDelimiter(s);
    FillGrid;
  end
  else
  if FileExists(s) then
    ShowComics(FLog, s, FConfig)
  else
  ;
end;

procedure TcbzLibrary.cbHideReadClick(Sender: TObject);
begin
  Fconfig.LibraryHideRead:=cbHideRead.Checked;
  if cbHideRead.Checked then
    FDisplayFilters := [dfUnread]
  else
    FDisplayFilters := [dfAll];

  if Assigned(FVisibleList) then
    FillGrid;
end;

procedure TcbzLibrary.cbSearchChange(Sender: TObject);
var
  p, c, r, i : integer;
  s, ltr : string;
begin
  if cbSearch.Text = '' then
    exit;

  p := -1;
  cbSearch.DroppedDown:=True;
  Screen.Cursor := crHourGlass;
  try
    for i:= 0 to FVisibleList.Count - 1 do
    begin
      ltr := UpperCase(cbSearch.Text);
      s := GetLastPath(ExcludeTrailingPathDelimiter(FVisibleList[i])).ToUpper;
      if s.StartsWith(ltr) then
        with dgLibrary do
        begin
          p := i;
          r := p div ColCount;
          c := p - (r * ColCount);
          SetGridTopPos(c, r);
          //TopRow := r;
          //col := c;
          //MoveIntoFolder(i);
          break;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TcbzLibrary.MoveIntoFolder;
var
  i : integer;
  s : string;
begin
  for i := 0 to FVisibleList.Count - 1 do
    if FVisibleList[i].Contains(cbSearch.Items[cbSearch.ItemIndex]) then
    begin
      s := FVisibleList[i];
      if DirectoryExists(s) then
      begin
        CurrentPath := ExcludeTrailingPathDelimiter(s);
        FillGrid;
      end;
      break;
    end;
end;

procedure TcbzLibrary.cbSearchCloseUp(Sender: TObject);
begin
  with cbSearch do
    if ItemIndex >= 0 then
      MoveIntoFolder;
end;

procedure TcbzLibrary.cbVisibleDatesChange(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoFillGrid, 0);
end;

procedure TcbzLibrary.btnRefreshClick(Sender: TObject);
begin
  if not Assigned(FThreadSearchFiles) then
  begin
    btnRefresh.enabled := False;
    FFileList.ResetStampState;
    //FFileList.Clear;
    FLog.Log('TCbzLibrary.btnRefreshClick : Refresh started.');
    FThreadSearchFiles := ThreadedSearchFiles(FFileList.RootPath, ['*.cbz'], @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
end;

procedure TcbzLibrary.btnTopPathClick(Sender: TObject);
begin
  CurrentPath := FFileList.RootPath;
  Flvl := 1;
  FillGrid
end;

procedure TcbzLibrary.btnReturnClick(Sender: TObject);
begin
  if CurrentPath = FFileList.RootPath then
    Exit;

  CurrentPath := ExcludeTrailingPathDelimiter(ExtractFilePath(CurrentPath));
  FillGrid;
end;

procedure TcbzLibrary.mnuCreateFolderClick(Sender: TObject);
begin

end;

procedure TcbzLibrary.mnuDeleteClick(Sender: TObject);
begin
  if not FileExists(SelectedStr) then
    exit;

  if DeleteFile(SelectedStr) then
  begin
    TFileItem(SelectedObj).Deleted:=True;
    FillGrid;
  end;
end;

procedure TcbzLibrary.mnuMoveTocbzManagerClick(Sender: TObject);
var
  dest, destp : string;
  Files : TSTringlist;
  s : string;
begin
  destp := IncludeTrailingPathDelimiter(Fconfig.BdPathPath) + 'Library' + PathDelim;
  ForceDirectories(destp);

  if FileExists(SelectedStr) then
  begin
    dest := destp + ExtractFileName(SelectedStr);
    CopyFile(SelectedStr, dest);
  end
  else
  if DirectoryExists(SelectedStr) then
  begin
     Files := TSTringlist.Create;
     try
       GetFiles(SelectedStr, ['*'], Files);
       for s in files do
       begin
         dest := destp + s.Replace(FFileList.RootPath, '');
         CopyFile(s, dest);
       end;
     finally
       Free;
     end;
  end;
end;


procedure TcbzLibrary.mnuCutClick(Sender: TObject);
begin
  if FileExists(SelectedStr) then
    FFileToCopy := SelectedObj;
end;

procedure TcbzLibrary.mnuPasteClick(Sender: TObject);
var
  dest : string;
begin
  dest := IncludeTrailingPathDelimiter(SelectedStr) + ExtractFileName(FFileToCopy.Filename);
  if DirectoryExists(SelectedStr) then
    if RenameFile(FFileToCopy.Filename, dest) then
      FFileToCopy.Filename := dest;

  FFileToCopy := nil;
end;

function TcbzLibrary.GetCacheFileName: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
  expandfilename('~/') + CS_CONFIG_PATH + '/Library/' +
{$else}
  IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Library\' +
{$endif}
  'cbzLibrary.xml';
  ForceDirectories(ExtractFilePath(result));
end;

procedure TcbzLibrary.SetCurrentPath(AValue: String);
begin
  if FCurrentPath=AValue then Exit;
  FCurrentPath:=AValue;
  Caption := 'Library (' + FCurrentPath + ')';
end;

procedure TcbzLibrary.UpdateNbItems;
begin
  StatusBar1.Panels[0].Text := IntToStr(FFileList.Count - FFileList.DeletedCount) + ' Albums, ' +
                               IntToStr(FFileList.ReadCount) + ' Read';
end;

procedure TcbzLibrary.SwitchPath(const aLibPath: String);
begin
  Fconfig.LibPath:=aLibPath;
  FVisibleList.Clear;
  FThreadScrub.Terminate;
  FThreadScrub.WaitFor;
  FFileList.Clear;
  FFileList.RootPath:=aLibPath;
  CurrentPath:=aLibPath;
  FillGrid;
end;

procedure TcbzLibrary.CheckModified;
begin
  if FFileList.Modified then
    FFileList.SaveToFile(GetCacheFileName);
end;

procedure TcbzLibrary.btnletterclick(sender: Tobject);
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
          SetGridTopPos(c, r);
          //TopRow := r;
          //col := c;
          break;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TcbzLibrary.SizeGrid;
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

procedure TcbzLibrary.DefaultBtnClick(Sender: TObject);
//var
//  i, j : integer;
//  s : string;
begin
  //i := FBtnList.IndexOf(Pointer(Sender));
  //j := i;
  //if (i >= 0) and (i + 1 < FBtnList.Count) then
  //begin
  //  s := TButton(FBtnList[i]).Caption;
  //  dec(j);
  //  while j >= 0 do
  //  begin
  //    s := IncludeTrailingPathDelimiter(TButton(FBtnList[j]).Caption) + s;
  //    dec(j);
  //  end;
  //  s := IncludeTrailingPathDelimiter(FFileList.RootPath) + s;
  //  CurrentPath := s;
  //  while (i + 1 < FBtnList.Count) do
  //  begin
  //    TButton(FBtnList[FBtnList.Count-1]).Free;
  //    FBtnList.Delete(FBtnList.Count-1);
  //  end;
  //  FillGrid(False);
  //end;
end;

procedure TcbzLibrary.DoFillGrid(data: int64);
begin
  FillGrid;
end;

procedure TcbzLibrary.FillGrid;
var
  fs : TFillSettings;
begin
  cbSearch.Enabled:=False;
  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;
  Progress(Self, 0, 0, 0, 'Loading folder...');
  btnRefresh.Enabled:=False;

  if Length(FPathPos) < FLvl then
    SetLength(FPathPos, FLvl);

  FPathPos[FLvl-1].x := dgLibrary.Col;
  FPathPos[FLvl-1].y := MakeLong(dgLibrary.TopRow, dgLibrary.Row);

  Flvl := length(CurrentPath.Split([PathDelim]));

  //SizeGrid;

  if Flvl > Length(FPathPos) then
    SetLength(FPathPos, Flvl);

  FVisibleList.Clear;

  fs.FFileList := FFileList;
  fs.FVisibleList:= FVisibleList;
  fs.FCurrentPath:= CurrentPath;
  fs.FLvl:=FLvl;
  fs.FDisplayFilters:=FDisplayFilters;
  fs.Fdate := 0;
  with cbVisibleDates do
    if ItemIndex > 0 then
      fs.FDate:= TDateTime(Items.Objects[ItemIndex]);

  FFillThread := TThreadFill.Create(Flog, fs,@ThreadFillTerminate, @Progress);
end;

procedure TcbzLibrary.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  StatusBar1.Panels[2].Text := 'Ready.';

  FFileList.SaveToFile(GetCacheFileName);
  btnRefresh.Enabled:=True;
  UpdateNbItems;
  UpdateVisibleDates;
  FLog.Log('TCbzLibrary.SearchEnded : Refresh ended.');
end;

procedure TcbzLibrary.UpdateVisibleDates;
var
  i : integer;
  s : string;
begin
  with cbVisibleDates do
  begin
    Items.BeginUpdate;
    try
      Clear;
      Items.Add('');
      with FFileList do
        for i := 0 to Count - 1 do
          if Assigned(TFileItem(Objects[i])) then
          with TFileItem(Objects[i]) do
          begin
            s := FormatDateTime('yyyy/mm/dd', DateAdded);
            if Items.IndexOf(s) < 0 then
              Items.AddObject(s, Tobject(DateAdded));
            Enabled:=True;
          end;
    finally
      Items.EndUpdate;
    end;
    Enabled := True;
  end;
end;

function TcbzLibrary.FoundFile(const aFileName: string; IsNew: Boolean): TTreeNode;
var
  fi : TFileItem;
  acol, arow : integer;
  s : string;
begin
  if (not FileExists(aFilename)) then
    exit;

  if (FFileList.IndexOf(aFilename) < 0) then
  begin
    fi := TFileItem.Create(FFileList, FLog, aFilename);
    fi.Text := GetLastPath(aFilename);
    FFileList.AddObject(aFilename, fi);
    FLog.Log('TCbzLibrary.FoundFile: Added:' + aFilename);
  end
  else
  with FFileList do
    if TFileItem(Objects[IndexOf(aFilename)]).Deleted then
    begin
      TFileItem(Objects[IndexOf(aFilename)]).Deleted := false;
      Exit;
    end;

  if (CurrentPath = FFileList.RootPath) then
  begin
    aRow := (FVisibleList.Count div dgLibrary.ColCount);
    aCol := FVisibleList.Count - (aRow * dgLibrary.ColCount);
    SetGridTopPos(aCol, aRow);

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

procedure TcbzLibrary.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
var
  ind : integer;
begin
  ind := ifthen(ProgressID = 1, 1, 2);
  StatusBar1.Panels[ind].Text := Msg;
end;

procedure TcbzLibrary.DoSizegrid(data : int64);
var
  oldtoprow,
  oldrow : integer;
begin
  try
    SizeGrid;

    // auto relect last selected item
    if Length(FPathPos) >= Flvl then
      if (FPathPos[FLvl-1].x <> 0) or (FPathPos[FLvl-1].y <> 0) then
      begin
        dgLibrary.Col := FPathPos[FLvl-1].x;
        oldtoprow := HighWord(FPathPos[FLvl-1].y);
        oldrow := LowWord(FPathPos[FLvl-1].y);

        with dgLibrary do
          if (RowCount >= oldrow) and (RowCount > 0) then
            if (FPathPos[FLvl-1].y <> 0) then
              if (TopRow <> oldtoprow) then
                TopRow:=oldtoprow
              else
              if (Row <> oldrow) then
                Row := oldrow
              else
              if (Col <> FPathPos[FLvl-1].x) then
                Col := FPathPos[FLvl-1].x;
      end;
  finally
    dec(FInQueue);
  end;
end;

procedure TcbzLibrary.VisibleListChanged(Sender: TObject);
begin
  if (FVisibleList.Count > 0) and (FInQueue = 0) then
  begin
    inc(FInQueue);
    Application.QueueAsyncCall(@DoSizegrid, 0);
  end;
end;

procedure TcbzLibrary.ThreadFillTerminate(Sender: TObject);
var
  oldtoprow, oldrow, i : Integer;
  s : string;
begin
  Progress(Self, 0, 0, 0, 'Ready.');
  FFillThread := nil;
  btnRefresh.Enabled:=True;
  FVisibleList.Sort;
  with cbSearch do
  begin
    Items.BeginUpdate;
    try
      Clear;

      for i:=0 to FVisibleList.Count - 1 do
      begin
        s := FVisibleList[i];
        if not s.ToLower.EndsWith('.cbz') then
          Items.Add(ExcludeLeadingPathDelimiter(ExcludeTrailingPathDelimiter(s.Replace(CurrentPath, ''))));
      end;
    finally
      Items.EndUpdate;
      cbSearch.Enabled:=True;
    end;
  end;
  if not TThreadFill(Sender).Cancelled then
    if (FPathPos[FLvl-1].x <> 0) or (FPathPos[FLvl-1].y <> 0) then
    begin
      dgLibrary.Col := FPathPos[FLvl-1].x;
      oldtoprow := HighWord(FPathPos[FLvl-1].y);
      oldrow := LowWord(FPathPos[FLvl-1].y);

      with dgLibrary do
      begin
        BeginUpdate;
        try
          if (RowCount >= oldrow) and (RowCount > 0) then
            if (FPathPos[FLvl-1].y <> 0) and
               ((TopRow <> oldtoprow) or (Row <> oldrow) or (Col <> FPathPos[FLvl-1].x)) then
            begin
              SetGridPos(FPathPos[FLvl-1].x, oldrow);
              SetGridTopPos(FPathPos[FLvl-1].x, oldtoprow);
              FPathPos[FLvl-1].x := 0;
              FPathPos[FLvl-1].y := 0;
            end;
        finally
          EndUpdate;
        end;
      end;
    end;
  UpdateNbItems;
end;

procedure TcbzLibrary.ThreadScrubNotify(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil);
begin
  if aAction = laDelete then
    if FVisibleList.HasObject(aFileItem) then
      Application.QueueAsyncCall(@DoFillGrid, 0);
end;

function TcbzLibrary.SelectedStr: String;
var
    p : integer;
begin
  p := (dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col;
  result := FVisibleList[p];
end;

function TcbzLibrary.SelectedObj: TFileItem;
var
  p : integer;
begin
  p := (dgLibrary.ColCount * dgLibrary.row) + dgLibrary.col;
  result := TFileItem(FVisibleList.Objects[p]);
end;


end.

