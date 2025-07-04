unit uCbzLibrary;

{$mode objfpc}{$H+}

{$define Library}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Menus, ActnList, Types,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  Utils.SearchFiles, utils.Logger, uConfig, Utils.Strings,
  uLibraryClasses, ucbz, uThreadScrub, uThreadFill;


type
  { TcbzLibrary }

  TcbzLibrary = class(TForm)
    ActionMarkAsUnread: TAction;
    ActionRename: TAction;
    ActionPaste: TAction;
    ActionCut: TAction;
    ActionCreateFolder: TAction;
    ActionDelete: TAction;
    ActionCopyToMngr: TAction;
    ActionMarkasRead: TAction;
    ActionList1: TActionList;
    btnScrub: TButton;
    btnReturn: TSpeedButton;
    btnTopPath: TSpeedButton;
    btnRefresh: TButton;
    btnTest: TButton;
    cbHideRead: TCheckBox;
    cbVisibleDates: TComboBox;
    cbSearch: TComboBox;
    cbReadingList: TComboBox;
    dgLibrary: TDrawGrid;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuStamps: TMenuItem;
    mnuMarkasUnread: TMenuItem;
    OpenDialog1: TOpenDialog;
    mnuConfig: TMenuItem;
    mnuCreateFolder: TMenuItem;
    mnuCut: TMenuItem;
    mnuPaste: TMenuItem;
    mnuDelete: TMenuItem;
    mnuMoveTocbzManager: TMenuItem;
    N1: TMenuItem;
    mnuMarkasRead: TMenuItem;
    N2: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlbtns: TPanel;
    pnlPath: TPanel;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionMarkAsUnreadExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionMarkasReadExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnScrubClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnTopPathClick(Sender: TObject);
    //procedure Button1Click(Sender: TObject);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure mnuStampsClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    Flog : ILog;
    FFileList : TItemList;
    FVisibleList : TThreadStringList;
    FThreadSearchFiles : TThread;
    //FBtnList : TList;
    FCurrentPath : String;
    FLvl : Integer;
    Fconfig : TConfig;
    FReadingList : TStringlist;
    FPathPos : array of TPoint;
    FFillThread : TThreadFill;
    FDisplayFilters : TDisplayFilters;
    FInQueue : Integer;
    FThreadScrub : TThreadScrub;
    FFileToCopy : TFileItem;
    FOwnConfig : Boolean;
    FConfigFile : string;
    procedure CheckVersionTerminate(Sender : TObject);
    procedure SetCaption;
    procedure StopThreads;
    procedure EnableActions;
    function GetCacheFileName: String;
    procedure SetCurrentPath(AValue: String);
    procedure UpdateNbItems;
    procedure SetGridPos(aCol, aRow : Integer);
    procedure SetGridTopPos(aCol, aRow : Integer);
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
    function CheckPaths:Boolean;
    procedure SearchEnded(Sender: TObject);
    procedure UpdateVisibleDates;
    function FoundFile(const aFileName: string;
                            IsNew: Boolean = False): TTreeNode;
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    property CacheFileName : String read GetCacheFileName;
    procedure VisibleListChanged(Sender : TObject);
    procedure ThreadFillTerminate(Sender : TObject);
    procedure ThreadScrubTerminate(Sender : TObject);
    procedure ThreadScrubNotify(Sender : TObject; aAction : TLibrayAction; aFileItem : TFileItem = nil);
    function SelectedStr : String;
    function SelectedObj : TFileItem;
    procedure ChangeReadState(aReadState : boolean);
    property CurrentPath : String read FCurrentPath write SetCurrentPath;
  public
    constructor Create(aOwner : TComponent); override;
    constructor Create(aOwner : TComponent; aConfig : TConfig); reintroduce;
    destructor Destroy; override;
    //property RootPath : String read FRootPath write FRootPath;
    function ImportFile(const aFilename : String; const RelativePath : String = '';
                        aCbz : TCbz = nil):Boolean;
  end;


implementation

uses
  LazUTF8,
  lclintf,
  uVersionThread,
  Math, StrUtils, DateUtils,
  utils.files, uCbzViewer,
  Utils.Vcl, Config, frmWait,
  Utils.SoftwareVersion;


{$R *.lfm}

{$if defined(Darwin) or defined(Linux)}
const
  CS_CONFIG_LIB_JSON = '/configlib.json';
{$endif}


function GetLevel(const Path : String):Integer; //inline;
begin
  result := length(Path.Split([PathDelim]));
end;


{ TcbzLibrary }

constructor TcbzLibrary.Create(aOwner: TComponent);
begin
  FConfig := nil;
  FOwnConfig := True;
  FFillThread := nil;
  FThreadScrub:=nil;
  FThreadSearchFiles:=nil;
  FFileToCopy := nil;
  FReadingList := TStringList.Create;
  inherited Create(aOwner);

  TThreadCheckVersion.Create(pvCbzLibrary, FLog, @CheckVersionTerminate);
end;

procedure TcbzLibrary.CheckVersionTerminate(Sender : TObject);
begin
  if TThreadCheckVersion(Sender).NeedUpdate then
    if MessageDlg('A new version is available, do you want to download the update ?',
                  mtConfirmation, MbYesNo, 0) = MrYes then
    begin
{$if defined(Darwin)}
      OpenUrl('https://github.com/ociviol/cbzManager/tree/master/precompiled%20binairies/Mac%20OsX');
{$elseif defined(Linux)}
      OpenUrl('https://github.com/ociviol/cbzManager/tree/master/precompiled%20binairies/Linux');
{$else}
      OpenUrl('https://github.com/ociviol/cbzManager/tree/master/precompiled%20binairies/Windows');
{$endif}
    end;
end;

constructor TcbzLibrary.Create(aOwner: TComponent; aConfig: TConfig);
begin
  inherited Create(aOwner);
  FOwnConfig := False;
  FConfig := aConfig;
end;

destructor TcbzLibrary.Destroy;
begin
  FReadingList.Free;
  inherited Destroy;
end;

function TcbzLibrary.ImportFile(const aFilename: String; const RelativePath : String = '';
                                aCbz : TCbz = nil):Boolean;
var
  dest : string;
begin
  SCreen.Cursor := crHourglass;
  try
    if CurrentPath <> '' then
    try
      dest := IncludeTrailingPathDelimiter(CurrentPath) +
              ifthen(RelativePath.IsEmpty, '', IncludeTrailingPathDelimiter(RelativePath));

      if not ForceDirectories(dest) then
        raise Exception.Create('Unable to create folder : ' + dest);

      dest := dest + ExtractFileName(aFilename);
      if FileExists(dest) then
        if MessageDlg('Conflict', 'File already exists, overwrite ?', mtInformation, mbYesNo, 0) = mrno then
          exit(false);

      if assigned(aCbz) then
        aCbz.SaveToFile(dest)
      else
        CopyFile(aFilename, dest, Flog);

      if FFileList.IndexOf(dest) >= 0 then
        TFileItem(FFileList.Objects[FFileList.IndexOf(dest)]).Deleted := True;

      FoundFile(dest);
      FillGrid;
      result := true;
    except
      result := false;
  end;
  finally
    SCreen.Cursor := crDefault;
  end;
end;

procedure TcbzLibrary.FormCreate(Sender: TObject);
var
  c : char;
  s : string;
begin
  if FOwnConfig and Not Assigned(FConfig) then
  begin
  {$if defined(Darwin) or defined(Linux)}
    FConfigFile := expandfilename('~/') + CS_CONFIG_PATH;
    ForceDirectories(FConfigFile);
    FConfigFile := FConfigFile + CS_CONFIG_LIB_JSON;
  {$else}
    FConfigFile := ConfigPath + 'configlib.json';
  {$endif}
    // load config
    FConfig := TConfig.Load(FConfigFile);
    FConfig.RestoreForm(Self);
  end;

  // start logger
  FLog := GetILog(
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/' +
{$else}
    ConfigPath + 'Logs\' +
{$endif}
    'cbzLibrary.log', Fconfig.DoLog);

  StatusBar1.Font.Size := 9;
  {$if defined(Linux)}
  StatusBar1.Panels[0].Width:=200;
  StatusBar1.Panels[1].Width:=550;
  {$endif}
  {$ifndef Debug}
  btnTest.Visible := False;
  {$endif}
  Flog.Log('cbzLibrary started.');
  FInQueue := 0;
  FDisplayFilters := [dfAll];
  cbHideRead.Checked := Fconfig.HideRead;
  FFileList := TItemList.Create(Flog);
  FFileList.RootPath:=Fconfig.LibPath;
  FFileList.SyncPath:=Fconfig.SyncPath;
  //FBtnList := TList.Create;
  FVisibleList := TThreadStringList.Create;
  FVisibleList.OnChanging := @VisibleListChanged;
  //FVisibleList.Sorted:=True;

  dgLibrary.DefaultColWidth:=FConfig.DefaultColWidth;
  dgLibrary.DefaultRowHeight:=Round(FConfig.DefaultColWidth * 1.3) +
                              dgLibrary.Canvas.TextHeight('A') * 3;

  s := IncludeTrailingPathDelimiter(FConfig.SyncPath) + 'cbzReadingList.json';
  if FileExists(s) then
  begin
    FReadingList.LoadFromFile(s);
    cbReadingList.Clear;
    for s in FReadingList do
        cbReadingList.items.add(ExtractFileName(s))
  end;

  for c := 'Z' downto 'A' do
    with TSpeedButton.Create(self) do
    begin
      Caption := c;
      align := altop;
      parent := pnlbtns;
      //enabled := false;
      onclick := @btnletterclick;
    end;

  SetCaption;

  if not CheckPaths then
    ShowMessage('You must set the Library Path and the SyncPath before using the library.');
end;

function TcbzLibrary.CheckPaths:Boolean;
begin
  result := DirectoryExists(FConfig.LibPath) and
            DirectoryExists(FConfig.SyncPath);
end;

procedure TcbzLibrary.StopThreads;
begin
  if Assigned(FThreadSearchFiles) then
  begin
    FThreadSearchFiles.Terminate;
    FThreadSearchFiles.WaitFor;
    FreeAndNil(FThreadSearchFiles);
  end;

  if Assigned(FThreadScrub) then
  begin
    if FThreadScrub.Paused then
      FThreadScrub.Paused := true;
    FThreadScrub.Terminate;
    FThreadScrub.Waitfor;
    //FreeAndNil(FThreadScrub);
  end;

  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
    FreeAndNil(FFillThread);
  end;
end;

procedure TcbzLibrary.FormDestroy(Sender: TObject);
begin
  FVisibleList.Free;
  FFileList.Free;
  //FBtnList.Free;

  if FOwnConfig and ASsigned(FConfig) then
    FConfig.Free;

  Flog.Log('cbzLibrary destroyed.');
  Flog := nil;
end;

procedure TcbzLibrary.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TFormWait.Create(Application) do
  begin
    Show;
    BringToFront;
  end;

  FConfig.LibCurPath:=CurrentPath;
  FConfig.Save(FConfigFile);
  FConfig.SaveForm(Self);
  CheckModified;

  StopThreads;

  CloseAction := caFree;
end;

procedure TcbzLibrary.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Canclose := not Assigned(FFillThread);
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
    btnScrub.Enabled:=True;
  end
  else
  if DirectoryExists(FFileList.RootPath) then
  begin
    CurrentPath := FFileList.RootPath;
    FLvl := length(CurrentPath.Split([PathDelim]));
    btnRefresh.Enabled:=false;
    btnScrub.Enabled:=False;
    Progress(Self, 0, 0, 0, 'Scanning...');
    FThreadSearchFiles := ThreadedSearchFiles(CurrentPath, ['*.cbz'], @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
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
        begin
          Brush.Color := clLime;
          {$ifdef Darwin}
          Font.Color := clBlack;
          {$endif}
        end
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
          pic := TFileItem(FVisibleList.Objects[p]).Image;
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
    ACol := 0; aRow := 0; // disable hint
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
    PopupMenu1.PopUp(dgLibrary.ClientOrigin.x + X, dgLibrary.ClientOrigin.y + Y);
end;

procedure TcbzLibrary.PopupMenu1Popup(Sender: TObject);
begin
  EnableActions;
  //mnuDelete.Enabled:= not Assigned(FThreadSearchFiles);
  //mnuCut.Enabled := not FileExists(SelectedStr);
  //mnuPaste.Enabled:= Assigned(FFileToCopy);
end;

procedure TcbzLibrary.SetCaption;
begin
  Caption := GetFileVersionInternalName + ' (' + GetFileVersion + ') Lib: ' + FCurrentPath;
end;

procedure TcbzLibrary.EnableActions;
begin
  ActionCopyToMngr.enabled := Assigned(SelectedObj);
  ActionCreateFolder.enabled := True;
  ActionCut.enabled := FileExists(SelectedStr) and not Assigned(FFileToCopy);
  ActionDelete.enabled := FileExists(SelectedStr) or DirectoryExists(SelectedStr);
  ActionPaste.enabled := Assigned(FFileToCopy);
  ActionMarkasRead.enabled := Assigned(SelectedObj);
  ActionMarkasUnread.enabled := Assigned(SelectedObj);
  ActionRename.Enabled:= DirectoryExists(SelectedStr) or FileExists(SelectedStr);
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
    if Assigned(FindForm(ExtractFileName(s))) then
      FindForm(ExtractFileName(s)).BringToFront
    else
      ShowComics(FLog, TFileItem(SelectedObj), FConfig)
  else
  ;
end;

procedure TcbzLibrary.cbHideReadClick(Sender: TObject);
begin
  Fconfig.HideRead:=cbHideRead.Checked;
  FConfig.Save(FConfigFile);
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
    StopThreads;
    btnRefresh.enabled := False;
    btnScrub.enabled := False;
    //FFileList.Clear;
    FLog.Log('TCbzLibrary.btnRefreshClick : Refresh started.');
    FThreadSearchFiles := ThreadedSearchFiles(FCurrentPath, ['*.cbz'], @FoundFile, @SearchEnded,
                                              @Progress, //str_scanning
                                              'scanning : ', [sfoRecurse]);
  end;
end;

procedure TcbzLibrary.btnScrubClick(Sender: TObject);
begin
  btnRefresh.enabled := False;
  btnScrub.enabled := False;
  FThreadScrub := TThreadScrub.Create(FLog, FFileList, FConfig, @ThreadScrubNotify, @ThreadScrubTerminate, @Progress);
end;

procedure TcbzLibrary.ThreadScrubTerminate(Sender : TObject);
begin
  FThreadScrub := nil;
  btnRefresh.Enabled:=True;
  btnScrub.enabled := True;
  FillGrid;
end;

procedure TcbzLibrary.btnTestClick(Sender: TObject);
begin
  ;
end;

procedure TcbzLibrary.ActionMarkAsUnreadExecute(Sender: TObject);
begin
  ChangeReadState(false);
end;

procedure TcbzLibrary.ActionMarkasReadExecute(Sender: TObject);
begin
  ChangeReadState(true);
end;

procedure TcbzLibrary.ChangeReadState(aReadState: boolean);
var
  i : integer;
begin
  if FileExists(SelectedStr) then
    with TFileItem(SelectedObj) do
      ReadState := aReadState
  else
  for i := 0 to FFileList.Count - 1 do
    if FFileList[i].StartsWith(SelectedStr) then
      if FileExists(FFileList[i]) then
         with TFileItem(FFileList.Objects[i]) do
           ReadState := aReadState;

  if not cbHideRead.Checked then
  begin
    dgLibrary.InvalidateCell(dgLibrary.Col, dgLibrary.Row);
    UpdateNbItems;
  end
  else
    FillGrid;

  FFileList.SaveToFile(GetCacheFileName);
end;

procedure TcbzLibrary.ActionRenameExecute(Sender: TObject);
var
  new, v : string;
  done : boolean;
begin
  if FileExists(SelectedStr) then
  begin
    v := extractfilename(SelectedStr);
    done := InputQuery('Rename File', 'Input new Filename', v);
    new := IncludeTrailingPathDelimiter(FCurrentPath) + v;
    if done then
      if FileExists(new) then
        ShowMessage('File "' + new + '" already exists !')
      else
      begin
        RenameFile(SelectedObj.Filename, new);
        SelectedObj.Deleted := True;
        btnRefresh.Click;
      end;
  end
  else
  if DirectoryExists(SelectedStr) then
  begin
    v := GetLastPath(SelectedStr);

    done := InputQuery('Rename Folder', 'Input new Foldername', v);
    new := IncludeTrailingPathDelimiter(FCurrentPath) + v;

    if done then
      if DirectoryExists(new) then
        ShowMessage('Folder "' + new + '" already exists !')
      else
      begin
        RenameFile(SelectedStr, new);
        btnRefresh.Click;
      end;

  end;
end;

procedure TcbzLibrary.ActionCutExecute(Sender: TObject);
begin
  if FileExists(SelectedStr) then
    FFileToCopy := SelectedObj;
end;

procedure TcbzLibrary.ActionDeleteExecute(Sender: TObject);
begin
  if Assigned(SelectedObj) and not DirectoryExists(SelectedStr) then
  begin
    TFileItem(SelectedObj).Deleted:=True;

    if FileExists(SelectedStr) then
      if MessageDlg('Confirmation', 'Delete comic file as well ?', mtInformation, mbYesNo, 0) = mryes then
        DeleteFile(SelectedStr);

    FillGrid;
  end
  else
  if DirectoryExists(SelectedStr) then
    if MessageDlg('Confirmation', 'Delete folder and all comic files ?', mtInformation, mbYesNo, 0) = mryes then
    begin
      KillFolder(SelectedStr);
      btnRefresh.Click;
    end;
end;

procedure TcbzLibrary.ActionPasteExecute(Sender: TObject);
var
  dest, dfile, ext : string;
  i : integer;
begin
  i := 1;
  dest := IncludeTrailingPathDelimiter(FCurrentPath) +
          ExtractFileName(FFileToCopy.Filename);

  dfile := ExtractFileName(FFileToCopy.Filename);
  ext := ExtractFileExt(dfile);

  while FileExists(dest) do
  begin
    dest := Format('%s%s(%d)%s',
                   [IncludeTrailingPathDelimiter(FCurrentPath),
                    ChangeFileExt(ExtractFileName(FFileToCopy.Filename), ''),
                    i, Ext]);
    inc(i);
  end;

  if RenameFile(FFileToCopy.Filename, dest) then
  begin
    FFileToCopy.Deleted := True;
    btnRefresh.Click;
  end;

  FFileToCopy := nil;
end;

procedure TcbzLibrary.btnTopPathClick(Sender: TObject);
begin
  CurrentPath := FFileList.RootPath;
  Flvl := 1;
  FillGrid
end;
{
procedure TcbzLibrary.Button1Click(Sender: TObject);
const
  accented : String =   'ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÇçÌÍÎÏìíîïÙÚÛÜùúûüÿÑñ';
    procedure dump(s, fnm : string);
  var
    p: PChar;
    CPLen: integer;
    FirstByte, SecondByte, ThirdByte, FourthByte: Char;
    tmp : String;
  begin
    tmp := '';
    p:=PChar(s);
    repeat
      CPLen := UTF8CodepointSize(p);

      if CPLen = 1 then
        tmp := tmp + String(p[0])
      else
      // Here you have a pointer to the char and its length
      // You can access the bytes of the UTF-8 Char like this:
      if CPLen >= 1 then
        tmp := tmp + hexStr(integer(p[0]), 2);  //FirstByte := P[0];
      if CPLen >= 2 then
        tmp := tmp + hexStr(integer(p[1]), 2); // SecondByte := P[1];
      if CPLen >= 3 then
        tmp := tmp + hexStr(integer(p[2]), 2); //ThirdByte := P[2];
      if CPLen = 4 then
        tmp := tmp + hexStr(integer(p[3]), 2); //FourthByte := P[3];

      tmp := tmp + #13#10;
      inc(p,CPLen);
    until (CPLen=0) or (p^ = #0);

    with TStringList.Create do
    try
      Add(tmp);
      SaveToFile(fnm);
    finally
      Free;
    end;
  end;

var
  s : string;
begin
  with TSelectDirectoryDialog.Create(Application) do
    if Execute then
      s := Filename;

  dump(accented, 'c:\temp\accented1.txt');
  dump(s, 'c:\temp\accented2.txt');
end;
}
procedure TcbzLibrary.btnReturnClick(Sender: TObject);
begin
  if CurrentPath = FFileList.RootPath then
    Exit;

  CurrentPath := ExcludeTrailingPathDelimiter(ExtractFilePath(CurrentPath));
  FillGrid;
end;

procedure TcbzLibrary.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TcbzLibrary.mnuConfigClick(Sender: TObject);
var
  s1, S2 : string;
begin
  StopThreads;

  with TConfigFrm.Create(Application) do
  try
    s1 := Fconfig.LibPath;
    s2 := FConfig.SyncPath;
    Config := FConfig;
    if ShowModal = mrOk then
    begin
      FConfig.Save(FConfigFile);
      Flog.Log('Config saved.');
      if (s1 <> Fconfig.LibPath) or (s2 <> FConfig.SyncPath) then
      begin
        ShowMessage('cbzLibrary must be restarted.');
        Self.Close;
      end;
    end;
  finally
    free;
  end;
end;

procedure TcbzLibrary.mnuStampsClick(Sender: TObject);
var
  i : integer;
begin
  mnuStamps.Enabled := false;
  if Assigned(FThreadScrub) then
      FThreadScrub.Paused := True;
  try
    for i := 0 to FFileList.count - 1 do
      // make stamp if needed
      with TFileItem(FFileList.Objects[i]) do
      begin
        Progress(Self, 0, i, FFileList.count, Format('Creating covers %d on %d ...', [i+1, FFileList.count]));
        GenerateStamp;

        Application.processMessages;
        if Application.Terminated then
          exit;
      end;
  finally
    mnuStamps.Enabled := true;
    if Assigned(FThreadScrub) then
    begin
      if FThreadScrub.Paused then
        FThreadScrub.Paused := False;
    end;
    Progress(Self, 0, 0, 0, 'Ready.');
  end;
end;

function TcbzLibrary.GetCacheFileName: String;
begin
  result :=
{$if defined(Darwin) or defined(Linux)}
  expandfilename('~/') + CS_CONFIG_PATH + '/Library/' +
{$else}
  ConfigPath + 'Library\' +
{$endif}
  'cbzLibrary.xml';
  ForceDirectories(ExtractFilePath(result));
end;

procedure TcbzLibrary.SetCurrentPath(AValue: String);
begin
  if FCurrentPath=AValue then Exit;
  FCurrentPath:=AValue;
  SetCaption;
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
  if not CheckPaths then
    exit;

  cbSearch.Enabled:=False;
  if Assigned(FFillThread) then
  begin
    FFillThread.Terminate;
    FFillThread.Waitfor;
  end;

  Progress(Self, 0, 0, 0, 'Loading folder...');
  btnRefresh.Enabled:=False;
  btnScrub.Enabled:=False;

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

  if Assigned(FThreadScrub) then
  begin
    FThreadScrub.Paused := true;
//    FThreadScrub.WaitFor;
  end;

  mnuFile.Enabled := false;
  mnuEdit.Enabled := false;

  FFillThread := TThreadFill.Create(Flog, fs,@ThreadFillTerminate, @Progress);
end;

procedure TcbzLibrary.ThreadFillTerminate(Sender: TObject);
var
  oldtoprow, oldrow, i : Integer;
  s : string;
begin
  Progress(Self, 0, 0, 0, 'Sorting...');
  FFillThread := nil;
  btnRefresh.Enabled:=true;
  btnScrub.enabled := true;
  mnuFile.Enabled := true;
  mnuEdit.Enabled := true;
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
  Progress(Self, 0, 0, 0, 'Ready.');

  //if Assigned(FThreadScrub) then
  //begin
  //  if FThreadScrub.Paused then
  //    FThreadScrub.Paused := False;
  //end
  //else
  //  btnScrubClick(nil);
end;


procedure TcbzLibrary.SearchEnded(Sender: TObject);
begin
  FThreadSearchFiles := nil;
  StatusBar1.Panels[2].Text := 'Ready.';

  FFileList.Sort;
  FFileList.SaveToFile(GetCacheFileName);
  btnRefresh.Enabled:=True;
  btnSCrub.Enabled:=True;
  UpdateNbItems;
  UpdateVisibleDates;
  Application.QueueAsyncCall(@DoFillGrid, 0);

  FLog.Log('TCbzLibrary.SearchEnded : Refresh ended.');

  // start scrub
  btnScrubClick(nil);
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

  try
    s := ExtractFileName(aFileName);

    if (FFileList.IndexOf(aFilename) >= 0) then
    with FFileList do
      if TFileItem(Objects[IndexOf(aFilename)]).Deleted or s.StartsWith('._') then
        Delete(IndexOf(aFilename));

    if (FFileList.IndexOf(aFilename) < 0) and not s.StartsWith('._') then
    begin
      fi := TFileItem.Create(FFileList, FLog, aFilename);
      fi.Text := GetLastPath(aFilename);
      FFileList.AddObject(aFilename, fi);
      FLog.Log('TCbzLibrary.FoundFile: Added:' + aFilename);

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
    end;
  except
    on E: Exception do
      FLog.Log('TcbzLibrary.FoundFile:Error:'+E.Message);
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

