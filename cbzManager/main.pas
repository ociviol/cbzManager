unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ExtCtrls, Grids, ActnList, Spin, uCbz,
{$ifdef Darwin}
  OpenSslSockets,
{$endif}
  Utils.Logger, Utils.SearchFiles, Utils.Gridhelper, Types,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  Utils.Arrays, uDataPool, uWorkerThread, uConfig;

type
  TProgressRec = class
    FPanel, FLabel: TPanel;
    FProgressBar: TProgressBar;
    FButton: TButton;
  end;

  { TMainFrm }
  TMainFrm = class(TForm)
    ActionCropTool: TAction;
    ActionRewriteManga: TAction;
    ActionUndoAll: TAction;
    ActionUndo: TAction;
    ActionSplitImage: TAction;
    ActionJoin: TAction;
    ActionRefresh: TAction;
    ActionChooseFolder: TAction;
    ActionMoveToTop: TAction;
    ActionMoveToBottom: TAction;
    ActionMoveDown: TAction;
    ActionMoveup: TAction;
    ActionDelete: TAction;
    ActionRotm90: TAction;
    ActionVertFlip: TAction;
    ActionRot90: TAction;
    ActionHorizFlip: TAction;
    ActionFirst: TAction;
    ActionLast: TAction;
    ActionList1: TActionList;
    btnCancel: TButton;
    btnFirst: TButton;
    btnHorizFlip: TButton;
    btnLast: TButton;
    btnRotateLeft: TButton;
    btnRotateRight: TButton;
    btnVertFlip: TButton;
    btnCrop: TButton;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    memoLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    N10: TMenuItem;
    N9: TMenuItem;
    N8: TMenuItem;
    N7: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N6: TMenuItem;
    N5: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    mnuAbout: TMenuItem;
    mnuConfig: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    mnuSetDefaultPath: TMenuItem;
    mnuChooseFolder: TMenuItem;
    mnuExit: TMenuItem;
    mnuFile: TMenuItem;
    Panel1: TPanel;
    PanelCrop: TPanel;
    pnlProgress: TPanel;
    Panel3: TPanel;
    pnlimgName: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Shape1: TShape;
    speLeft: TSpinEdit;
    speBottom: TSpinEdit;
    speRight: TSpinEdit;
    speTop: TSpinEdit;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure ActionChooseFolderExecute(Sender: TObject);
    procedure ActionCropToolExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionFirstExecute(Sender: TObject);
    procedure ActionHorizFlipExecute(Sender: TObject);
    procedure ActionJoinExecute(Sender: TObject);
    procedure ActionLastExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveToBottomExecute(Sender: TObject);
    procedure ActionMoveToTopExecute(Sender: TObject);
    procedure ActionMoveupExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRewriteMangaExecute(Sender: TObject);
    procedure ActionRot90Execute(Sender: TObject);
    procedure ActionRotm90Execute(Sender: TObject);
    procedure ActionSplitImageExecute(Sender: TObject);
    procedure ActionUndoAllExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionVertFlipExecute(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCropClick(Sender: TObject);
    procedure DrawGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseEnter(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSetDefaultPathClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure speBottomChange(Sender: TObject);
    procedure speLeftChange(Sender: TObject);
    procedure speRightChange(Sender: TObject);
    procedure speTopChange(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FLog: ILog;
    zf : TCbz;
    FInFill : Boolean;
    FConfigFile : String;
    FConfig : TConfig;
    FThreadSearchFiles : TThread;
    FTreeViewPaths : TStringlist;
    FClosing : Boolean;
    FProgress: TStringList;
    FoldPos: Integer;
    FThreadDataPool: TThreadDataPool;
    FJobpool : TJobPool;
    FWorkerThreads: Array of TCbzWorkerThread;
    FConvertReport : TstringList;
    FLastUPdate : TDateTime;
    Fignores : TStringlist;

    procedure HideCropTool;
    procedure SaveConfig;
    function CheckPrograms:boolean;
    procedure FillTreeView(const Path: String);
    function AddFileToTree2(const aFileName: string;
                            IsNew: Boolean = False): TTreeNode;
    procedure AddFileToTree(const aFileName: string;
                            IsNew: Boolean = False);
    function FindNode(const FileName: String): TTreeNode;
    function GetNode(const FileName: string): TTreeNode;
    function CreatePath(const Path: String): TTreeNode;
    procedure ExpandParents(aNode: TTreeNode);
    function NodeSort(Node1, Node2 : TTreeNode): Integer;
    procedure SearchEnded(Sender: TObject);
    procedure CreateConversionQueues;
    procedure ReduceConversionQueue(NewSize: Integer);
    procedure OnBadFile(Sender: TObject);
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    function GetProgress(ProgressID: QWord; Sender: TObject):TProgressRec;
    procedure SetMainImage(Index: Integer);
    procedure StampReady(ProgressID: QWord; Index: Integer);
    function SelectedGridItems: TIntArray;
    procedure SetAppCaption;
    procedure EnableActions;

    procedure AfterCellSelect(data : int64);
    procedure CheckVersionTerminate(Sender : TObject);
  public

  end;

  TTreeNodeEx = Class Helper for TTreeNode
  private
    function GetPath: String;
  protected
    property Path: String read GetPath;
  End;

  { TThreadCheckVersion }

  TThreadCheckVersion = Class(TThread)
  protected
    FNeedUpdate : Boolean;
  public
    constructor Create(aTerminate : TNotifyEvent);
    procedure Execute; override;
  end;

  { TDrawGridAccess }

  TDrawGridAccess = Class(TDrawGrid);

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  Config, LclIntf,
  Utils.Strings, frmwait, fpHttpClient,
{$if defined(Darwin) or defined(Linux)}
  unix,
{$endif}
  Utils.SoftwareVersion, uDataTypes,
  Utils.ZipFile, Utils.Graphics,
  uLoadReport;

const
//  CS_CONFIG = 'config';
{$if defined(Darwin) or defined(Linux)}
  CS_CONFIG_PATH = '.config/cbzManager';
  CS_CONFIG_JSON = '/config.json';
{$else}
  CS_CONFIG_PATH = 'cbzManager';
{$endif}

{ TThreadCheckVersion }

constructor TThreadCheckVersion.Create(aTerminate: TNotifyEvent);
begin
  FNeedUpdate := false;
  OnTerminate := aTerminate;
  FreeOnTerminate:=True;
  inherited Create(false);
end;

procedure TThreadCheckVersion.Execute;
var
  t : TStringList;
  i : integer;
  ar : Array of String;
  v : string;
begin
  try
    with TFPHTTPClient.Create(nil) do
    try
      t := TStringList.Create;
      try
        SimpleGet('https://ollivierciviolsoftware.wordpress.com/version/', t);

        with t do
          for i := 0 to Count - 1 do
            if Strings[i].StartsWith('<meta property="og:description" content=') then
                v := copy(Strings[i], 41, length(Strings[i])-44);

        v := StringReplace(v, '"', '', [rfReplaceAll]);
        v := StringReplace(v, ' ', ',', [rfReplaceAll]);
        ar := v.Split([',']);

        for i:=low(ar) to high(ar) do
  {$IFDEF Drawin}
          if ar[i].StartsWith('osx:') then
          begin
            v := copy(ar[i], 5, length(ar[i]));
            break;
          end;
  {$ELSE}
          if ar[i].StartsWith('linux:') then
          begin
            v := copy(ar[i], 7, length(ar[i]));
            break;
          end;
  {$ENDIF}
          if v <> '' then
            FNeedUpdate := CompareVersion(GetFileVersion, v) > 0;
      finally
        t.Free;
      end;
    finally
      Free;
    end;
  except
  end;

  Terminate;
end;

//  CS_BDPATH = 'bdpath';
//  CS_CWEBP = 'cwebp';
//  CS_UNRAR =  'unrar';
//  CS_P7ZIP = 'p7zip';


{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
//var
//  t : Tstringlist;
//  s : string;
begin
  FClosing := False;
  FThreadSearchFiles := nil;
  //Caption := GetFileVersionInternalName + ' ' +
  //           GetFileVersion;
  //
  // make sure config folder exists
{$if defined(Darwin) or defined(Linux)}
  FConfigFile := expandfilename('~/') + CS_CONFIG_PATH;
  ForceDirectories(FConfigFile);
  FConfigFile := FConfigFile + CS_CONFIG_JSON;
{$else}
  FConfigFile := ChangeFileExt(Application.ExeName, '.json');
{$endif}
  // load config
  FConfig := TConfig.Load(FConfigFile);
  if FConfig.Wleft <> 0 then
    left := FConfig.Wleft;
  if FConfig.WTop <> 0 then
    top := FConfig.WTop;
  if FConfig.WWidth <> 0 then
    Width := FConfig.WWidth;
  if FConfig.WHeight <> 0 then
    Height := FConfig.WHeight;
  if FConfig.WTreeViewWidth <> 0 then
    TreeView1.Width := FConfig.WTreeViewWidth;
  // start logger
  FLog := GetILog(
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/' +
{$else}
    IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\' +
{$endif}
    'cbzManager.log', FConfig.bLog);

  Flog.Log('cbzManager started.');

  //FConfig.QueueSize := CPUCount;

  FTreeViewPaths := TStringlist.Create;
  FProgress := TStringList.Create;
  FIgnores := Tstringlist.Create;

  zf := TCbz.Create(FLog, DrawGrid1.DefaultColWidth - 5,
    DrawGrid1.DefaultRowHeight - 5, @StampReady);

  // check required programs
  if not CheckPrograms then
    exit;

  FJobpool := TJobPool.Create(FLog);

  FConvertReport := TstringList.Create;

  FThreadDataPool := TThreadDataPool.Create(FConfig.QueueSize,
                                            FLog, FConfig.NbThreads);
  CreateConversionQueues;
  //
  if DirectoryExists(FConfig.BdPathPath) then
    FillTreeView(FConfig.BdPathPath);

  SetAppCaption;
  TThreadCheckVersion.Create(@CheckVersionTerminate);
end;

procedure TMainFrm.CheckVersionTerminate(Sender : TObject);
begin
  if TThreadCheckVersion(Sender).FNeedUpdate then
    if MessageDlg('A new version is available, do you want to update ?',
                  mtConfirmation, MbYesNo, 0) = MrYes then
      OpenUrl('https://ollivierciviolsoftware.wordpress.com');
end;

procedure TMainFrm.SetAppCaption;
begin
  Caption := GetFileVersionInternalName + ' (' + GetFileVersion + ')';
  Caption := format('%s - Path : %s - %d Queues - %d Encoding threads',
                    [Caption, Fconfig.BdPathPath,
                    FThreadDataPool.PoolSize,
                    FThreadDataPool.NbWorkers]);
end;

procedure TMainFrm.EnableActions;
begin
  ActionUndo.Enabled := (zf.Mode <> zmClosed) and zf.CanUndo;
  ActionUndoAll.Enabled := (zf.Mode <> zmClosed) and zf.CanUndo;
  //ActionCrop.Enabled := (zf.Mode <> zmClosed) and
  //  ImageEnView1.CropToolInteraction.Selected;
  //ActionCropAll.Enabled := (zf.Mode <> zmClosed) and
  //  ImageEnView1.CropToolInteraction.Selected;
  //ActionFilters.Enabled := (zf.Mode <> zmClosed) and
  //  Assigned(ImageEnView1.Bitmap);
  ActionDelete.Enabled := zf.Mode <> zmClosed;
  //ActionJoin.Enabled := (zf.Mode <> zmClosed) and
  //  (Length(SelectedGridItems) = 2);
  //ActionExport.Enabled := (zf.Mode <> zmClosed) and
  //  (Length(SelectedGridItems) = 1);
  //ActionImportAbove.Enabled := (zf.Mode <> zmClosed) and
  //  (Length(SelectedGridItems) = 1);
  //ActionImportBelow.Enabled := (zf.Mode <> zmClosed) and
  //  (Length(SelectedGridItems) = 1);
  //ActionExportSelection.Enabled := (zf.Mode <> zmClosed) and
  //(Length(SelectedGridItems) >= 1);
  ActionMoveUp.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
  ActionMoveDown.Enabled := (zf.Mode <> zmClosed) and
    (DrawGrid1.Position < zf.ImageCount - 1);
  //ActionClip.Enabled := (zf.Mode <> zmClosed) and
  //  ImageEnView1.CropToolInteraction.Selected;
  //ActionRemoveBorders.Enabled := (zf.Mode <> zmClosed) and
  //  (Length(SelectedGridItems) = 1);
  //ActionRemoveAllBorders.Enabled := (zf.Mode <> zmClosed);
  ActionMoveToTop.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
  ActionMoveToBottom.Enabled := (zf.Mode <> zmClosed) and
    (DrawGrid1.Position < zf.FileCount - 1);
  //ActionFit.Enabled := (zf.Mode <> zmClosed) and Assigned(ImageEnView1.Bitmap);
  ActionCropTool.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position >= 0);

  // files
  ActionRewriteManga.Enabled := (zf.Mode <> zmClosed);
  ActionChooseFolder.Enabled := not FInFill;
  //ActionRewrite.Enabled := Assigned(TreeView1.Selected) and
  //  (TreeView1.SelectionCount = 1);
  //ActionJoinFiles.Enabled := TreeView1.SelectionCount > 1;
  //ActionRename.Enabled := Assigned(TreeView1.Selected) and
  //  (TreeView1.SelectionCount = 1) and not(TreeView1.Selected.HasChildren);
  //ActionRefresh.Enabled := (FPath.Length > 0) and not FInFill;
  //ActionTest.Enabled := Assigned(TreeView1.Selected);
  //ActionFixFilenames.Enabled := Assigned(TreeView1.Selected);
  //ActionCollapsenodes.Enabled := Assigned(TreeView1.Selected) and
  //  TreeView1.Selected.HasChildren;
  //ActionExpandNodes.Enabled := Assigned(TreeView1.Selected) and
  //  TreeView1.Selected.HasChildren;
  //ActionDeleteFile.Enabled := Assigned(TreeView1.Selected) and
  //  not TreeView1.Selected.HasChildren;
  //ActionSetDefaultPath.Enabled := FPath <> '';
  //ActionCreateFolder.Enabled := Assigned(TreeView1.Selected) and
  //  (TDirectory.Exists(TreeView1.Selected.Path));
  //ActionReader.Enabled := (zf.Mode <> zmClosed);
  // btns
  ActionRotm90.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (Length(SelectedGridItems) = 1);
  ActionRot90.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (Length(SelectedGridItems) = 1);
  ActionFirst.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (DrawGrid1.Position > 0);
  ActionLast.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (DrawGrid1.Position < zf.ImageCount - 1);
  ActionHorizflip.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (Length(SelectedGridItems) = 1);
  ActionVertflip.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (Length(SelectedGridItems) = 1);
  ActionJoin.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
    (Length(SelectedGridItems) = 2);
  ActionSplitImage.Enabled := (zf.Mode <> zmClosed) and (Length(SelectedGridItems) = 1);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  SaveConfig;

  FTreeViewPaths.Free;
  FProgress.Free;
  FIgnores.Free;
  zf.Close;
  zf.Free;
  FConvertReport.Free;

  Flog.Log('cbzManager destroyed.');
  // destroy logger
  Flog := nil;
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  if not Application.Terminated then
    EnableActions;
end;

procedure TMainFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i : Integer;

  procedure SetCursor(aCursor : TCursor);
  var
    j : integer;
  begin
    for j := 0 to ComponentCount - 1 do
      if Components[j] is TControl then
        TControl(Components[j]).Cursor := aCursor;
  end;

begin
  SetCursor(crHourglass);
  Update;
  Refresh;
  FClosing := True;
  with TFormWait.Create(nil) do
  begin
    Show;
    Update;
    Refresh;
    Panel1.Update;
    Panel1.Refresh;
    Application.ProcessMessages;
    ThreadSwitch;
  end;

  if Assigned(FThreadSearchFiles) then
    FThreadSearchFiles.Terminate;

  FThreadDataPool.Stop;

  //with FOperationsList.LockList do
  //  try
  //    Clear;
  //  finally
  //    FOperationsList.UnlockList;
  //  end;
  //
  //with FOperationThread do
  //begin
  //  Terminate;
  //  WaitFor;
  //  Free;
  //end;
  //FOperationsList.Free;

  for i := 0 to Length(FWorkerThreads) - 1 do
    with FWorkerThreads[i] do
    begin
      FLog.Log(ClassName + ' Stopping WorkerThread ' + IntToStr(i + 1));
      Terminate;
    end;

  for i := 0 to Length(FWorkerThreads) - 1 do
    with FWorkerThreads[i] do
    begin
      WaitFor;
      Free;
      Application.ProcessMessages;
    end;

  FThreadDataPool.Free;
  FJobpool.Free;
  CloseAction:=caFree;
end;

procedure TMainFrm.CreateConversionQueues;
begin
  while Length(FWorkerThreads) < FConfig.QueueSize do
  begin
    SetLength(FWorkerThreads, Length(FWorkerThreads) + 1);
    FLog.Log(ClassName + ' Creating WorkerThread ' +
      IntToStr(Length(FWorkerThreads)));
    FWorkerThreads[Length(FWorkerThreads) - 1] :=
      TCbzWorkerThread.Create(FJobpool,
      FThreadDataPool.Pool[Length(FWorkerThreads) - 1], @Progress,
      @AddFileToTree, FLog, FConvertReport, @OnBadFile);
  end;
end;

procedure TMainFrm.ReduceConversionQueue(NewSize: Integer);
var
  aFile: string;
  aType: TArcType;
begin
  while Length(FWorkerThreads) > NewSize do
  begin
    aFile := '';
    aType := arcUnknown;
    with FWorkerThreads[Length(FWorkerThreads) - 1] do
    begin
      if Assigned(CurJob) and (CurJob.Status = jsProcessing) then
      begin
        aFile := CurJob.FileName;
        aType := CurJob.arcType;
      end;
      Terminate;
      WaitFor;
      Free;
    end;
    FThreadDataPool.Pool[Length(FWorkerThreads) - 1].Disable;
    FThreadDataPool.RemovePool(length(FWorkerThreads) - 1);
    SetLength(FWorkerThreads, Length(FWorkerThreads) - 1);
    if aFile <> '' then
      FJobpool.AddJob(aFile, aType);
  end;
end;

procedure TMainFrm.OnBadFile(Sender: TObject);
begin
  memoLog.Lines.Assign(FConvertReport);
  memoLog.Visible := True;
end;

procedure TMainFrm.StampReady(ProgressID: QWord; Index: Integer);
begin
  if Application.Terminated then
    Exit;

  if Index < 0 then
    Progress(Self, ProgressID, 0, 0, '')
  else
  begin
    Progress(Self, ProgressID, zf.StampCount, zf.ImageCount, 'Generating stamps...');
    DrawGrid1.InvalidateCell(0, Index);
    Application.ProcessMessages;
  end;
end;

procedure TMainFrm.mnuAboutClick(Sender: TObject);
begin
  ShowMessage(GetFileVersionInternalName + ' ' +
              GetFileVersion + ' © ' + GetFileVersionCopyright);
end;


procedure TMainFrm.SaveConfig;
begin
  FConfig.Wleft := Left;
  FConfig.WTop := Top;
  FConfig.WWidth := Width;
  FConfig.WHeight := Height;
  FConfig.WTreeViewWidth := TreeView1.Width;

  FConfig.Save(FConfigFile);
  Flog.Log('Config saved.');
end;

function TMainFrm.CheckPrograms:boolean;
var
  msg : string;
begin
  msg := '';
  if not FileExists(Fconfig.cwebp) then
  begin
    msg := msg + 'cwebp not found, install using ' +
{$ifdef darwin}
          '''brew install webp''' + #13;
{$else}
           '''sudo apt install webp''' + #13;
{$endif}
  Flog.Log('required binary missing : cwebp.');
  end;

  if not FileExists(Fconfig.unrar) then
  begin
    msg := msg + 'unrar not found, install using ' +
    {$ifdef darwin}
              '''brew install unrar''' + #13;
    {$else}
               '''sudo apt install unrar''' + #13;
    {$endif}
    Flog.Log('required binary missing : unrar.');
  end;
  if not FileExists(Fconfig.p7zip) then
  begin
    msg := msg + '7z not found, install using ' +
    {$ifdef darwin}
              '''brew install p7zip''' + #13;
    {$else}
               '''sudo apt install p7zip-full''' + #13;
    {$endif}
    Flog.Log('required binary missing : p7zip.');
  end;
{$if defined(Darwin)}
  if not FileExists('/usr/local/lib/libwebp.dylib') then
  begin
    msg := msg + 'libwebp not found, install using ''brew install libwebp-dev''' + #13;
    Flog.Log('required library missing : libwebp-dev.');
  end;	 
{$elseif Defined(Linux)}
  if not FileExists('/usr/lib/x86_64-linux-gnu/libwebp.so') then
  begin
    msg := msg + 'libwebp not found, install using ''sudo apt install libwebp-dev''' + #13;
    Flog.Log('required library missing : libwebp-dev.');
  end;
{$endif}
  if msg <> '' then
  begin
    msg := 'WARNING:' +#13 + #13 + msg;
    ShowMessage(msg);
    Application.Terminate;
    Halt;
  end;
  Result := True;
end;

procedure TMainFrm.mnuExitClick(Sender: TObject);
begin
  close;
end;

procedure TMainFrm.mnuSetDefaultPathClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TMainFrm.PopupMenu1Popup(Sender: TObject);
begin
  EnableActions;
end;

procedure TMainFrm.speBottomChange(Sender: TObject);
begin
  Shape1.Height := speBottom.Value;
end;

procedure TMainFrm.speLeftChange(Sender: TObject);
begin
  Shape1.left := speLeft.Value;
  speRight.MaxValue:=(Image1.left+(Image1.DestRect.Right - Image1.DestRect.left))-Shape1.left;
end;

procedure TMainFrm.speRightChange(Sender: TObject);
begin
  Shape1.width := speRight.Value;
end;

procedure TMainFrm.speTopChange(Sender: TObject);
begin
  Shape1.Top := speTop.Value;
  speBottom.MaxValue:=(Image1.top+(Image1.DestRect.Bottom - Image1.DestRect.Top))-shape1.top;
end;

procedure TMainFrm.AfterCellSelect(data : int64);
begin
  EnableActions;
  SetMainImage(DrawGrid1.Position);
end;

procedure TMainFrm.HideCropTool;
begin
  PanelCrop.Visible:=false;
  Shape1.Visible:=False;
end;

procedure TMainFrm.SetMainImage(Index: Integer);
var
  b: TBitmap;
begin
  Screen.Cursor := crHourGlass;
  try
    if (zf.Mode <> zmClosed) and (Index < zf.ImageCount) then
    begin
      {
      if (Index = zf.FileCount - 1) then
      begin
        Image1.Visible := False;
        pnlimgName.Caption := format('%s', [zf.FileNames[Index]]);
      end
      else
      }
      begin
        HideCropTool;
        Image1.Visible := True;
        b := zf.Image[Index];
        if Assigned(b) then
        begin
          pnlimgName.Caption := format('%s (%dx%d)',
            [zf.FileNames[Index], b.Width, b.Height]);
          if Assigned(b) then
            Image1.Picture.Bitmap := b;
        end;
      end;
    end
    else
      Image1.Picture.Clear;
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainFrm.TreeView1Change(Sender: TObject; Node: TTreeNode);
  procedure EnableControls(State: Boolean);
  begin
    zf.Close;
    with DrawGrid1 do
    begin
      FoldPos := -1;
      RowCount := 1;
      Row := 0;
      Refresh;
    end;
    Image1.Picture.Clear;
    pnlimgName.Caption := '';
    EnableActions;
    // Application.ProcessMessages;
  end;

begin
  if TreeView1.SelectionCount = 1 then
    if Assigned(Node) and not Node.HasChildren then
    begin
      if FileExists(Node.Path) then
      begin
        EnableControls(True);
        try
          Node.Data := nil;
          TreeView1.Refresh;
          zf.Open(Node.Path, zmRead);
          zf.Progress := @Progress;
          with DrawGrid1 do
          begin
            Visible := False;
            Max := zf.ImageCount;
            Position := 0;
            Visible := True;
            Application.QueueAsyncCall(@AfterCellSelect, 0);
          end;
        except
        end;
      end;
    end
    else
    begin
      EnableControls(False);
      zf.ClearUndo;
    end;
end;

procedure TMainFrm.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender.Canvas do
    if Node.Data <> nil then
      Font.Style := [fsBold]
    else
      Font.Style := [];
end;

function TMainFrm.SelectedGridItems: TIntArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 0 to DrawGrid1.Max - 1 do
    if DrawGrid1.Selected[i] then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := i;
    end;
end;

procedure JoinImages(b1, b2, bFinal: TBitmap);
begin
  bFinal.PixelFormat := b1.PixelFormat;
  bFinal.Width := b1.Width * 2;
  bFinal.Height := b1.Height;
  bFinal.Canvas.Draw(0, 0, b1);
  bFinal.Canvas.StretchDraw(Rect(b1.Width, 0, bFinal.Width, bFinal.Height), b2);
end;

procedure TMainFrm.ActionHorizFlipExecute(Sender: TObject);
var
  rpos: Integer;
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      rpos := DrawGrid1.Position;
      zf.HorizontalFlip(SelectedGridItems, @Progress);
      DrawGrid1.Position := rpos;
      SetMainImage(DrawGrid1.Position);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.ActionJoinExecute(Sender: TObject);
var
  lst: TIntArray;
  ar : TIntArray;
  b1, b2, bFinal: TBitmap;
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      lst := SelectedGridItems;
      if Length(lst) <> 2 then
        raise Exception.Create('Can only join 2 images.');

      b1 := zf.Image[lst[0]];
      try
        b2 := zf.Image[lst[1]];
        try
          bFinal := TBitmap.Create;
          try
            JoinImages(b1, b2, bFinal);
            {
              bfinal.PixelFormat := b1.PixelFormat;
              bfinal.Width := b1.Width *2;
              bfinal.Height := b1.Height;
              bfinal.Canvas.Draw(0, 0, b1);
              bfinal.Canvas.StretchDraw(Rect(b1.Width, 0, bfinal.Width-1, bfinal.Height-1), b2);
            }
            SetLength(ar, 1);
            zf.Image[lst[0]] := bFinal;
            ar[0] := lst[1];
            zf.Delete(ar);
            DrawGrid1.Max := zf.ImageCount;
            DrawGrid1.Position := lst[0];
            DrawGrid1.Invalidate;
            Application.QueueAsyncCall(@AfterCellSelect, 0);
          finally
            bFinal.Free;
          end;
        finally
          b2.Free;
        end;
      finally
        b1.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.ActionFirstExecute(Sender: TObject);
begin
   if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := 0;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TMainFrm.ActionDeleteExecute(Sender: TObject);
var
  rpos: Integer;
  lst: TIntArray;
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      rpos := DrawGrid1.Position;
      lst := SelectedGridItems;

      TreeView1.Selected.Text := ExtractFileName(zf.Delete(lst, @Progress));
      DrawGrid1.Max := zf.ImageCount;
      while (rpos >= zf.ImageCount) and (rpos > 0) do
        dec(rpos);
      DrawGrid1.Position := rpos;
      DrawGrid1.Invalidate;
      Application.QueueAsyncCall(@AfterCellSelect, 0);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.ActionChooseFolderExecute(Sender: TObject);
begin
  with SelectDirectoryDialog1 do
    if Execute then
    begin
      FConfig.BdPathPath := Filename;
      FillTreeView(FConfig.BdPathPath);
      SetAppCaption;
    end;
end;

procedure TMainFrm.ActionCropToolExecute(Sender: TObject);
var
  r : TRect;
begin
  PanelCrop.Visible := true;
  Update;
  r := Image1.DestRect;
  // shape
  Shape1.Left:= Image1.Left + r.left + 10;
  Shape1.top := Image1.Top + r.top + 10;
  Shape1.Width:= (r.Right - r.left) - 20;
  Shape1.Height := (r.Bottom - r.Top) - 20;
  // width
  speRight.MinValue := (r.Right - r.left) div 2;
  speRight.MaxValue := (r.Right - r.left);
  speRight.Value:=(r.Right - r.left) - 20;
  // height
  speBottom.MinValue:= (r.Bottom - r.Top) div 2;
  speBottom.MaxValue:= (r.Bottom - r.Top);
  speBottom.Value:= (r.Bottom - r.Top) - 20;
  //left
  speLeft.MinValue := Image1.Left + r.left;
  speLeft.MaxValue := Image1.Left + (r.Right - r.left) div 2;
  speLeft.Value:= Image1.Left + r.left + 10;
  // top
  speTop.MinValue:=Image1.top + r.Top;
  speTop.MaxValue:=Image1.Top + (r.Bottom - r.top) div 2;
  speTop.Value:= Image1.Top + r.top + 10;
  // shape
  //Shape1.Left:= Image1.left + r.left + 10;
  //Shape1.top := Image1.Top + r.top + 10;
  //Shape1.Width:= Image1.Left + (r.Right - r.left) - 20;
  //Shape1.Height := Image1.Top + (r.Bottom - r.Top) - 20;
  Shape1.Visible:=True;
end;

procedure TMainFrm.ActionLastExecute(Sender: TObject);
begin
   if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := zf.ImageCount - 1;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TMainFrm.ActionMoveDownExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    zf.Invert(DrawGrid1.Position, DrawGrid1.Position + 1, @Progress);
    DrawGrid1.Position := DrawGrid1.Position + 1;
    DrawGrid1.Invalidate;
    SetMainImage(DrawGrid1.Position);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionMoveToBottomExecute(Sender: TObject);
var
  b: TBitmap;
  ms: TMemoryStream;
  ar : TIntArray;
  ars : TStreamArray;
begin
  Screen.Cursor := crHourGlass;
  try
    b := zf.Image[DrawGrid1.Position];
    try
      ms := TMemoryStream.Create;
      b.SaveToStream(ms);
      SetLength(ar, 1);
      ar[0] := DrawGrid1.Position;
      zf.Delete(ar, @Progress);
      SetLength(ars, 1);
      ars[0] := ms;
      zf.Insert(ars, zf.FileCount);
      DrawGrid1.Position := zf.FileCount - 1;
      DrawGrid1.Invalidate;
      SetMainImage(DrawGrid1.Position);
    finally
      b.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionMoveToTopExecute(Sender: TObject);
var
  b: TBitmap;
  ms: TMemoryStream;
  ar : TIntArray;
  ars : TSTreamArray;
begin
  Screen.Cursor := crHourGlass;
  try
    b := zf.Image[DrawGrid1.Position];
    try
      ms := TMemoryStream.Create;
      b.SaveToStream(ms);
      SetLength(ar, 1);
      ar[0] := DrawGrid1.Position;
      zf.Delete(ar, @Progress);
      SetLength(ars, 1);
      ars[0] := ms;
      zf.Insert(ars, 0);
      DrawGrid1.Position := 0;
      DrawGrid1.Invalidate;
      SetMainImage(DrawGrid1.Position);
    finally
      b.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionMoveupExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    zf.Invert(DrawGrid1.Position, DrawGrid1.Position - 1, @Progress);
    DrawGrid1.Position := DrawGrid1.Position - 1;
    DrawGrid1.Invalidate;
    SetMainImage(DrawGrid1.Position);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionRefreshExecute(Sender: TObject);
begin
  FillTreeView(FConfig.BdPathPath);
end;

procedure TMainFrm.ActionRewriteMangaExecute(Sender: TObject);
begin
  with TreeView1 do
    if Assigned(Selected) then
      if not Selected.HasChildren then
      begin
        Screen.Cursor := crHourGlass;
        try
          zf.RewriteManga(@Progress);
          DrawGrid1.Invalidate;
          SetMainImage(DrawGrid1.Position);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
end;

procedure TMainFrm.ActionRot90Execute(Sender: TObject);
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      zf.Rotate(SelectedGridItems, -90, @Progress);
      SetMainImage(DrawGrid1.Position);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.ActionRotm90Execute(Sender: TObject);
begin
  if DrawGrid1.Position >= 0 then
  Screen.Cursor := crHourGlass;
  try
    zf.Rotate(SelectedGridItems, 90, @Progress);
    SetMainImage(DrawGrid1.Position);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionSplitImageExecute(Sender: TObject);
var
  b, b1, b2: TBitmap;
  ms1, ms2: TMemoryStream;
  ar : TIntArray;
  sa : TStreamArray;
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      b := zf.Image[DrawGrid1.Position];
      try
        b1 := TBitmap.Create;
        b2 := TBitmap.Create;
        try
          b1.PixelFormat := b.PixelFormat;
          b2.PixelFormat := b.PixelFormat;
          b1.Height := b.Height;
          b2.Height := b.Height;
          b1.Width := b.Width div 2;
          b2.Width := b.Width div 2;
          b1.Canvas.CopyRect(Rect(0, 0, b1.Width - 1, b1.Height - 1), b.Canvas,
            Rect(0, 0, b1.Width - 1, b1.Height - 1));
          b2.Canvas.CopyRect(Rect(0, 0, b2.Width - 1, b2.Height - 1), b.Canvas,
            Rect(b1.Width, 0, b.Width - 1, b.Height - 1));

          SetLength(ar, 1);
          ar[0] := DrawGrid1.Position;
          zf.Delete(ar, @Progress);
          ms1 := TMemoryStream.Create;
          ms2 := TMemoryStream.Create;
          b1.SaveToStream(ms1);
          b2.SaveToStream(ms2);
          SetLength(sa, 2);
          sa[0] := ms1;
          sa[1] := ms2;
          zf.Insert(sa, DrawGrid1.Position);
          SetMainImage(DrawGrid1.Position);
          DrawGrid1.Invalidate;
        finally
          b1.Free;
          b2.Free;
        end;
      finally
        b.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.ActionUndoAllExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    while zf.CanUndo do
      zf.Undo(nil);
    DrawGrid1.Max := zf.ImageCount;
    DrawGrid1.Invalidate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionUndoExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    zf.Undo(@Progress);
    DrawGrid1.Max := zf.ImageCount;
    DrawGrid1.Invalidate;
    SetMainImage(DrawGrid1.Position);
    EnableActions;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.ActionVertFlipExecute(Sender: TObject);
var
  rpos: Integer;
begin
  if DrawGrid1.Position >= 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      rpos := DrawGrid1.Position;
      zf.VerticalFlip(SelectedGridItems, @Progress);
      SetMainImage(DrawGrid1.Position);
      DrawGrid1.Position := rpos;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainFrm.btnCancelClick(Sender: TObject);
begin
  HideCropTool;
end;

procedure TMainFrm.btnCropClick(Sender: TObject);
var
  r : TRect;
  b : TBitmap;
begin
  Screen.Cursor := crHourGlass;
  try
    b := zf.Image[DrawGrid1.Position];
    r := Rect(shape1.Left, Shape1.Top, shape1.Left + Shape1.Width - 1, shape1.Top + Shape1.Height - 1);
    CropBitmap(b, r);
    zf.Image[DrawGrid1.Position] := b;
    SetMainImage(DrawGrid1.Position);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFrm.DrawGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aRow, ACol: Integer;
begin
  DrawGrid1.MouseToCell(X, Y, ACol, aRow);
  zf.Invert(DrawGrid1.Position, aRow, @Progress);
  DrawGrid1.Invalidate;
  SetMainImage(DrawGrid1.Position);
end;

procedure TMainFrm.DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aRow, ACol: Integer;
begin
  DrawGrid1.MouseToCell(X, Y, ACol, aRow);
  Accept := (Sender = Source) and (aRow <> DrawGrid1.Position) and
    (State <> dsDragEnter); // and (GetKeyState(VK_LBUTTON) and $8000 <> 0);
end;

procedure TMainFrm.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  b: TBitmap;
  X, Y: Integer;
  //function Apos:Integer;
  //begin
  //  result := ifthen(Fconfig.StampView = svVert, aRow, aCol);
  //end;
begin
  if zf.Mode <> zmClosed then
    with DrawGrid1.Canvas do
    begin
      if DrawGrid1.Selected[aRow] then
        Brush.Color := clHighlight
      else
        Brush.Color := clWhite;

      FillRect(aRect);

      //b := nil;
      b := zf.Stamp[aRow];
      if Assigned(b) then
      begin
        X := (DrawGrid1.DefaultColWidth - b.Width) div 2;
        Y := (DrawGrid1.DefaultRowHeight - b.Height) div 2;
        Draw(aRect.Left + X, aRect.Top + Y, b);
      end;

      // if gdFocused in State then
      if aRow = DrawGrid1.Position then
        DrawFocusRect(aRect);
    end;
end;

procedure TMainFrm.DrawGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (zf.Mode <> zmClosed) then
    with DrawGrid1 do
      case Key of
        40, // down
        37: // left
          if Position >= 0 then
          begin
            //Position := Position - 1;
            ClearSelection;
            Selected[Position] := true;
            SetMainImage(Position);
          end;

        38, // up
        39: //right
          if Position <= Max then
          begin
            //Position := Position + 1;
            ClearSelection;
            Selected[Position] := true;
            SetMainImage(Position);
          end;
      end;
end;

procedure TMainFrm.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, aRow, ACol: Integer;
begin
  if zf.Mode <> zmClosed then
    with DrawGrid1 do
    begin
      ACol:=0;
      aRow:=0;
      MouseToCell(X, Y, ACol, aRow);

      if (aRow >= 0) and (ACol >= 0) then
      begin
{$ifdef Darwin}
        if (Button = mbLeft) and (ssMeta in Shift) then
{$else}
        if (Button = mbLeft) and (ssCtrl in Shift) then
{$endif}
          Selected[aRow] := not Selected[aRow]
        else
        if (Button = mbLeft) and (ssShift in Shift) then
        begin
          ClearSelection;
          if aRow > FoldPos then
            for i := FoldPos to aRow do
              Selected[i] := not Selected[aRow]
          else
            for i := FoldPos downto aRow do
              Selected[i] := not Selected[aRow];
        end
        else if ((Button = mbRight) and not Selected[aRow]) or (Button = mbLeft)
        then
        begin
          if aRow <> Position then
            Position := aRow;
          ClearSelection;
          Selected[aRow] := True;
          Application.QueueAsyncCall(@AfterCellSelect, 0);
        end;
      end;

      Invalidate;
    end;
  EnableActions;
end;

procedure TMainFrm.DrawGrid1MouseEnter(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
    DrawGrid1.SetFocus;
end;

procedure TMainFrm.DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ShiftState: TShiftState;
begin
  if Application.Terminated then
    Exit;

  ShiftState := GetKeyShiftState;
  if not (ssShift in ShiftState) then
    FoldPos := aRow;

  CanSelect := zf.Mode <> zmClosed;
  if not CanSelect then
    Exit;

  EnableActions;
end;

procedure TMainFrm.mnuConfigClick(Sender: TObject);
begin
  with TConfigFrm.Create(Application) do
  try
    edtcwebp.Text:=Fconfig.cwebp;
    edtunrar.Text:=Fconfig.unrar;
    edtp7zip.Text:=Fconfig.p7zip;
    speNbThreads.Value:= FConfig.NbThreads;
    speQueues.Value:=FConfig.QueueSize;
    cblogging.Checked:=Fconfig.Blog;
    if ShowModal = mrOk then
    begin
      edtcwebp.Text:=Fconfig.cwebp;
      Fconfig.unrar := edtunrar.Text;
      Fconfig.p7zip := edtp7zip.Text;
      Fconfig.Blog := cblogging.Checked;
      FConfig.QueueSize := speQueues.Value;
      Fconfig.NbThreads:=speNbThreads.Value;
      SaveConfig;
      FThreadDataPool.SetPerfs(FConfig.NbThreads);
      if Length(FWorkerThreads) > 2 then
        ReduceConversionQueue(FConfig.QueueSize)
      else
      begin
        while FThreadDataPool.PoolSize < FConfig.QueueSize do
          FThreadDataPool.AddPool(FLog);
        CreateConversionQueues;
      end;

      SetAppCaption;
    end;
  finally
    free;
  end;
end;

function TMainFrm.CreatePath(const Path: String): TTreeNode;
var
  p, pp: string;
  lp: TStringArray;
  index : integer;
begin
  Result := nil;
  lp := Path.Split([PathDelim]);
  pp := '';
  for p in lp do
  begin
    if pp.Length = 0 then
      pp := p
    else
      pp := pp + PathDelim + p;

    index := FTreeViewPaths.IndexOf(pp);
    if index >= 0 then
      result := TTreeNode(FTreeViewPaths.Objects[index])
    else
    begin
      Result := TreeView1.Items.AddChild(Result, p);
      FTreeViewPaths.AddObject(pp, Result);
    end;
  end;
  Result := Result;
end;

function TMainFrm.GetNode(const FileName: string): TTreeNode;
var
  Path: string;
  index : integer;
begin
  Path := FileName;
  if not DirectoryExists(FileName) then
  begin
    Path := ExtractFilePath(FileName);
    Path := Path.TrimRight([PathDelim]);
  end;

  index := FTreeViewPaths.Indexof(Path);
  if index >= 0 then
    result := TTreeNode(FTreeViewPaths.Objects[index])
  else
  begin
    try
      Result := CreatePath(Path);
      ExpandParents(Result);
    except
    end;
  end;
end;

procedure TMainFrm.ExpandParents(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if not aNode.Expanded then
      aNode.Expand(True);
    aNode := aNode.Parent;
  end;
end;

function TMainFrm.FindNode(const FileName: String): TTreeNode;
var
  aNode: TTreeNode;
begin
  Result := GetNode(FileName);
  if Assigned(Result) then
  begin
    aNode := Result.getFirstChild;
    while Assigned(aNode) do
    begin
      if aNode.Path = FileName then
        Exit(aNode);

      aNode := Result.getNextChild(aNode);
    end;
  end;

  Result := nil;
end;

function TMainFrm.NodeSort(Node1, Node2 : TTreeNode): Integer;
begin
  Result := AnsiNaturalCompareStrings(Node1.Text, Node2.Text);
end;

procedure TMainFrm.AddFileToTree(const aFileName: string;
  IsNew: Boolean = False);
begin
  AddFileToTree2(aFileName, IsNew);

  if (GetTickCount64 - FLastUPdate) > 3000 then
  begin
    with TreeView1 do
    begin
      Application.ProcessMessages;
      FLastUPdate := GetTickCount64;
    end;
  end;
end;

function TMainFrm.AddFileToTree2(const aFileName: string;
                                 IsNew: Boolean = False): TTreeNode;
var
  arcType: TArcType;
  aNode: TTreeNode;
  cmt, fname: String;

  {
  function InOpList(const aFileName: String): Boolean;
  var
    i: Integer;
  begin
    result := false;

    with FOperationsList.LockList do
      try
        for i := 0 to count - 1 do
          if Items[i].FileName.Tolower.Equals(aFileName.Tolower) then
            Exit(True);
        Result := False;
      finally
        FOperationsList.UnlockList;
      end;
  end;
  }
begin
  if FileExists(aFileName) then
  begin
    Result := FindNode(aFileName);
    if Assigned(Result) then
      Exit;

    arcType := TCbz.GetArcType(aFileName);

    if (arcType <> arcZip) and (arcType <> arcUnknown) then
    begin
      //if FConfig.Moveoriginaltotrash then
      //  FJobpool.AddJob(aFileName, arcType, [opConvert, opDeleteFile])
      //else
        FJobpool.AddJob(aFileName, arcType);
    end
    else if (arcType = arcZip) and not FJobpool.FileInQueue(aFileName) then
      //not InOpList(aFileName) then
    begin
      with TZipFile.Create do
        try
          try
            Filename := aFileName;
            Active := True;
            try
              if FileCount > 0 then
              begin
                cmt := Comment;
                if Length(cmt) >= Length(CbzComment) then
                  cmt := Copy(cmt, 1, Length(CbzComment));

                fname := ExtractFileName(FileNames[0]);
                if (cmt <> CbzComment) or
                  (LowerCase(ExtractFileExt(fname)) <> '.webp') or
                  (fname <> format(format('%%.%dd', [IntToStr(FileCount).Length]
                  ) + '.webp', [1])) then
                begin
                  //if FConfig.Moveoriginaltotrash then
                  //  FJobpool.AddJob(aFileName, arcZip,
                  //    [opConvert, opDeleteFile])
                  //else
                    FJobpool.AddJob(aFileName);
                  Exit;
                end;
              end;
            except
            end;

            aNode := GetNode(aFileName);
            Result := FindNode(aFileName);
            if not Assigned(Result) then
            begin
              Result := TreeView1.Items.AddChild(aNode,
                ExtractFileName(aFileName));
              Result.Data := Pointer(Integer(IsNew));
              if IsNew then
                TreeView1.CustomSort(@NodeSort);
            end;
            ExpandParents(Result.Parent);

          except
            on e: Exception do
            begin
              FLog.Log('Error reading file :' + e.Message);
              FIgnores.Add(aFileName);
              Exit;
            end;
          end;
        finally
          Free;
        end;
    end
    else if Assigned(FIgnores) and not FJobpool.FileInQueue(aFileName) then
    //and not InOpList(aFileName) then
      FIgnores.Add(aFileName);
  end
  else
    Result := GetNode(aFileName);
end;


procedure TMainFrm.SearchEnded(Sender: TObject);
begin
  if not FClosing then
  begin
    with TreeView1 do
    begin
      ActionChoosefolder.Enabled := True;
      if Items.count > 0 then
      begin
        Items[0].Expand(True);
        Selected := Items[0];
        Selected.MakeVisible;
      end;
      TreeView1.CustomSort(@NodeSort);
      Items.EndUpdate;
      Enabled := True;
      Refresh;
    end;

    if FIgnores.count > 0 then
      try
        with TfrmReport.Create(Application, FIgnores) do
          try
            ShowModal;
          finally
            Free;
          end;
      finally
        FIgnores.Clear;
      end;
  end;

  FInFill := False;
  FThreadSearchFiles := nil;
end;

procedure TMainFrm.FillTreeView(const Path: String);
begin
  if FInFill then
    Exit;

  FInFill := True;
  ActionChooseFolder.Enabled := False;
  FTreeViewPaths.Clear;
  //FLastUPdate := GetTickCount;
  //MemoReport.Lines.Clear;
  with TreeView1 do
  begin
    Enabled := False;
    Items.BeginUpdate;
    FIgnores.Clear;
    Items.Clear;
    zf.Close;
  end;

  FThreadSearchFiles := ThreadedSearchFiles(Path, '*.cbz;*.cbr;*.zip;*.rar;*.pdf', @AddFileToTree2, @SearchEnded,
                                            @Progress, //str_scanning
                                            'scanning : ', [sfoRecurse, sfoFolders]);
end;


function TMainFrm.GetProgress(ProgressID: QWord; Sender: TObject):TProgressRec;
var
  index : integer;
begin
  if Assigned(FProgress) then
  begin
    index := FProgress.IndexOf(IntToStr(ProgressID));
    if index >= 0 then
      result := TProgressRec(FProgress.Objects[Index])
    else
    begin
      //LockWindowUpdate(Handle);
      try
        result := TProgressRec.Create;
        with Result do
        begin
          FPanel := TPanel.Create(Self);
          with FPanel do
          begin
            DoubleBuffered := True;
            Align := alBottom;
            Height := 30; //FprogressHeight;
            Parent := Self;
            Top := 0;
            BevelInner := bvNone;
            BevelOuter := bvNone;
            Visible := True;
          end;
          FLabel := TPanel.Create(Self);
          with FLabel do
          begin
            DoubleBuffered := True;
            Alignment := taLeftJustify;
            Align := alClient;
            BevelInner := bvNone;
            BevelOuter := bvNone;
            Parent := FPanel;
          end;
          FProgressBar := TProgressBar.Create(Self);
          with FProgressBar do
          begin
            DoubleBuffered := True;
            Align := alRight;
            Parent := FPanel;
            Width := Self.ClientWidth div 4;
          end;

          if (Sender is TCbzWorkerThread) {or (Sender is TThreadDoOperations)} then
          begin
            FButton := TButton.Create(Self);
            with FButton do
            begin
              Caption := 'Cancel';
              Align := alRight;
              Left := FProgressBar.Left + FProgressBar.Width + 10;
              Width := Canvas.TextWidth(Caption) + 20;
              Parent := FPanel;
              if (Sender is TCbzWorkerThread) then
                OnClick := @TCbzWorkerThread(Sender).Cancel;
              //else if (Sender is TThreadDoOperations) then
              //  OnClick := TThreadDoOperations(Sender).Cancel
            end;
          end;

          result.FPanel.Parent := pnlProgress;
          FProgress.AddObject(IntToStr(ProgressID), Result);
        end;
      finally
        //LockWindowUpdate(0);
      end;
    end;
  end;
end;

procedure TMainFrm.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String = '');
var
  r: TProgressRec;
  index : integer;
begin
  if (aPos = 0) and (aMax = 0) then
  begin
    index := FProgress.IndexOf(IntToStr(ProgressID));
    if index >= 0 then
    begin
      r := TProgressRec(FProgress.Objects[index]);
      r.FPanel.Free;
      r.Free;
      FProgress.Delete(index);
    end;
    Exit;
  end;

  r := GetProgress(ProgressID, Sender);

  pnlProgress.Height := FProgress.Count * 30;

  if Assigned(r.FLabel) then
    with r.FLabel do
    begin
      if Caption <> Msg then
        Caption := Msg;
      Refresh;
    end;

  if Assigned(r.FProgressBar) then
    with r, FProgressBar do
    begin
      if Max <> aMax then
      begin
        Max := aMax;
        if (aPos = 0) and (aMax = 1) and Assigned(FButton) then
          FButton.Enabled := True;
      end;
      if Position <> aPos then
        Position := aPos;
      Refresh;
    end;
end;


{ TTreeNode }

function TTreeNodeEx.GetPath: String;
var
  n: TTreeNode;
begin
  n := Self;
  Result := '';
  while Assigned(n) do
  begin
    if Result = '' then
      Result := Text
    else
      Result := n.Text + PathDelim + Result;
    n := n.Parent;
  end;
end;

end.

