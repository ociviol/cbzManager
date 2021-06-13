unit main;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ExtCtrls, ActnList, uCbz, Utils.treeview,
{$ifdef Darwin}
  OpenSslSockets,
{$endif}
  Utils.Logger, Utils.SearchFiles, Utils.Gridhelper,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  Utils.Arrays, uCbzViewerFrame,
  uDataPool, uWorkerThread, uConfig, uCbzLibrary;

type
  TProgressRec = class
    FPanel, FLabel: TPanel;
    FProgressBar: TProgressBar;
    FButton: TButton;
  end;

  { TMainFrm }
  TMainFrm = class(TForm)
    ActionNewFolder: TAction;
    ActionDelete: TAction;
    ActionCopyToLib: TAction;
    ActionReadLog: TAction;
    ActionLibrary: TAction;
    ActionFileCleaner: TAction;
    ActionShowStats: TAction;
    ActionRename: TAction;
    ActionJoin: TAction;
    ActionRefresh: TAction;
    ActionChooseFolder: TAction;
    ActionList1: TActionList;
    Addtoqueue1: TMenuItem;
    Label5: TLabel;
    lbConvThreads: TListBox;
    lblFilesInQueue: TLabel;
    lbQueue: TListBox;
    MainMenu1: TMainMenu;
    memoLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    N14: TMenuItem;
    N13: TMenuItem;
    N12: TMenuItem;
    N11: TMenuItem;
    N10: TMenuItem;
    N9: TMenuItem;
    MenuItem2: TMenuItem;
    mnuAbout: TMenuItem;
    mnuConfig: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    mnuSetDefaultPath: TMenuItem;
    mnuChooseFolder: TMenuItem;
    mnuExit: TMenuItem;
    mnuFile: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlProgress: TPanel;
    pnlStats: TPanel;
    pmTreeView: TPopupMenu;
    PopupMenuQueue: TPopupMenu;
    Removefromlist1: TMenuItem;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    Timerstats: TTimer;
    TreeView1: TTreeView;

    procedure ActionChooseFolderExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionFileCleanerExecute(Sender: TObject);
    procedure ActionLibraryExecute(Sender: TObject);
    procedure ActionCopyToLibExecute(Sender: TObject);
    procedure ActionNewFolderExecute(Sender: TObject);
    procedure ActionReadLogExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure ActionShowStatsExecute(Sender: TObject);
    procedure Addtoqueue1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSetDefaultPathClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure pmTreeViewPopup(Sender: TObject);
    procedure PopupMenuQueuePopup(Sender: TObject);
    procedure Removefromlist1Click(Sender: TObject);
    procedure TimerstatsTimer(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FLog: ILog;
    FInFill,
    FLibDocked: Boolean;
    FConfigFile : String;
    FConfig : TConfig;
    FThreadSearchFiles : TThread;
    FTreeViewPaths : TStringlist;
    FClosing : Boolean;
    FProgress: TStringList;
    FThreadDataPool: TThreadDataPool;
    FJobpool : TJobPool;
    FWorkerThreads: Array of TCbzWorkerThread;
    FConvertReport : TstringList;
    FLastUPdate : TDateTime;
    Fignores : TStringlist;
    CbzViewerFrame : TCbzViewerFrame;

    function SelectionValid:boolean;
    //procedure CheckAlbumArt(const aFilename : string);
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
    //procedure ReduceConversionQueue(NewSize: Integer);
    procedure OnBadFile(Sender: TObject);
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    function GetProgress(ProgressID: QWord; Sender: TObject):TProgressRec;
    procedure SetAppCaption;
    procedure EnableActions;
    procedure OpenFileForview(aNode : TTreeNode);

    procedure CheckVersionTerminate(Sender : TObject);
  public
  end;

  { TThreadCheckVersion }

  TThreadCheckVersion = Class(TThread)
  protected
    FNeedUpdate : Boolean;
    FUpdateVersion : String;
    Flog : ILog;
  public
    constructor Create(Log : ILog; aTerminate : TNotifyEvent);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TDrawGridAccess }

  TDrawGridAccess = Class(TDrawGrid);

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  Config, LclIntf, LazUTF8,
  frmwait,
  fpHttpClient, uLogReader,
{$if defined(Darwin) or defined(Linux)}
  unix,
{$endif}
  Utils.Vcl, Math,
  Utils.SoftwareVersion, uDataTypes,
  Utils.ZipFile, Utils.Files,
  Utils.NaturalSortStringList,
  uLoadReport, uAbout, uFileCleaner;


{ TThreadCheckVersion }

constructor TThreadCheckVersion.Create(Log : ILog; aTerminate: TNotifyEvent);
begin
  Flog := Log;
  FNeedUpdate := false;
  OnTerminate := aTerminate;
  FreeOnTerminate:=True;
  inherited Create(false);
end;

destructor TThreadCheckVersion.Destroy;
begin
  Flog := nil;
  inherited Destroy;
end;

procedure TThreadCheckVersion.Execute;
var
  t : TStringList;
  i : integer;
begin
  try
    with TFPHTTPClient.Create(nil) do
    try
      t := TStringList.Create;
      try
        SimpleGet('https://filedn.com/ld6vdapF8EELCrRa1IVpQwu/version.txt', t);

        for i:=0 to t.Count - 1 do
  {$if defined(Darwin)}
          if t[i].StartsWith('osx:') then
          begin
            FUpdateVersion := copy(t[i], 5, length(t[i]));
            break;
          end;
  {$ELSEif Defined(Linux)}
          if t[i].StartsWith('linux:') then
          begin
            FUpdateVersion := copy(t[i], 7, length(t[i]));
            break;
          end;
  {$ELSEif Defined(MsWindows)}
          if t[i].StartsWith('winos:') then
          begin
            FUpdateVersion := copy(t[i], 7, length(t[i]));
            break;
          end;
  {$ENDIF}
          if FUpdateVersion <> '' then
            FNeedUpdate := CompareVersion(GetFileVersion, FUpdateVersion) > 0;
      finally
        t.Free;
      end;
    finally
      Free;
    end;
  except
    on e: Exception do
      Flog.Log('TThreadCheckVersion.Execute : Error : ' + E.Message);
  end;

  Terminate;
end;


{ TMainFrm }
{
procedure test;
var
  c : char;
  Files : TStringList;
  s, ss : string;
  su : UnicodeString;
  ar : TCharArray;
  i, j : integer;
begin
  Files := TStringlist.Create;
  with Files do
  try

    s := 'ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÇçÌÍÎÏìíîïÙÚÛÜùúûüÿÑñ';
    Files.CLear;
    Files.Add(s);

    ar := s.ToCharArray;
    ss := '';
    for c in ar do
    begin
      if c > #127 then
        ss := ss + IntToHex(ord(c), 2) + ',';
      if c < #128 then
      begin
        if ss <> '' then
          Files.Add(ss);
        ss := c + ' = ';
      end;
    end;

    Files.SaveToFile('c:\temp\f2.txt');
  finally
    Free;
  end;
end;
 }
procedure TMainFrm.FormCreate(Sender: TObject);
begin
  //test;
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
  FConfigFile := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'config.json';
{$endif}
  // load config
  FConfig := TConfig.Load(FConfigFile);
  FConfig.RestoreForm(Self);

  if FConfig.MngrTreeViewWidth <> 0 then
    Panel2.Width := ifthen(FConfig.MngrTreeViewWidth > (ClientWidth div 2),
                           ClientWidth div 3, FConfig.MngrTreeViewWidth);

  FLibDocked := False;
  pnlStats.Visible := FConfig.ShowStats;
  MenuItem35.Checked := FConfig.ShowStats;
  Timerstats.Enabled:=FConfig.ShowStats;
  // start logger

  FLog := GetILog(
{$if defined(Darwin) or defined(Linux)}
    expandfilename('~/') + CS_CONFIG_PATH + '/' +
{$else}
    IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Logs\' +
{$endif}
    'cbzManager.log', FConfig.DoLog);

  Flog.Log('cbzManager started.');

  CbzViewerFrame := TCbzViewerFrame.Create(self, FConfig, FLog, @Progress);
  CbzViewerFrame.Parent := self;

  //FConfig.QueueSize := CPUCount;

  FTreeViewPaths := TStringlist.Create;
  FProgress := TStringList.Create;
  FIgnores := Tstringlist.Create;

  // check required programs
  if not CheckPrograms then
    exit;

  FJobpool := TJobPool.Create(FLog);

  FConvertReport := TstringList.Create;

  FThreadDataPool := TThreadDataPool.Create(FConfig.QueueSize,
                                            FLog, FConfig.NbThreads,
                                            @FConfig.WebpQuality);
  CreateConversionQueues;
  //
  if DirectoryExists(FConfig.BdPathPath) then
    FillTreeView(FConfig.BdPathPath);

  SetAppCaption;
  TThreadCheckVersion.Create(FLog, @CheckVersionTerminate);
end;

procedure TMainFrm.CheckVersionTerminate(Sender : TObject);
begin
  if TThreadCheckVersion(Sender).FNeedUpdate then
    if MessageDlg('A new version is available, do you want to download the update ?',
                  mtConfirmation, MbYesNo, 0) = MrYes then
    begin
{$if defined(Darwin)}
      OpenUrl('https://filedn.com/ld6vdapF8EELCrRa1IVpQwu/cbzManager/OpenSource/cbzmanagerOsX.zip');
{$elseif defined(Linux)}
      OpenUrl('https://filedn.com/ld6vdapF8EELCrRa1IVpQwu/cbzManager/OpenSource/cbzmanagerLinux.zip');
{$else}
      OpenUrl('https://filedn.com/ld6vdapF8EELCrRa1IVpQwu/cbzManager/OpenSource/cbzmanagerWin.zip');
{$endif}
    end;
end;

procedure TMainFrm.SetAppCaption;
begin
  Caption := GetFileVersionInternalName + ' (' + GetFileVersion + ')';
  Caption := format('%s - Path : %s - %d Queues - %d Encoding threads - Temp: %s',
                    [Caption, Fconfig.BdPathPath,
                    FThreadDataPool.PoolSize,
                    FThreadDataPool.NbWorkers,
                    GetTempDir(True)]);
end;

procedure TMainFrm.EnableActions;
begin
  try
    // files
    with TreeView1 do
      ActionDelete.Enabled:=
        Assigned(Selected) and
        (FileExists(Selected.Path) or DirectoryExists(Selected.Path)) and
        SelectionValid;

    ActionCopyToLib.Enabled := (Treeview1.SelectionCount > 0) and
                               Assigned(FindForm(TCbzLibrary)) and
                               SelectionValid;

    ActionChooseFolder.Enabled := not FInFill;
    ActionNewFolder.Enabled:= (TreeView1.SelectionCount = 1) and DirectoryExists(TreeView1.Selected.Path);
    ActionRename.Enabled := Assigned(TreeView1.Selected) and
                               (TreeView1.SelectionCount = 1) and
                               SelectionValid;
    ActionRefresh.Enabled := (FConfig.BdPathPath.Length > 0) and not FInFill;
  except
  end;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FTreeViewPaths.Free;
  FProgress.Free;
  FIgnores.Free;
  FConvertReport.Free;
  FConfig.Free;

  Flog.Log('cbzManager destroyed.');
  // destroy logger
  Flog := nil;
end;

procedure TMainFrm.FormResize(Sender: TObject);
begin
  FConfig.SaveForm(Self);
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  if not Application.Terminated then
    EnableActions;
  if FConfig.OpenLibrary then
    ActionLibrary.Execute;
end;

procedure TMainFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);

  procedure SetCursor(aCursor : TCursor);
  var
    j : integer;
  begin
    for j := 0 to ComponentCount - 1 do
      if Components[j] is TControl then
        TControl(Components[j]).Cursor := aCursor;
  end;

var
  i : Integer;
  f : TFormWait;
begin
  SetCursor(crHourglass);
  Update;
  Refresh;
  FClosing := True;
  f := TFormWait.Create(self);
  with f do
  begin
    Show;
    Update;
    Refresh;
    Panel1.Update;
    Panel1.Refresh;
    Application.ProcessMessages;
    ThreadSwitch;
  end;

  f.Text := 'Stopping Search files thread ...';
  if Assigned(FThreadSearchFiles) then
    FThreadSearchFiles.Terminate;

  FConfig.OpenLibrary:=Assigned(FindForm(TCbzLibrary));
  if Assigned(FindForm(TCbzLibrary)) then
  begin
    f.Text := 'Closing Library ...';
    FindForm(TCbzLibrary).Close;
  end;

  f.Text := 'Stopping Data Pools ...';
  FThreadDataPool.Stop;

  f.Text := 'Stopping Worker threads ...';
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

  f.Text := 'Closing Application ...';
  FThreadDataPool.Free;
  FJobpool.Free;
  SaveConfig;
  CloseAction:=caFree;;
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

(*
procedure TMainFrm.ReduceConversionQueue(NewSize: Integer);
var
  aFile: string;
  aType: TArcType;
begin
{
  while Length(FWorkerThreads) > NewSize do
  begin
    aFile := '';
    aType := arcUnknown;
  }
    FThreadDataPool.Pool[Length(FWorkerThreads) - 1].Disable;
    {
    with FWorkerThreads[Length(FWorkerThreads) - 1] do
    begin
      if Assigned(CurJob) and (CurJob.Status = jsProcessing) then
      begin
        aFile := CurJob.FileName;
        aType := CurJob.arcType;
      end;
      Terminate;
      WaitFor;
      //Free;
    end;

    FThreadDataPool.RemovePool(length(FWorkerThreads) - 1);
    FWorkerThreads[Length(FWorkerThreads) - 1].Free;
    SetLength(FWorkerThreads, Length(FWorkerThreads) - 1);
    if aFile <> '' then
      if FConfig.DeleteFile then
        FJobpool.AddJob(aFile, aType, [opConvert, opDeleteFile])
      else
        FJobpool.AddJob(aFile, aType);
  end;
  }
end;
*)

procedure TMainFrm.OnBadFile(Sender: TObject);
begin
  memoLog.Lines.Assign(FConvertReport);
  memoLog.Visible := True;
end;

procedure TMainFrm.mnuAboutClick(Sender: TObject);
begin
  with TfrmAbout.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainFrm.SaveConfig;
begin
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
    msg := msg + 'cwebp not found, ' +
{$if defined(Darwin)}
          'install using ''brew install webp''' + #13;
{$elseif defined(Linux)}
           'install using ''sudo apt install webp''' + #13;
{$else}
   #13;
{$endif}
  Flog.Log('required binary missing : cwebp.');
  end;

  if not FileExists(Fconfig.unrar) then
  begin
    msg := msg + 'unrar not found, ' +
{$if defined(Darwin)}
              'install using ''brew install unrar''' + #13;
{$elseif defined(Linux)}
               'install using ''sudo apt install unrar''' + #13;
{$else}
   #13;
{$endif}
    Flog.Log('required binary missing : unrar.');
  end;
  if not FileExists(Fconfig.p7zip) then
  begin
    msg := msg + '7z not found, ' +
{$if defined(Darwin)}
              'install using ''brew install p7zip''' + #13;
{$elseif defined(Linux)}
               'install using ''sudo apt install p7zip-full''' + #13;
{$else}
   #13;
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
    //Application.Terminate;
    //Halt;
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

procedure TMainFrm.Panel2Resize(Sender: TObject);
begin
  FConfig.MngrTreeViewWidth := Panel2.Width;
end;

procedure TMainFrm.pmTreeViewPopup(Sender: TObject);
begin
  EnableActions;
end;

procedure TMainFrm.PopupMenuQueuePopup(Sender: TObject);
begin
  with lbQueue do
  begin
    Removefromlist1.Enabled := ItemIndex >= 0;
    if ItemIndex >= 0 then
      Removefromlist1.Enabled := FJobpool.JobStatus[ItemIndex] = jsWaiting;
  end;
end;

procedure TMainFrm.Removefromlist1Click(Sender: TObject);
begin
  with lbQueue do
    if ItemIndex >= 0 then
      FJobpool.DeleteJob(Items[ItemIndex]);
end;

procedure TMainFrm.TimerstatsTimer(Sender: TObject);
var
  i, nbZ, nbR, nbP, nbIn, nbOut: Integer;
  lst: TStringlist;
begin
  FJobpool.Stats(nbZ, nbR, nbP);
  lblFilesInQueue.Caption := format('Files to process : Zip:%d   Rar:%d', [nbZ, nbR]); //    Pdf:%d', [nbZ, nbR, nbP]);
  with lbQueue, Items do
  begin
    BeginUpdate;
    try
      if count <> FJobpool.count then
      begin
        Clear;
        lst := FJobpool.JobFileNames;
        try
          Items.Assign(lst);
        finally
          lst.Free;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  with lbConvThreads.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to FThreadDataPool.PoolSize - 1 do
      begin
        FThreadDataPool.Pool[i].Stats(nbIn, nbOut);
        Add(format('Thread %d - Images awaiting encoding :%d', [i + 1, nbIn]));
      end;
    finally
      EndUpdate;
    end;
  end;
end;
   {
procedure TMainFrm.CheckAlbumArt(const aFilename: string);
var
  b : TBitmap;
  p : TPicture;
  s1,s2 : string;
begin
  s1 := ChangeFileExt(aFilename, '.bmp');
  s2 := ChangeFileExt(aFilename, '.jpg');
  if not FileExists(s2) then
  try
    with TCbz.Create(FLog, DrawGrid1.DefaultColWidth - 5,
                     DrawGrid1.DefaultRowHeight - 5) do
    try
      Open(aFileName, zmRead);
      b := GenerateStamp(0);
      try
        b.SaveToFile(s1);
        try
          p := TPicture.Create;
          Try
            p.LoadFromFile(s1);
            P.SaveToFile(s2, '.jpg');
          finally
            p.Free;
          end;
        finally
          if Sysutils.FileExists(s1) then
            Sysutils.DeleteFile(s1);
        end;
      finally
        b.free;
      end;
    finally
      Free;
    end;
  except
    on e: exception do
      FLog.Log('Error generating album art :' + e.Message);
  end;
end;
  }
procedure TMainFrm.OpenFileForview(aNode : TTreeNode);
begin
  aNode.Data := nil;
  TreeView1.Refresh;
  CbzViewerFrame.Filename:= aNode.Path;
end;

procedure TMainFrm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if TreeView1.SelectionCount = 1 then
    if Assigned(Node) and not Node.HasChildren then
    begin
      if FileExists(Node.Path) then
      begin
        CbzViewerFrame.Filename:=Node.Path;
        try
          OpenFileForview(Node);
        except
        end;
      end;
    end
    else
    begin
      CbzViewerFrame.Clear;
      CbzViewerFrame.ClearUndo;
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

procedure TMainFrm.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Src, Dst : TTreeNode;
begin
  Src := TreeView1.Selected;
  if Sender is TTreeView then
  begin
    Dst := TreeView1.GetNodeAt(X, Y);
    Accept := (Src.Level > 0) and not Src.HasChildren and Assigned(Dst) and
      ((Src <> Dst) and DirectoryExists(Dst.Path)) and
    // Dst.Path.ToLower.Contains(FPath.ToLower) and
      (Assigned(Src.Parent) and (Src.Parent <> Dst));
  end
  else
  if Sender is TDrawGrid then
    Accept := TDrawGrid(Sender).Owner = FindForm(TcbzLibrary)
  else
    Accept := false;
end;

procedure TMainFrm.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Src, Dst: TTreeNode;
  DstFile: String;
begin
  Src := TreeView1.Selected;
  Dst := TreeView1.GetNodeAt(X, Y);

  CbzViewerFrame.FullClear;
  DstFile := IncludeTrailingPathDelimiter(Dst.Path) +
                ExtractFileName(Src.Path);
  RenameFile(Src.Path, DstFile);
  TreeView1.Items.Delete(Src);
  TreeView1.Selected := AddFileToTree2(DstFile);
end;

function TMainFrm.SelectionValid:boolean;
var
  i : integer;
begin
  with TreeView1 do
    for i := 0 to SelectionCount - 1 do
      if not Selections[i].Path.Contains(FConfig.BdPathPath) then
        exit(false);
  result := true;
end;

procedure TMainFrm.ActionDeleteExecute(Sender: TObject);
var
  n : TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
    if FileExists(TreeView1.Selected.Path) then
      with TreeView1 do
      begin
        n := Selected;
        Selected := n.Parent;
        CbzViewerFrame.FullClear;
        DeleteFile(n.path);
        Items.Delete(n);
      end
    else
    if DirectoryExists(TreeView1.Selected.Path) then
    begin
      if FileCount(TreeView1.Selected.Path, ['*.cbz']) > 0 then
        if MessageDlg('Delete folder', 'Are your sure you want to delete a folder ?',
                      mtInformation, mbYesNo, 0) = mrno then
          Exit;

      with TreeView1 do
      begin
        n := Selected;
        Selected := n.Parent;
        KillFolder(n.Path);
        RemoveDir(n.Path);
        Items.Delete(n);
      end;
    end;
end;

procedure TMainFrm.ActionFileCleanerExecute(Sender: TObject);
begin
  with TTFormFilenameCleanerConfig.Create(Application) do
    try
      ShowModal;
    finally
      Free;
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

procedure TMainFrm.ActionLibraryExecute(Sender: TObject);
begin
  if ((FConfig.LibPath = '') or (not DirectoryExists(FConfig.LibPath))) or
     ((FConfig.SyncPath = '') or (not DirectoryExists(FConfig.SyncPath))) then
     ShowMessage('Lib Path and Sync Path must be set in config before opening the Library')
  else

  if Assigned(FindForm(TCbzLibrary)) then
  begin
    with FindForm(TCbzLibrary) do
    begin
      Show;
      BringToFront
    end;
  end
  else
  with TCbzLibrary.Create(nil, FConfig) do
  begin
    if FLibDocked then
    begin
      Align := alRight;
      Parent := Self;
    end;
    Show;
    BringToFront;
  end;
end;

procedure TMainFrm.ActionCopyToLibExecute(Sender: TObject);
var
  Files : TStringlist;
  Sel : TList;
  s,p : string;
  i : integer;
begin
  if Assigned(FindForm(TCbzLibrary)) and Assigned(TreeView1.Selected) then

  Screen.Cursor:=crHourglass;
  Sel := TList.Create;
  try
    for i := 0 to TreeView1.SelectionCount - 1 do
      Sel.Add(TreeView1.Selections[i]);

    for i := 0 to Sel.Count - 1 do
    begin
      s := TTreeNode(Sel[i]).Path;
      if FileExists(s) then
      begin
        if Utf8CompareStr(CbzViewerFrame.Filename, s) = 0 then
          TCbzLibrary(FindForm(TCbzLibrary)).ImportFile(s, '', CbzViewerFrame.Cbz)
        else
          TCbzLibrary(FindForm(TCbzLibrary)).ImportFile(s, '');
      end
      else
      if DirectoryExists(s) then
      begin
         Files := TStringlist.Create;
         try
           GetFiles(s, ['*.cbz'], Files);
           for s in Files do
           begin
             p := ExtractFilePath(s.Replace(FConfig.BdPathPath, ''));
             p := ExcludeTrailingPathDelimiter(ExcludeLeadingPathDelimiter(p));
             if Utf8CompareStr(CbzViewerFrame.Filename, s) = 0 then
               TCbzLibrary(FindForm(TCbzLibrary)).ImportFile(s, p, CbzViewerFrame.Cbz)
             else
               TCbzLibrary(FindForm(TCbzLibrary)).ImportFile(s, p);
           end;
         finally
           Files.Free;
         end;
      end;
    end;
  finally
    Sel.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TMainFrm.ActionNewFolderExecute(Sender: TObject);
var
  new : string;
begin
  repeat
    new := IncludeTrailingPathDelimiter(TreeView1.Selected.Path) +
           InputBox('Create Folder', 'Input name', '');
    if DirectoryExists(new) then
      ShowMessage('Folder "' + new + '" already exists !');
  until (new = '') or not DirectoryExists(new);
  if (new <> '') then
  begin
    TreeView1.Items.AddChild(TreeView1.Selected, ExtractFileName(new));
    CreateDir(new);
  end;
end;

procedure TMainFrm.ActionReadLogExecute(Sender: TObject);
begin
  with TFrmLogReader.Create(Application, FLog) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainFrm.ActionRefreshExecute(Sender: TObject);
begin
  FillTreeView(FConfig.BdPathPath);
end;

procedure TMainFrm.ActionRenameExecute(Sender: TObject);
var
  old, new, p, v : string;
  done : boolean;
begin
  old := TreeView1.Selected.Path;
  if FileExists(old) then
  begin
    v := extractfilename(old);
    p := ExtractFilePath(old);

    done := InputQuery('Rename File', 'Input new Filename', v);
    new := IncludeTrailingPathDelimiter(p) + v;
    if done then
      if FileExists(new) then
        ShowMessage('File "' + new + '" already exists !')
      else
      begin
        CbzViewerFrame.Clear;
        TreeView1.Selected.Text := ExtractFileName(new);
        RenameFile(old, new);
      end;

  end
  else
  if DirectoryExists(old) then
  begin
    p := extractfilepath(old);
    v := GetLastPath(old);

    done := InputQuery('Rename Folder', 'Input new Foldername', v);
    new := IncludeTrailingPathDelimiter(p) + v;

    if done then
      if DirectoryExists(new) then
        ShowMessage('Folder "' + new + '" already exists !')
      else
        if RenameFile(old, new) then
          TreeView1.Selected.Text := ExtractFileName(new);

  end;
end;

procedure TMainFrm.ActionShowStatsExecute(Sender: TObject);
begin
  FConfig.ShowStats := not FConfig.ShowStats;
  pnlStats.Visible:=FConfig.ShowStats;
  Timerstats.Enabled:=FConfig.ShowStats;
  FConfig.Save(FConfigFile);
  Flog.Log('Config saved.');
end;

procedure TMainFrm.Addtoqueue1Click(Sender: TObject);
var
  arcType: TArcType;
begin
  with TOpenDialog.Create(Application) do
  begin
    Filter := 'Cbz files|*.cbz|Zip files|*.zip|Rar files|*.rar|Cbr files|*.cbr|Pdf files|*.pdf';
    if Execute then
    begin
      arcType := TCbz.GetArcType(FileName);
      if arcType <> arcUnknown then
        FJobpool.AddJob(FileName, arcPdf)
    end;
  end;
end;


procedure TMainFrm.mnuConfigClick(Sender: TObject);
begin
  with TConfigFrm.Create(Application) do
  try
    Config := FConfig;
    if ShowModal = mrOk then
    begin
      SaveConfig;
      // log
      FLog.SetActive(Fconfig.DoLog, Self);
      // set perfs
      FThreadDataPool.SetPerfs(FConfig.NbThreads);
      // set nb queues
      if Length(FWorkerThreads) < FConfig.QueueSize then
      //  ReduceConversionQueue(FConfig.QueueSize)
      //else
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
  Result := NaturalCompare(Node1.Text, Node2.Text);
  //Result := NaturalCompareText(Node1.Text, Node2.Text);
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
      if FConfig.DeleteFile then
        FJobpool.AddJob(aFileName, arcType, [opConvert, opDeleteFile])
      else
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

      //if FConfig.DoAlbumart then
      //  CheckAlbumArt(aFilename);
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
  EnableActions;
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
    CbzViewerFrame.FullClear;
  end;

  FThreadSearchFiles := ThreadedSearchFiles(Path, ['*.cbz','*.cbr','*.zip','*.rar'], @AddFileToTree2, @SearchEnded,
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

          if (Sender is TCancellableThread) then
          begin
            FButton := TButton.Create(Self);
            with FButton do
            begin
              Caption := 'Cancel';
              Align := alRight;
              Left := FProgressBar.Left + FProgressBar.Width + 10;
              Width := Canvas.TextWidth(Caption) + 20;
              Parent := FPanel;
              if (Sender is TCancellableThread) then
                OnClick := @TCancellableThread(Sender).Cancel;
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


end.

