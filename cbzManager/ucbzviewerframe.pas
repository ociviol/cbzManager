unit uCbzViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Grids, StdCtrls, Spin, Menus,
  ActnList, uCbz,
  {$if defined(Linux) or defined(Darwin)}
    cthreads,
  {$endif}
  utils.Logger, Utils.Gridhelper, utils.Arrays, Types, graphics,
  Utils.Graphics, uconfig, utils.zipfile, uDataTypes;

type
  TViewMode = (vmRead, vmModify);
  { TCbzViewerFrame }

  TCbzViewerFrame = class(TFrame)
    ActionAppendFile: TAction;
    ActionRewriteManga: TAction;
    ActionCropTool: TAction;
    ActionDelete: TAction;
    ActionFirst: TAction;
    ActionHorizFlip: TAction;
    ActionJoin: TAction;
    ActionLast: TAction;
    alViewerFrame: TActionList;
    ActionMoveDown: TAction;
    ActionMoveToBottom: TAction;
    ActionMoveToTop: TAction;
    ActionMoveup: TAction;
    ActionRot90: TAction;
    ActionRotm90: TAction;
    ActionSelectAll: TAction;
    ActionSplitImage: TAction;
    ActionUndo: TAction;
    ActionUndoAll: TAction;
    ActionVertFlip: TAction;
    btnCancel: TButton;
    btnCrop: TButton;
    btnFirst: TButton;
    btnHorizFlip: TButton;
    btnLast: TButton;
    btnRotateLeft: TButton;
    btnRotateRight: TButton;
    btnVertFlip: TButton;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    memoLog: TMemo;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
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
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    PanelCrop: TPanel;
    pnlimgName: TPanel;
    pmgrid: TPopupMenu;
    pmImage: TPopupMenu;
    Shape1: TShape;
    speBottom: TSpinEdit;
    speLeft: TSpinEdit;
    speRight: TSpinEdit;
    speTop: TSpinEdit;

    procedure ActionAppendFileExecute(Sender: TObject);
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
    procedure ActionRewriteMangaExecute(Sender: TObject);
    procedure ActionRot90Execute(Sender: TObject);
    procedure ActionRotm90Execute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
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
    procedure pmgridPopup(Sender: TObject);
    procedure pmImagePopup(Sender: TObject);
    procedure speBottomChange(Sender: TObject);
    procedure speLeftChange(Sender: TObject);
    procedure speRightChange(Sender: TObject);
    procedure speTopChange(Sender: TObject);
  private
    FConfig : TConfig;
    FLog : ILog;
    //FViewMode: TViewMode;
    zf : TCbz;
    FFilename : String;
    FoldPos: Integer;
    FProgress : TCbzProgressEvent;

    function GetStampHeight: Integer;
    function GetStampWidth: Integer;
    function GetState: TZipMode;
    procedure SetFilename(AValue: String);
    //procedure SetViewMode(AValue: TViewMode);
    procedure StampReady(ProgressID: QWord; Index: Integer);
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    procedure AfterCellSelect(data : int64);
    procedure EnableActions;
    procedure SetMainImage(Index: Integer);
    procedure HideCropTool;
    function SelectedGridItems: TIntArray;
    procedure AppendFile(const aFileName : String);
  public
    constructor Create(aOwner: TComponent; aConfig : TConfig; aLog : ILog;
                       const aProgress : TCbzProgressEvent = nil); reintroduce;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearUndo;
    procedure FullClear;

    property Filename : String read FFilename write SetFilename;
    property State : TZipMode read GetState;
    //property Mode : TViewMode read FViewMode write SetViewMode;
    property StampWidth:Integer read GetStampWidth;
    property StampHeight : Integer read GetStampHeight;
    property Cbz : TCbz read zf;
  end;

implementation

{$R *.lfm}

uses
  dialogs, utils.vcl;

{ TCbzViewerFrame }

constructor TCbzViewerFrame.Create(aOwner: TComponent; aConfig : TConfig; aLog : ILog;
                                   const aProgress : TCbzProgressEvent = nil);
begin
  inherited Create(aOwner);
  FLog := aLog;
  Fconfig := aConfig;
  FProgress := aProgress;
  zf := TCbz.Create(FLog, DrawGrid1.DefaultColWidth - 5,
                      DrawGrid1.DefaultRowHeight - 5,
                      75, @StampReady);
  zf.Progress := @Progress;
end;

destructor TCbzViewerFrame.Destroy;
begin
  if Assigned(zf) then
  begin
    zf.Close;
    zf.Free;
  end;
  FLog := nil;
  inherited Destroy;
end;

procedure TCbzViewerFrame.ActionLastExecute(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := zf.ImageCount - 1;
    DrawGrid1.Selected[zf.ImageCount - 1] := True;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TCbzViewerFrame.DrawGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
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

procedure TCbzViewerFrame.DrawGrid1KeyUp(Sender: TObject; var Key: Word;
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

procedure TCbzViewerFrame.DrawGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCbzViewerFrame.DrawGrid1MouseEnter(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
    DrawGrid1.SetFocus;
end;

procedure TCbzViewerFrame.DrawGrid1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  ShiftState: TShiftState;
begin
  if Application.Terminated or (not Assigned(zf)) then
    Exit;

  ShiftState := GetKeyShiftState;
  if not (ssShift in ShiftState) then
    FoldPos := aRow;

  CanSelect := zf.Mode <> zmClosed;
  if not CanSelect then
    Exit;

  EnableActions;
  Application.QueueAsyncCall(@AfterCellSelect, 0);
end;


procedure TCbzViewerFrame.pmgridPopup(Sender: TObject);
begin
  EnableActions;
end;

procedure TCbzViewerFrame.pmImagePopup(Sender: TObject);
begin
  EnableActions;
end;

procedure TCbzViewerFrame.speBottomChange(Sender: TObject);
begin
  Shape1.Height := speBottom.Value;
end;

procedure TCbzViewerFrame.speLeftChange(Sender: TObject);
begin
  Shape1.left := speLeft.Value;
  speRight.MaxValue:=(Image1.left+(Image1.DestRect.Right - Image1.DestRect.left))-Shape1.left;
end;

procedure TCbzViewerFrame.speRightChange(Sender: TObject);
begin
  Shape1.width := speRight.Value;
end;

procedure TCbzViewerFrame.speTopChange(Sender: TObject);
begin
  Shape1.Top := speTop.Value;
  speBottom.MaxValue:=(Image1.top+(Image1.DestRect.Bottom - Image1.DestRect.Top))-shape1.top;
end;

procedure TCbzViewerFrame.ActionFirstExecute(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := 0;
    DrawGrid1.Selected[0] := True;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TCbzViewerFrame.AfterCellSelect(data : int64);
begin
  EnableActions;
  SetMainImage(DrawGrid1.Position);
end;

procedure TCbzViewerFrame.SetFilename(AValue: String);
begin
  if FFilename=AValue then Exit;

  FFilename:=AValue;
  if AVAlue.IsEmpty then
  begin
    zf.close;
    Image1.Picture.Clear;
  end
  else
  begin
    zf.Open(AVAlue, zmRead);
    with DrawGrid1 do
    begin
      Visible := False;
      Max := zf.ImageCount;
      Position := 0;
      Visible := True;
      Application.QueueAsyncCall(@AfterCellSelect, 0);
    end;
  end;
end;

function TCbzViewerFrame.GetStampHeight: Integer;
begin
  result := DrawGrid1.DefaultRowHeight;
end;

function TCbzViewerFrame.GetStampWidth: Integer;
begin
  result := DrawGrid1.DefaultColWidth;
end;

function TCbzViewerFrame.GetState: TZipMode;
begin
  result := zf.Mode;
end;

{
procedure TCbzViewerFrame.SetViewMode(AValue: TViewMode);
begin
  FViewMode:=AValue;

  EnableActions;
  if Mode = vmRead then
  begin
    Image1.PopupMenu := nil;
    DrawGrid1.PopupMenu:=nil;
  end
  else
  begin
    Image1.PopupMenu := pmImage;
    DrawGrid1.PopupMenu:=pmGrid;
  end;
end;
}

procedure TCbzViewerFrame.Clear;
begin
  zf.Close;
  Image1.Picture.Clear;
  EnableActions;
end;

procedure TCbzViewerFrame.ClearUndo;
begin
  zf.ClearUndo;
end;

procedure TCbzViewerFrame.FullClear;
begin
  Clear;
  ClearUndo;
end;

procedure TCbzViewerFrame.StampReady(ProgressID: QWord; Index: Integer);
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

procedure TCbzViewerFrame.Progress(Sender: TObject; const ProgressID: QWord;
  const aPos, aMax: Integer; const Msg: String);
begin
  if Assigned(FProgress) then
    FProgress(Self, ProgressID, aPos, aMax, Msg);
end;

procedure TCbzViewerFrame.HideCropTool;
begin
  PanelCrop.Visible:=false;
  Shape1.Visible:=False;
end;

procedure TCbzViewerFrame.SetMainImage(Index: Integer);
var
  b: TBitmap;
begin
  Screen.Cursor := crHourGlass;
  try
    if (zf.Mode <> zmClosed) and (Index < zf.ImageCount) then
    begin
      HideCropTool;
      Image1.Visible := True;
      b := zf.Image[Index];
      try
        if Assigned(b) then
        begin
          pnlimgName.Caption := format('%s (%dx%d)',
            [zf.FileNames[Index], b.Width, b.Height]);
          if Assigned(b) then
            Image1.Picture.Bitmap.Assign(b);
          Image1.Update;
        end;
      finally
        b.free;
      end;
    end
    else
      Image1.Picture.Clear;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TCbzViewerFrame.SelectedGridItems: TIntArray;
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

procedure TCbzViewerFrame.EnableActions;
begin
  if Assigned(zf) then
  begin
    ActionUndo.Enabled := (zf.Mode <> zmClosed) and zf.CanUndo;
    ActionUndoAll.Enabled := (zf.Mode <> zmClosed) and zf.CanUndo;
    ActionDelete.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0);
    ActionSelectAll.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0);
    ActionMoveUp.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
    ActionMoveDown.Enabled := (zf.Mode <> zmClosed) and
      (DrawGrid1.Position < zf.ImageCount - 1);
    ActionMoveToTop.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
    ActionMoveToBottom.Enabled := (zf.Mode <> zmClosed) and
      (DrawGrid1.Position < zf.FileCount - 1);
    ActionCropTool.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position >= 0);
    // files
    ActionAppendFile.Enabled := (zf.Mode <> zmClosed);
    ActionRewriteManga.Enabled := (zf.Mode <> zmClosed);
    // btns
    ActionRotm90.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
      (Length(SelectedGridItems) >= 1);
    ActionRot90.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0) and
      (Length(SelectedGridItems) >= 1);
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
end;

procedure TCbzViewerFrame.ActionSplitImageExecute(Sender: TObject);
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
        b1.Canvas.AntialiasingMode:=amOn;
        b2 := TBitmap.Create;
        b2.Canvas.AntialiasingMode:=amOn;
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

procedure TCbzViewerFrame.ActionUndoAllExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionUndoExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionVertFlipExecute(Sender: TObject);
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

procedure TCbzViewerFrame.btnCancelClick(Sender: TObject);
begin
  HideCropTool;
end;

procedure TCbzViewerFrame.btnCropClick(Sender: TObject);
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

procedure TCbzViewerFrame.DrawGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aRow, ACol: Integer;
begin
  DrawGrid1.MouseToCell(X, Y, ACol, aRow);
  zf.Invert(DrawGrid1.Position, aRow, @Progress);
  DrawGrid1.Invalidate;
  SetMainImage(DrawGrid1.Position);
end;

procedure TCbzViewerFrame.DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aRow, ACol: Integer;
begin
  DrawGrid1.MouseToCell(X, Y, ACol, aRow);
  Accept := (Sender = Source) and (aRow <> DrawGrid1.Position) and
    (State <> dsDragEnter); // and (GetKeyState(VK_LBUTTON) and $8000 <> 0);
end;


procedure TCbzViewerFrame.ActionCropToolExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionAppendFileExecute(Sender: TObject);
var
  i : integer;
begin
  with TOpenDialog.Create(Self) do
  try
    Title := 'Choose cbz to append';
    Filter := 'Cbz files (*.cbz)|*.cbz';
    Options:=[ofHideReadOnly,ofAllowMultiSelect,ofPathMustExist,ofFileMustExist,
              ofNoTestFileCreate,ofEnableSizing,ofDontAddToRecent,ofViewDetail];
    if Execute then
      for i:=0 to Files.Count-1 do
        AppendFile(Files[i]);
  finally
    Free;
  end;
end;

procedure TCbzViewerFrame.AppendFile(const aFileName: String);
var
  tmpz : TCbz;
  i : longint;
  sar : TStreamArray;
  rpos : integer;
begin
  tmpz := TCbz.Create(FLog);
  try
    SetLength(sar, 0);
    rpos := DrawGrid1.Position;
    try
      tmpz.Open(aFileName, zmRead);
      for i:=0 to tmpz.FileCount-1 do
        if TCbz.AllowedFile(tmpz.FileNames[i]) then
        begin
          SetLength(sar, length(sar)+1);
          sar[length(sar)-1] := tmpz.GetFileStream(i);
        end;
        zf.Add(sar, @Progress);
        // refresh
        DrawGrid1.Max := zf.ImageCount;
        DrawGrid1.Position := rpos;
        DrawGrid1.Invalidate;
        Application.QueueAsyncCall(@AfterCellSelect, 0);
    finally
      for i:=low(sar) to high(sar) do
        if Assigned(sar[i]) then
          sar[i].Free;
    end;
  finally
    tmpz.Free;
  end;
end;

procedure TCbzViewerFrame.ActionMoveDownExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionMoveToBottomExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionMoveToTopExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionMoveupExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionRewriteMangaExecute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionRot90Execute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionRotm90Execute(Sender: TObject);
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

procedure TCbzViewerFrame.ActionSelectAllExecute(Sender: TObject);
var
  i : integer;
begin
  with DrawGrid1 do
  begin
    for i := 0 to RowCount - 1 do
      Selected[i] := true;
    Invalidate;
  end;
end;

procedure TCbzViewerFrame.ActionHorizFlipExecute(Sender: TObject);
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

procedure JoinImages(b1, b2, bFinal: TBitmap);
begin
  {$ifdef Linux}
    bFinal.PixelFormat := pf24bit;
  {$else}
    bFinal.PixelFormat := pf32bit;
  {$endif}
  bFinal.Width := b1.Width * 2;
  bFinal.Height := b1.Height;
  bFinal.Canvas.AntialiasingMode:=amOn;
  bFinal.Canvas.Draw(0, 0, b1);
  bFinal.Canvas.Draw(b1.Width, 0, b2);
end;

procedure TCbzViewerFrame.ActionJoinExecute(Sender: TObject);
var
  lst: TIntArray;
  ar : TIntArray;
  b1, b2 : Tbitmap;
  bFinal: TBitmap;
  ms : TMemoryStream;
  ars : TStreamArray;
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
            ms := TMemoryStream.Create;
            bFinal.SaveToStream(ms);
            // delete images
            SetLength(ar, 2);
            ar[0] := lst[0];
            ar[1] := lst[1];
            zf.Delete(ar, @Progress);
            // insert joined image
            SetLength(ars, 1);
            ars[0] := ms;
            zf.Insert(ars, lst[0]);
            // re select
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

procedure TCbzViewerFrame.ActionDeleteExecute(Sender: TObject);
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

      //TreeView1.Selected.Text := ExtractFileName(zf.Delete(lst, @Progress));
      zf.Delete(lst, @Progress);
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


end.

