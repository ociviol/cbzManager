unit uCbzViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Grids, StdCtrls, Spin, Menus,
  ActnList, uCbz,
  {$if defined(Linux) or defined(Darwin)}
    cthreads,
  {$endif}
  utils.Logger, Utils.Gridhelper, utils.Arrays, Types, graphics, PairSplitter,
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
    Panel2: TPanel;
    Panel3: TPanel;
    PanelCrop: TPanel;
    pnlimgName: TPanel;
    pmgrid: TPopupMenu;
    pmImage: TPopupMenu;
    Shape1: TShape;

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
    procedure DrawGrid1DragDrop(Sender, {%H-}Source: TObject; X, Y: Integer);
    procedure DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure DrawGrid1KeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseEnter(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure pmgridPopup(Sender: TObject);
    procedure pmImagePopup(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure CellReady(data : int64);
    //procedure SetViewMode(AValue: TViewMode);
    procedure StampReady(ProgressID: QWord; Index: Integer);
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    procedure AfterCellSelect({%H-}data : int64);
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
    procedure SelectImage(const Index : integer);
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
  dialogs;

const
  GripSize=20; // sets the area size of the corner to drag

type
  TControlEx = class(TControl);// need to follow the mouse even when outside the control area

var
  xPositionMouseDown,
  yPositionMouseDown : Integer;
  MouseIsDragging,
  MouseIsDraggingCorner : Boolean;

{ TCbzViewerFrame }

constructor TCbzViewerFrame.Create(aOwner: TComponent; aConfig : TConfig; aLog : ILog;
                                   const aProgress : TCbzProgressEvent = nil);
begin
  inherited Create(aOwner);
  FLog := aLog;
  Fconfig := aConfig;
  FProgress := aProgress;
  DrawGrid1.DefaultColWidth := aConfig.DefaultColWidth;
  DrawGrid1.DefaultRowHeight := aConfig.DefaultRowHeight;
  DrawGrid1.Width := DrawGrid1.DefaultColWidth + 20;
  zf := TCbz.Create(FLog, DrawGrid1.DefaultColWidth - 5,
                      DrawGrid1.DefaultRowHeight - 5,
                      75, @StampReady);
  zf.Progress := @Progress;
  DrawGrid1.Visible:=FAlse;
end;

destructor TCbzViewerFrame.Destroy;
begin
  if Assigned(zf) then
    zf.Close;

  inherited Destroy;
  Application.RemoveAsyncCalls(self);

  if Assigned(zf) then
    FreeAndNil(zf);

  FLog := nil;
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
        X := (aRect.Width - b.Width) div 2;
        Y := (aRect.Height - b.Height) div 2;
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
        46: // delete
            if ActionDelete.Enabled then
              ActionDelete.Execute;

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

         // 74 : j
         74: if ActionJoin.Enabled then
                ActionJoin.Execute;
         // 83 : S
         83: if ActionSplitImage.Enabled then
               ActionSplitImage.Execute;

         // 35 : end
         35: ActionLast.Execute;

         // 36: Home
         36: ActionFirst.Execute;
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

procedure TCbzViewerFrame.Shape1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  xPositionMouseDown := X;
  yPositionMouseDown := Y;
  MouseIsDragging := True;

  //if mouse down position is within grip area of panel (south west corner)
  if (y>Shape1.Height-GripSize) and (x>Shape1.Width-GripSize) then
  begin
    MouseIsDraggingCorner:=True;
  end;
{$IFNDEF DEBUG}
  TControlEx(Sender).MouseCapture := True;
{$ENDIF}
end;

procedure TCbzViewerFrame.Shape1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  nleft, ntop, nWidth, nHeight : integer;
  Bounds : TRect;
begin
  // get image bounds
  Bounds := Image1.DestRect;

  if (MouseIsDragging) and not(MouseIsDraggingCorner) then
  begin
    with Sender as TControl do
    begin
      nleft := X - xPositionMouseDown + Shape1.Left;
      ntop  := Y - yPositionMouseDown + Shape1.Top;
      //Bounds.Height := Bounds.Height + Panel3.Height;
      //Bounds.Top := Bounds.Top + Panel3.Height;
      // check top left bounds
      if nleft < Bounds.left then
        nleft := Bounds.left;
      if ntop < Bounds.Top then
        ntop := Bounds.top;
      // check bottom right bounds
      if nleft + shape1.Width > Bounds.Right then
        nleft := Bounds.right - Shape1.Width;
      if ntop + Shape1.Height > Bounds.Height then
        ntop := Bounds.Height - Shape1.Height;
      // final check
      Shape1.Left := nleft; //X - xPositionMouseDown + Shape1.Left;
      Shape1.Top  := ntop; //Y - yPositionMouseDown + Shape1.Top;
    end;
  end;

  if (Y>Panel1.Height-GripSize) and (X>Panel1.Width-GripSize) then
  begin
    Shape1.Cursor := crSizeSE;
  end else
  begin
    Shape1.Cursor := crdefault;
  end;

  if MouseIsDraggingCorner then
  begin
    with Sender as TControl do
    begin
      if X > Bounds.Right - Bounds.Left - (Shape1.Left - Bounds.left)  then
        nWidth  := Bounds.Right - Bounds.Left - (Shape1.Left - Bounds.left)
      else
        nWidth  := X;

      if Y > Bounds.Bottom - Bounds.Top - (Shape1.Top - Bounds.Top)  then
        nHeight  := Bounds.Bottom - Bounds.Top - (Shape1.Top - Bounds.Top)
      else
        nHeight  := Y;

      if nWidth < 10 then
        nWidth := 10;
      if nHeight < 10 then
        nHeight := 10;

      Shape1.Width := nWidth;
      Shape1.Height := nHeight;
    end;
  end;

end;

procedure TCbzViewerFrame.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if MouseIsDragging then
  begin
    MouseIsDraggingCorner:=False;
    MouseIsDragging := False;
{$IFNDEF DEBUG}
    TControlEx(Sender).MouseCapture := False;
{$ENDIF}
  end;
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
  if zf.Mode <> zmClosed then
  begin
    EnableActions;
    SetMainImage(DrawGrid1.Position);
  end;
end;

procedure TCbzViewerFrame.SetFilename(AValue: String);
begin
  if FFilename=AValue then Exit;

  FFilename:=AValue;
  if AVAlue.IsEmpty then
  begin
    zf.close;
    Image1.Picture.Clear;
    DrawGrid1.Visible:=False;
  end
  else
  try
    zf.Open(AVAlue, zmRead);
    with DrawGrid1 do
    begin
      Visible := False;
      Max := zf.ImageCount;
      Position := 0;
      Visible := True;
      Application.QueueAsyncCall(@AfterCellSelect, 0);
    end;
  except
    on E: Exception do
    begin
      Flog.Log('TCbzViewerFrame.SetFilename: Error : ' + E.Message);
      raise;
    end;
  end;
end;

procedure TCbzViewerFrame.CellReady(data: int64);
begin
  if zf.Mode <> zmClosed then
    DrawGrid1.InvalidateCell(0, Data);
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
  DrawGrid1.Visible:=FAlse;
  FFilename:='';
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

procedure TCbzViewerFrame.SelectImage(const Index: integer);
begin
  with DrawGrid1 do
  begin
    ClearSelection;
    Selected[Index] := True;
    DrawGrid1.Position := Index;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
    Invalidate;
  end;
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
    Application.QueueAsyncCall(@CellReady, Index);
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
    try
      if (zf.Mode <> zmClosed) and (Index < zf.ImageCount) and (Index >= 0) then
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
    except
    end;
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

  function RealizeRec(r : TRect):TRect;
  var
    coefx,
    coefy : double;
  begin
    coefx := image1.Picture.width / Image1.DestRect.Width;
    coefy := image1.Picture.Height / Image1.DestRect.Height;
    result.Left := Round(double(r.Left) * coefx);
    result.Top := Round(double(r.Top) * coefy);
    result.Width := Round(double(r.Width) * coefx);
    result.Height := Round(double(r.Height) * coefy);
  end;
begin
  Screen.Cursor := crHourGlass;
  try
    b := zf.Image[DrawGrid1.Position];
    r := Rect(shape1.Left - Image1.DestRect.Left, Shape1.Top - Image1.DestRect.Top,
              (shape1.Left - Image1.DestRect.Left) + Shape1.Width,
              (Shape1.Top - Image1.DestRect.Top) + Shape1.Height);
    r := RealizeRec(r);
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
  ACol := 0; aRow := 0; // disable hint
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
  ACol := 0; aRow := 0; // disable hint
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

