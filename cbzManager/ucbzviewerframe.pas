unit uCbzViewerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Grids, StdCtrls, Spin, Menus,
  ActnList, uCbz,
  {$if defined(Linux) or defined(Darwin)}
    cthreads,
  {$endif}
  utils.Logger, Utils.Gridhelper, utils.Arrays, Types, graphics, Utils.Graphics;

type
  TViewMode = (vmRead, vmModify);
  { TCbzViewerFrame }

  TCbzViewerFrame = class(TFrame)
    ActionAppend: TAction;
    ActionCropTool: TAction;
    ActionDelete: TAction;
    ActionFirst: TAction;
    ActionHorizFlip: TAction;
    ActionJoin: TAction;
    ActionLast: TAction;
    ActionList1: TActionList;
    ActionMoveDown: TAction;
    ActionMoveToBottom: TAction;
    ActionMoveToTop: TAction;
    ActionMoveup: TAction;
    ActionRefresh: TAction;
    ActionRewriteManga: TAction;
    ActionRot90: TAction;
    ActionRotm90: TAction;
    ActionSelectAll: TAction;
    ActionShowStats: TAction;
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
    pmGrid: TPopupMenu;
    pmImage: TPopupMenu;
    Shape1: TShape;
    speBottom: TSpinEdit;
    speLeft: TSpinEdit;
    speRight: TSpinEdit;
    speTop: TSpinEdit;
    procedure ActionFirstExecute(Sender: TObject);
    procedure ActionHorizFlipExecute(Sender: TObject);
    procedure ActionLastExecute(Sender: TObject);
    procedure ActionRot90Execute(Sender: TObject);
    procedure ActionRotm90Execute(Sender: TObject);
    procedure ActionVertFlipExecute(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FLog : ILog;
    FViewMode: TViewMode;
    zf : TCbz;
    FFilename : String;
    FoldPos: Integer;

    procedure SetFilename(AValue: String);
    procedure SetViewMode(AValue: TViewMode);
    procedure StampReady(ProgressID: QWord; Index: Integer);
    procedure Progress(Sender: TObject; const ProgressID: QWord;
                       const aPos, aMax: Integer; const Msg: String = '');
    procedure AfterCellSelect(data : int64);
    procedure EnableActions;
    procedure SetMainImage(Index: Integer);
    procedure HideCropTool;
    function SelectedGridItems: TIntArray;
  public
    constructor Create(aOwner: TComponent; aLog : ILog);
    destructor Destroy; override;

    procedure Clear;
    property Filename : String read FFilename write SetFilename;
    property Mode : TViewMode read FViewMode write SetViewMode;
  end;

implementation

{$R *.lfm}

uses
  utils.zipfile;

{ TCbzViewerFrame }

constructor TCbzViewerFrame.Create(aOwner: TComponent; aLog : ILog);
begin
  inherited Create(aOwner);
  FLog := aLog;
  zf := nil;
  Mode := vmRead;
end;

destructor TCbzViewerFrame.Destroy;
begin
  if Assigned(zf) then
    zf.Free;
  FLog := nil;
  inherited Destroy;
end;

procedure TCbzViewerFrame.ActionLastExecute(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := zf.ImageCount - 1;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TCbzViewerFrame.ActionRot90Execute(Sender: TObject);
begin

end;

procedure TCbzViewerFrame.ActionRotm90Execute(Sender: TObject);
begin

end;

procedure TCbzViewerFrame.ActionVertFlipExecute(Sender: TObject);
begin

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

procedure TCbzViewerFrame.ActionFirstExecute(Sender: TObject);
begin
  if zf.Mode <> zmClosed then
  begin
    DrawGrid1.Position := 0;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

procedure TCbzViewerFrame.ActionHorizFlipExecute(Sender: TObject);
begin

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
  zf := TCbz.Create(FLog, DrawGrid1.DefaultColWidth - 5,
                    DrawGrid1.DefaultRowHeight - 5,
                    75, @StampReady);
  zf.Progress := @Progress;
  zf.Open(FFilename, zmRead);
  with DrawGrid1 do
  begin
    Visible := False;
    Max := zf.ImageCount;
    Position := 0;
    Visible := True;
    Application.QueueAsyncCall(@AfterCellSelect, 0);
  end;
end;

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

procedure TCbzViewerFrame.Clear;
begin
  if Assigned(zf) then
    FreeAndNil(zf);
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
        try
          if Assigned(b) then
          begin
            pnlimgName.Caption := format('%s (%dx%d)',
              [zf.FileNames[Index], b.Width, b.Height]);
            if Assigned(b) then
              Image1.Picture.Bitmap := b;
            Image1.Update;
          end;
        finally
          b.free;
        end;
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
    ActionDelete.Enabled := zf.Mode <> zmClosed;
    ActionSelectAll.Enabled := (zf.Mode <> zmClosed) and (zf.ImageCount > 0);
    ActionMoveUp.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
    ActionMoveDown.Enabled := (zf.Mode <> zmClosed) and
      (DrawGrid1.Position < zf.ImageCount - 1);

    ActionMoveToTop.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position > 0);
    ActionMoveToBottom.Enabled := (zf.Mode <> zmClosed) and
      (DrawGrid1.Position < zf.FileCount - 1);
    ActionCropTool.Enabled := (zf.Mode <> zmClosed) and (DrawGrid1.Position >= 0);

    // files
    ActionAppend.Enabled := (zf.Mode <> zmClosed);
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
  // visibles
  ActionUndo.Visible := Mode <> vmRead;
  ActionUndoAll.Visible := Mode <> vmRead;
  ActionDelete.Visible := Mode <> vmRead;
  ActionSelectAll.Visible := Mode <> vmRead;
  ActionMoveUp.Visible := Mode <> vmRead;
  ActionMoveDown.Visible := Mode <> vmRead;
  ActionMoveToTop.Visible := Mode <> vmRead;
  ActionMoveToBottom.Visible := Mode <> vmRead;
  ActionCropTool.Visible := Mode <> vmRead;

  // files
  ActionAppend.Visible := Mode <> vmRead;
  ActionRewriteManga.Visible := Mode <> vmRead;

  // btns
  ActionRotm90.Visible := Mode <> vmRead;
  ActionRot90.Visible := Mode <> vmRead;
  ActionHorizflip.Visible := Mode <> vmRead;
  ActionVertflip.Visible := Mode <> vmRead;
  ActionJoin.Visible := Mode <> vmRead;
  ActionSplitImage.Visible := Mode <> vmRead;
end;

end.

