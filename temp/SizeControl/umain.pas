unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, LCLType, LCLIntf, Forms, ExtCtrls,
  Graphics, Menus, StdCtrls, ComCtrls, SizeControl, Dialogs;

type

  { TfMain }

  TfMain = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    EnableSizeControl1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    cbSizeMove: TCheckBox;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    Label4: TLabel;
    TabSheet2: TTabSheet;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    Button1: TButton;
    PopupMenu2: TPopupMenu;
    MenuItem1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure EnableSizeControl1Click(Sender: TObject);
    procedure cbSizeMoveClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
  private
    SizeCtrl: TSizeCtrl;
    procedure SizeCtrlDuring(Sender: TObject; dx, dy: integer;{%H-} State: TSCState);
    procedure SizeCtrlEnd(Sender: TObject; {%H-}State: TSCState);
    procedure SizeCtrlTargetChange(Sender: TObject);
    procedure SizeCtrlMouseDown(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeCtrlSetCursor(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeCtrlKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
  protected
  public
    { Public declarations }
  end;

var
  fMain: TfMain;



implementation
const
  GRIDSIZE: integer = 10;
var
  popupMousePos: TPoint;

{$R *.lfm}

procedure RegComponents(aParent: TWinControl; SizeCtrl: TSizeCtrl);
var
  i: integer;
begin
  for i := 0 to aParent.ControlCount -1 do
  begin
    if aParent.Controls[i].Tag = 0 then
      SizeCtrl.RegisterControl(aParent.Controls[i]);
    if aParent.Controls[i] is TWinControl then
      RegComponents(TWinControl(aParent.Controls[i]), SizeCtrl);
  end;
end;

procedure UnregComponents(aParent: TWinControl; SizeCtrl: TSizeCtrl);
var
  i: integer;
begin
  for i := 0 to aParent.ControlCount -1 do
  begin
    SizeCtrl.UnRegisterControl(aParent.Controls[i]);
    if aParent.Controls[i] is TWinControl then
      UnregComponents(TWinControl(aParent.Controls[i]), SizeCtrl);
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  SizeCtrl := TSizeCtrl.Create(self);
  SizeCtrl.OnTargetChange := @SizeCtrlTargetChange;
  SizeCtrl.OnDuringSizeMove := @SizeCtrlDuring;
  SizeCtrl.OnEndSizeMove := @SizeCtrlEnd;
  SizeCtrl.GridSize := GRIDSIZE;
  SizeCtrl.PopupMenu := PopupMenu2;
  SizeCtrl.OnMouseDown := @SizeCtrlMouseDown;
  SizeCtrl.OnSetCursor := @SizeCtrlSetCursor;
  SizeCtrl.OnKeyDown := @SizeCtrlKeyDown;
  RegComponents(self, SizeCtrl);
  SizeCtrl.Enabled := true;
  SizeCtrl.BtnColor := $CC;
  Screen.Cursor := crDefault;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  SizeCtrl.Enabled := false;
end;

procedure TfMain.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TfMain.EnableSizeControl1Click(Sender: TObject);
begin
  cbSizeMove.Checked := not cbSizeMove.Checked;
end;

procedure TfMain.cbSizeMoveClick(Sender: TObject);
begin
  SizeCtrl.Enabled := cbSizeMove.Checked;
  EnableSizeControl1.Checked := cbSizeMove.Checked;
  if cbSizeMove.Checked then
  begin
    UnRegComponents(PageControl1, SizeCtrl);
    RegComponents(PageControl1, SizeCtrl);
  end;
  ActiveControl := nil;
  invalidate;
end;


procedure TfMain.FormResize(Sender: TObject);
begin
  SizeCtrl.Update;
end;



//------------------------------------------------------------------------------
// Pretty much everything below demonstrates optional features .
//------------------------------------------------------------------------------
procedure TfMain.Button1Click(Sender: TObject);
begin
  ShowMessage('Button1 pressed.');
end;

procedure TfMain.FormPaint(Sender: TObject);
var
  i,j: integer;
begin
  if (GRIDSIZE > 1) and SizeCtrl.Enabled then
    for i := 0 to width div GRIDSIZE do
      for j := 0 to height div GRIDSIZE do
        canvas.Pixels[i*GRIDSIZE, j*GRIDSIZE] := clGray;
end;

procedure TfMain.SizeCtrlTargetChange(Sender: TObject);
begin
  if SizeCtrl.TargetCount = 0 then
    StatusBar1.SimpleText := ''
  else
  begin
    with SizeCtrl.Targets[0] do
      StatusBar1.SimpleText := Format('  %s -  left:%d  top:%d, width:%d  height:%d',
                                      [Name, left, top, width, height]);
  end;
end;

procedure TfMain.SizeCtrlDuring(Sender: TObject; dx,dy: integer; State: TSCState);
begin
  with SizeCtrl.Targets[0] do
  begin
    if State = scsMoving then
      StatusBar1.SimpleText := Format('  %s -  left:%d  top:%d, width:%d  height:%d',
                                      [Name, left+dx, top+dy, width, height])
    else
      StatusBar1.SimpleText := Format('  %s -  left:%d  top:%d, width:%d  height:%d',
                                      [Name, left, top, width+dx, height+dy]);
  end;
end;

procedure TfMain.SizeCtrlEnd(Sender: TObject; State: TSCState);
begin
  with SizeCtrl do
  begin
    if TargetCount = 0 then
      StatusBar1.SimpleText := ''
    else with Targets[0] do
      StatusBar1.SimpleText :=  format('  %s -  left:%d  top:%d, width:%d  height:%d', [Name, left, top, width, height]);
  end;
end;

function My_IndexOfTabAt(PageControl: TPageControl; X, Y: Integer): Integer;
const
  TCM_FIRST = $1300;
  TCM_HITTEST = TCM_FIRST + 13;
var
  HitTest: TLMNCHitTest;
begin
  Result := -1;
  if PtInRect(PageControl.ClientRect, Point(X, Y)) then
  begin
    with HitTest do
    begin
      XPos := X;
      YPos := Y;
      {$HINTS OFF}
      Result := SendMessage(PageControl.Handle, TCM_HITTEST, 0, LPARAM(@HitTest));
      {$HINTS OFF}
    end;
  end;
end;

procedure TfMain.SizeCtrlMouseDown(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      with TargetPt do
        i := My_IndexOfTabAt(TPageControl(Target), X, Y);
      if (i >= 0) and ( ActivePage.PageIndex <> i) then
      begin
        handled := true;
        UnregComponents(PageControl1, SizeCtrl);
        ActivePage := Pages[i];
        RegComponents(PageControl1, SizeCtrl);
      end;
    end;
end;

procedure TfMain.SizeCtrlSetCursor(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      with TargetPt do i := My_IndexOfTabAt(TPageControl(Target), X, Y);
      if (i >= 0) and (ActivePage.PageIndex <> i) then
      begin
        handled := true;
        SetCursor(screen.Cursors[crDefault]);
      end;
    end;
end;

procedure TfMain.PopupMenu2Popup(Sender: TObject);
begin
  GetCursorPos(popupMousePos);
end;

procedure TfMain.MenuItem1Click(Sender: TObject);
var
  ctrl: TControl;
begin
  ctrl := SizeCtrl.TargetCtrlFromPt(popupMousePos);
  ShowMessage('You just clicked - '+ ctrl.Name);
end;


procedure TfMain.SizeCtrlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key > VK_HELP then beep;
end;


end.
