unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Spin;

type

  { TConfigFrm }

  TConfigFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cblogging: TCheckBox;
    edtcwebp: TEdit;
    edtunzip: TEdit;
    edtunrar: TEdit;
    edtp7zip: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    speNbThreads: TSpinEdit;
  private
  public
  end;


implementation

{$R *.lfm}

end.

