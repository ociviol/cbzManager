program cbzManagerWin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Sysutils,
  Forms, main, config, webp, uCbz, Utils.ZipFile, frmwait, uDataPool, uDataItem,
  uThreadConvert, uWorkerThread, uThreadExtract, uConfig, uCbzLibrary, utils.Logger;

{$R *.res}
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  //Application.CreateForm(TCbzLibrary, CbzLibrary);
  Application.Run;
end.



