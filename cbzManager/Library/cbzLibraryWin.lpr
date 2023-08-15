program cbzLibraryWin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmwait, uLibraryClasses, uCbzLibrary, config,
  uThreadScrub, uThreadFill, uConfig, uCbzViewer;

{$R *.res}
var
  CbzLibrary : TCbzLibrary;

begin
  RequireDerivedFormResource:=True;
  Application.Title:='cbzLibrary';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCbzLibrary, CbzLibrary);
  Application.Run;
end.

