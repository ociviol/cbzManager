program cbzLibraryLinux;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uConfig, frmwait, uCbzLibrary;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='cbzLibrary';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCbzLibrary, CbzLibrary);
  Application.Run;
end.

