program cbzlibraryosxarm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uCbzLibrary;

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

