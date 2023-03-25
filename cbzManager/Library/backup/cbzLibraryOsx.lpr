program cbzLibraryOsx;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uCbzLibrary, uConfig
  { you can add units after this };

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

