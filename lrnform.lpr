program lrnform;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='SoftwareListShow';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMyForm, MyForm);
  Application.Run;
end.

