program NoteSys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainWindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='NoteSys';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TNoteSysForm, NoteSysForm);
  Application.Run;
end.

