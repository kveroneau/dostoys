program EXCCoder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, CoderForm, exc
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ByteCode VM Coder';
  Application.Scaled:=True;
  {$IFNDEF DEBUG}
  Application.ExceptionDialog:=aedOkMessageBox;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TCoderWin, CoderWin);
  Application.Run;
end.

