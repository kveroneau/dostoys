program AdvenkEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, adveneditor
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Advenk Editor';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TEditorForm, EditorForm);
  Application.Run;
end.

