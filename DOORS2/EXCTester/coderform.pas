unit CoderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, exc, TypInfo;

type

  { TCoderWin }

  TCoderWin = class(TForm)
    Label1: TLabel;
    LblCP: TLabel;
    StepBtn: TButton;
    CompileBtn: TButton;
    SegList: TListBox;
    RunBtn: TButton;
    CodeEditor: TMemo;
    procedure CompileBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure SegListDblClick(Sender: TObject);
    procedure StepBtnClick(Sender: TObject);
  private
    FEXCFile: TEXCFile;
    procedure TestDevice(e: TEXCFile);
    procedure SysCall(e: TEXCFile; syscall: byte);
  public

  end;

var
  CoderWin: TCoderWin;

implementation

{$R *.lfm}

{ TCoderWin }

procedure TCoderWin.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Caption:=Application.Title;
  {$IFDEF DEBUG}
  CodeEditor.Lines.LoadFromFile('debug.src');
  FEXCFile:=TEXCFile.Create;
  if FEXCFile.LoadEXC('TEST.EXC') then
    for i:=0 to 31 do
      SegList.Items.Add(IntToStr(i)+': '+GetEnumName(TypeInfo(SegmentType), Ord(FEXCFile.GetSegType(i))));
  {$ENDIF}
end;

procedure TCoderWin.CompileBtnClick(Sender: TObject);
var
  i: integer;
begin
  {$IFDEF DEBUG}
  CodeEditor.Lines.SaveToFile('debug.src');
  {$ENDIF}
  if Assigned(FEXCFile) then
    FEXCFile.Free;
  FEXCFile:=TEXCFile.Create;
  FEXCFile.AddDevice(8, @TestDevice);
  FEXCFile.OnSysCall:=@SysCall;
  if not FEXCFile.Compile(CodeEditor.Lines) then
  begin
    ShowMessage('Compile error!');
    Exit;
  end;
  WriteLn('Compile OK');
  SegList.Clear;
  for i:=0 to 31 do
    SegList.Items.Add(IntToStr(i)+': '+GetEnumName(TypeInfo(SegmentType), Ord(FEXCFile.GetSegType(i))));
end;

procedure TCoderWin.FormDestroy(Sender: TObject);
begin
  {$IFDEF DEBUG}
  CodeEditor.Lines.SaveToFile('debug.src');
  {$ENDIF}
  if Assigned(FEXCFile) then
    FEXCFile.Free;
end;

procedure TCoderWin.RunBtnClick(Sender: TObject);
begin
  if not Assigned(FEXCFile) then
    Exit;
  repeat
    FEXCFile.RunTask;
    Application.ProcessMessages;
    Sleep(1);
  until FEXCFile.State = tskStop;
  ShowMessage('Program ended.');
end;

procedure TCoderWin.SegListDblClick(Sender: TObject);
var
  s: integer;
begin
  s:=SegList.ItemIndex;
  if FEXCFile.GetSegType(s) = segString then
    ShowMessage(IntToStr(s)+': '+FEXCFile.GetSegString(s));
  {if FEXCFile.GetSegType(GetEnumValue(TypeInfo(SegmentType), SegList.Items.Strings[s])) = segString then
    ShowMessage(FEXCFile.GetSegString(s));}
end;

procedure TCoderWin.StepBtnClick(Sender: TObject);
begin
  if not Assigned(FEXCFile) then
    Exit;
  {if FEXCFile.State <> tskRun then
    Exit;}
  FEXCFile.RunTask;
  LblCP.Caption:=IntToStr(FEXCFile.CP);
end;

procedure TCoderWin.TestDevice(e: TEXCFile);
begin

end;

procedure TCoderWin.SysCall(e: TEXCFile; syscall: byte);
begin
  WriteLn(' * SysCall: ',syscall);
end;

end.

