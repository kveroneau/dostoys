unit scrsys;

{ Started on May 28th, 2024 }

interface

uses zscrsave;

procedure AssignKernel(AScreen: PScreen);
procedure AssignSysLog(AScreen: PScreen);
procedure AssignConsole(AScreen: PScreen; tty: byte);
procedure WriteKernel(msg: string);
procedure WriteSysLog(msg: string);
procedure SwitchVT(tty: byte);
procedure WriteVT(tty: byte; msg: string);

implementation

var
  Kernel: PScreen;
  SysLog: PScreen;
  Console: array[1..7] of PScreen;
  vt: byte;
  TmpScr: TScreen;

procedure AssignKernel(AScreen: PScreen);
begin
  Kernel:=AScreen;
end;

procedure AssignSysLog(AScreen: PScreen);
begin
  SysLog:=AScreen;
end;

procedure AssignConsole(AScreen: PScreen; tty: byte);
begin
  Console[tty]:=AScreen;
end;

procedure WriteScreen(AScreen: PScreen; msg: string);
begin
  if AScreen = Nil then
    Exit;
  SaveScreen(@TmpScr);
  DrawScreen(AScreen);
  WriteLn(msg);
  SaveScreen(AScreen);
  DrawScreen(@TmpScr);
end;

procedure WriteKernel(msg: string);
begin
  WriteScreen(Kernel, msg);
end;

procedure WriteSysLog(msg: string);
begin
  WriteScreen(SysLog, msg);
end;

procedure SwitchVT(tty: byte);
begin
  if vt > 0 then
    SaveScreen(Console[vt]);
  vt:=tty;
  if vt > 0 then
    DrawScreen(Console[vt]);
end;

procedure WriteVT(tty: byte; msg: string);
begin
  if vt = tty then
    WriteLn(msg)
  else
    WriteScreen(Console[tty], msg);
end;

begin
  Kernel:=Nil;
  SysLog:=Nil;
  FillChar(Console[1], SizeOf(Console), #0);
  vt:=0;
end.
