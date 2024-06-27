unit globals;

interface

uses vbd, gfxlib, maplib;

type
  TInputModes = (imNone, imChar, imLine);
  TSysModes = (smMain, smShell, smRun, smMedia, smTileEdit, smMapEdit,
               smEngine, smEngineScript);

  TSysKey = array[0..9] of string;

  PSysCmd = ^TSysCmd;
  TSysCmd = record
    cmd: string[10];
    script: string;
  end;

  TSysCmds = array[0..31] of PSysCmd;

var
  StartMem: longint;
  SysRunning: boolean;
  SysMode: TSysModes;
  SysVerbose: boolean;
  SysSprites: TSpriteSet;
  SysMaps: TMapSet;
  InputMode: TInputModes;
  InputLine: string;
  InputCaps: boolean;
  RootBD: VHandle;
  MediaBD: VHandle;
  BootBD: PVBD;
  SysScripts: TSysKey;
  SysCmds: TSysCmds;

function FindCmd(cmd: string): PSysCmd;
function FreeCmd: integer;

implementation

var
  ExitSave: Pointer;

{$F+}
procedure GlobalDone;
var
  i: integer;
begin
  ExitProc:=ExitSave;
  {$IFDEF DEBUG}
  WriteLn('Globals done.');
  {$ENDIF}
  for i:=0 to 31 do
    if SysCmds[i] <> Nil then
      Dispose(SysCmds[i]);
  i:=StartMem-MemAvail;
  WriteLn('Memory at program start: ',StartMem);
  if i > 0 then
    WriteLn('Memory leaked: ',i)
  else
    WriteLn('No Memory leaked.');
end;
{$F-}

function FindCmd(cmd: string): PSysCmd;
var
  i: integer;
begin
  for i:=0 to 31 do
    if SysCmds[i] <> Nil then
      if SysCmds[i]^.cmd = cmd then
      begin
        FindCmd:=SysCmds[i];
        Exit;
      end;
  FindCmd:=Nil;
end;

function FreeCmd: integer;
var
  i: integer;
begin
  for i:=0 to 31 do
    if SysCmds[i] = Nil then
    begin
      FreeCmd:=i;
      Exit;
    end;
  FreeCmd:=-1;
end;

begin
  {$IFDEF DEBUG}
  WriteLn(' *** DEBUG Mode ***');
  SysVerbose:=True;
  WriteLn(' * Globals initializing...');
  {$ELSE}
  SysVerbose:=False;
  {$ENDIF}
  RootBD.active:=False;
  MediaBD.active:=False;
  BootBD:=@RootBD;
  FillChar(SysScripts, SizeOf(SysScripts), #0);
  FillChar(SysCmds, SizeOf(SysCmds), #0);
  FillChar(SysSprites, SizeOf(SysSprites), #0);
  FillChar(SysMaps, SizeOf(SysMaps), #0);
  ExitSave:=ExitProc;
  ExitProc:=@GlobalDone;
end.
