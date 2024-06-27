unit syscfg;

interface

uses registry, graph;

type
  TExitMode = (emNone, emMonitor, emHalt);
  PSysConfig = ^TSysConfig;
  TSysConfig = record
    GraphDriver, GraphMode: integer;
    BootGUI, DramaMode: boolean;
    ExitMode: TExitMode;
  end;
  TSysState = record
    gui_mode: boolean;
    running: boolean;
  end;

var
  SystemConfig: PSysConfig;
  SysState: TSysState;

const
  VER_STRING = 'v0.2';

implementation

procedure CreateRegistry;
var
  r: PRegistry;
begin
  SystemConfig^.GraphDriver:=CGA;
  SystemConfig^.GraphMode:=CGAC2;
  SystemConfig^.BootGUI:=True;
  SystemConfig^.DramaMode:=False;
  SystemConfig^.ExitMode:=emHalt;
  WriteLn('Creating new registry...');
  New(r);
  r^.key:='SYSCFG';
  Move(SystemConfig^, r^.data, SizeOf(SystemConfig^));
  WriteLn('Added: ', AddRegistry(r));
end;

procedure InitSysConfig;
begin
  WriteLn('Initializing SysCfg...');
  SysState.gui_mode:=False;
  SysState.running:=False;
  New(SystemConfig);
  if Reg[0] = Nil then
    CreateRegistry;
  if Reg[0]^.key = 'SYSCFG' then
  begin
    Move(Reg[0]^.data, SystemConfig^, SizeOf(SystemConfig^));
  end
  else
  begin
    WriteLn('Error Loading SysCfg!');
    Dispose(SystemConfig);
    CloseRegistry;
    Halt(1);
  end;
end;

begin
  InitSysConfig;
end.