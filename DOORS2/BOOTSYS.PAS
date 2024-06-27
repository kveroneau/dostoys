unit bootsys;

interface

uses syscfg, grlib, graph, registry;

procedure InitGUI;
procedure AddBootIcon(s: string);

implementation

var
  BootIconX: integer;

procedure AddBootIcon(s: string);
begin
  DrawButton((BootIconX*30)+10, 130, s);
  Inc(BootIconX);
end;

procedure InitGUI;
var
  s: string;
begin
  Write(' * Hit Enter to start GUI Mode...');
  ReadLn;
  InitMode(SystemConfig^.GraphDriver, SystemConfig^.GraphMode);
  SysState.gui_mode:=True;
  DrawBorder('BootSys Starting...');
  Str(MemAvail, s);
  WriteLine(s+' bytes available.');
  OutTextXY(20, 50, 'Door2 is now starting, please stand by...');
  AddBootIcon('Cfg');
  AddBootIcon('GUI');
end;

procedure InitBoot;
begin
  WriteLn('BootSys starting...');
  BootIconX:=0;
end;

begin
  InitBoot;
end.