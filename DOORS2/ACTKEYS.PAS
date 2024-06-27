unit actkeys;

interface

uses crt, grlib, bootsys, syscfg, registry;

procedure InitActKeys;
procedure KeyStat(var key, special: byte);

type
  TActionKeys = record
    up, down, left, right: Byte;
    help, save, load, abort: Byte;
    func: Array[1..4] of Byte;
    tasks, clock, pause, monitor: Byte;
  end;
  TInputMode = (icLine, icCommand, icParam);
  TInputConfig = record
    prompt: string[30];
    line: string[40];
    param: string[40];
    mode: TInputMode;
  end;

var
  ActionKeys: TActionKeys;
  InputConfig: TInputConfig;

implementation

function GetActionKey: Byte;
var
  k, sp: Byte;
begin
  repeat
    KeyStat(k, sp);
  until sp > 0;
  GetActionKey:=sp;
end;

procedure CreateActMap;
var
  scr: PScreen;
  r: PRegistry;
  i: integer;
  s: string;
begin
  scr:=Nil;
  GetScreen(scr);
  ClearBorder(3,0);
  WriteLine('Action Key Subsystem started.');
  WriteLine('No Action Keys are currently mapped.');
  WriteLine('Please Map each action key:');
  With ActionKeys do
  begin
    WriteLine(' - Up');
    up:=GetActionKey;
    WriteLine(' - Down');
    down:=GetActionKey;
    WriteLine(' - Left');
    left:=GetActionKey;
    WriteLine(' - Right');
    right:=GetActionKey;
    WriteLine(' - Help');
    help:=GetActionKey;
    WriteLine(' - Save');
    save:=GetActionKey;
    WriteLine(' - Load');
    load:=GetActionKey;
    WriteLine(' - Abort');
    abort:=GetActionKey;
    ClearBorder(3,0);
    for i:=1 to 4 do
    begin
      Str(i,s);
      WriteLine(' - Function '+s);
      func[i]:=GetActionKey;
    end;
    WriteLine(' - Tasks');
    tasks:=GetActionKey;
    WriteLine(' - Clock');
    clock:=GetActionKey;
    WriteLine(' - Pause');
    pause:=GetActionKey;
    WriteLine(' - Monitor');
    monitor:=GetActionKey;
  end;
  New(r);
  r^.key:='KEYMAP';
  Move(ActionKeys, r^.data, SizeOf(ActionKeys));
  i:=AddRegistry(r);
  PutScreen(scr);
  FreeScreen(scr);
end;

procedure InitActKeys;
var
  k: integer;
begin
  if SysState.gui_mode then
    AddBootIcon('Act')
  else
    WriteLn('Initializing Action Key Subsystem...');
  k:=FindKey('KEYMAP');
  if k = -1 then
    CreateActMap
  else
    Move(Reg[k]^.data, ActionKeys, SizeOf(ActionKeys));
  With InputConfig do
  begin
    prompt:='';
    line:='';
    param:='';
    mode:=icLine;
  end;
end;

procedure KeyStat(var key, special: byte);
begin
  key:=0;
  special:=0;
  if not KeyPressed then
    Exit;
  key:=ord(ReadKey);
  if key = 0 then
    special:=ord(ReadKey)
end;

end.