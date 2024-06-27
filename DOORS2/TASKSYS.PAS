unit tasksys;

interface

uses syscfg, bootsys, grlib, actkeys, dos, graph, coder, registry, crt, exc;

procedure InitTasks;
procedure StartTask(name: string);
procedure SetState(state: TaskState);
procedure LineInput(prompt, s: string);
procedure ProcessTasks;

implementation

type
  TMemoryMap = Array[0..255] of PMemory;

var
  TaskList: Array[0..31] of PTask;
  MainTask: PTask;
  GlobalMem: TMemoryMap;

procedure InitTasks;
var
  i: integer;
begin
  if SysState.gui_mode then
    AddBootIcon('Tsk')
  else
    WriteLn('Initializing Task System...');
  for i:=0 to 255 do
    GlobalMem[i]:=Nil;
  for i:=0 to 31 do
    TaskList[i]:=Nil;
  MainTask:=Nil;
end;

procedure StartTask(name: string);
var
  i: integer;
begin
  for i:=0 to 31 do
    if TaskList[i] = Nil then
    begin
      New(TaskList[i]);
      TaskList[i]^.name:=name;
      TaskList[i]^.state:=tskRun;
      if LoadEXC(name+'.EXC', TaskList[i]) then
        MainTask:=TaskList[i]
      else
      begin
        Dispose(TaskList[i]);
        TaskList[i]:=Nil;
      end;
      Exit;
    end;
end;

procedure SetState(state: TaskState);
begin
  if MainTask = Nil then
    Exit;
  MainTask^.state:=state;
end;

procedure LineInput(prompt, s: string);
begin
  InputConfig.prompt:=prompt;
  InputConfig.line:=s;
  SetState(tskInput);
  WriteOut(InputConfig.prompt+InputConfig.line);
end;

procedure CommandInput(prompt: string);
begin
  InputConfig.prompt:=prompt;
  InputConfig.line:='';
  InputConfig.mode:=icCommand;
  SetState(tskInput);
  WriteOut(InputConfig.prompt);
end;

function ProcessInput(key, sp: byte): Boolean;
var
  s: string;
begin
  if (InputConfig.mode = icLine) or (InputConfig.mode = icCommand) then
    s:=InputConfig.line
  else
    s:=InputConfig.param;
  ProcessInput:=False;
  if Length(s) > 25 then
    s[0]:=chr(25);
  if (key > 96) and (key < 123) then
  begin
    s:=s+chr(key-32);
    ProcessInput:=True;
  end
  else if sp = 83 then
  begin
    s:='';
    ProcessInput:=True;
  end
  else if key = 8 then
  begin
    if Length(s) > 0 then
      s[0]:=chr(ord(s[0])-1);
    ProcessInput:=True;
  end
  else if (key > 31) and (key < 127) then
  begin
    if key = 32 then
    begin
      if InputConfig.mode = icCommand then
      begin
        InputConfig.mode:=icParam;
        ProcessInput:=True;
        Exit;
      end;
    end;
    s:=s+chr(key);
    ProcessInput:=True;
  end;
  if (InputConfig.mode = icLine) or (InputConfig.mode = icCommand) then
    InputConfig.line:=s
  else
    InputConfig.param:=s;
end;

procedure ShowClock;
var
  scr: PScreen;
  h, m, s, s100: word;
  s1, s2: string;
begin
  scr:=Nil;
  GetScreen(scr);
  DrawBorder('System Clock');
  GetTime(h,m,s,s100);
  Str(h,s1);
  Str(m,s2);
  SetTextStyle(SmallFont, HorizDir, 10);
  OutTextXY(80,50,s1+':'+s2);
  ReadLn;
  PutScreen(scr);
  FreeScreen(scr);
end;

procedure FreeTask(var Task: PTask);
var
  i: integer;
begin
  if Task = Nil then
    Exit;
  for i:=0 to 31 do
    if Task^.segs[i] <> Nil then
      Dispose(Task^.segs[i]);
  Dispose(Task);
  Task:=Nil;
end;

procedure SysRoutine(Task: PTask; syscall: Byte);
begin
  Case syscall of
    $a0: InitCoder;
    $a1: DoneCoder;
    $a2: ProcessLine(Task, InputConfig.line, InputConfig.param);
    $ff: Task^.state:=tskStop;
  end;
end;

procedure IsReg(Task: PTask; r1, r2, jmp: byte);
begin
  if GetReg(Task, r1) = r2 then
    Task^.cp:=jmp;
end;

procedure RunTask(Task: PTask);
var
  op: Byte;
begin
  op:=Task^.segs[Task^.cseg]^.data[Task^.cp];
  Inc(Task^.cp);
  if SystemConfig^.DramaMode then
    Delay(500);
  Case op of
    $10: SetReg(Task, GetParam(Task), GetParam(Task));
    $11: IsReg(Task, GetParam(Task), GetParam(Task), GetParam(Task));
    $20: WriteLine(GetSegString(Task));
    $22: LineInput(GetSegString(Task), '');
    $23: CommandInput(GetSegString(Task));
    $60: SetGraphMode(GetParam(Task));
    $61: DrawBorder(GetSegString(Task));
    $62: DrawButton(GetParam(Task), GetParam(Task), GetSegString(Task));
    $80: Task^.cp:=GetParam(Task);
    $ff: SysRoutine(Task, GetParam(Task));
  end;
end;

function MonRunEXC(cmd, param: string): Boolean;
var
  f: File of TDoorSegment;
begin
  MonRunEXC:=False;
  Assign(f, cmd+'.EXC');
  {$I-}
  Reset(f);
  {$I+}
  If IOResult <> 0 then
    Exit;
  Close(f);
  MonRunEXC:=True;
  StartTask(cmd);
end;

procedure WriteTask(Task: PTask);
var
  s: string;
begin
  Case Task^.state of
    tskRun: s:='Running';
    tskStop: s:='Stopped';
    tskSleep: s:='Sleeping';
    tskSuspend: s:='Suspended';
    tskInput: s:='InputMode';
  end;
  WriteLine(Task^.name+': '+s);
end;

procedure SetMode(mode: integer);
begin
  SetGraphMode(mode);
  SystemConfig^.GraphMode:=mode;
  Move(SystemConfig^, Reg[0]^.data, SizeOf(SystemConfig^));
end;

procedure ShowConfig;
var
  s: string;
begin
  Str(SystemConfig^.GraphDriver, s);
  WriteLine('GraphDriver='+s);
  Str(SystemConfig^.GraphMode, s);
  WriteLine('GraphMode='+s);
  if SystemConfig^.BootGUI then
    WriteLine('BootGUI=YES');
  if SystemConfig^.DramaMode then
    WriteLine('DramaMode=YES');
  if SystemConfig^.ExitMode = emMonitor then
    WriteLine('EMODE=MONITOR')
  else if SystemConfig^.ExitMode = emHalt then
    WriteLine('EMODE=HALT');
end;

procedure ShowTasks;
var
  scr: PScreen;
  i, tcount: integer;
  mfree: string;
begin
  scr:=Nil;
  Str(MemAvail, mfree);
  GetScreen(scr);
  SetGraphMode(SystemConfig^.GraphMode);
  DrawBorder('DOORS2 Running Tasks Viewer');
  WriteLine('DOORS2 Version '+VER_STRING);
  WriteLine(mfree+' bytes of memory available.');
  ShowConfig;
  ReadLn;
  ClearBorder(2,0);
  WriteLine('Task List');
  WriteLine('====================');
  tcount:=0;
  for i:=0 to 31 do
    if TaskList[i] <> Nil then
    begin
      Inc(tcount);
      WriteTask(TaskList[i]);
    end;
  if tcount > 0 then
  begin
    Str(tcount, mfree);
    WriteLine('Total Tasks: '+mfree);
    ReadLn;
  end;
  PutScreen(scr);
  FreeScreen(scr);
end;

procedure MonRunTask;
var
  hdr: PEXCHeader;
begin
  hdr:=@MainTask^.segs[0]^.data;
  if hdr^.gui then
  begin
    WriteLine(' * CANNOT RUN GUI TASK HERE');
    Exit;
  end;
  repeat
    RunTask(MainTask);
    if MainTask^.state <> tskRun then
      MainTask^.state:=tskStop;
  until MainTask^.state = tskStop;
end;

procedure MonRoutine(cmd, param: string);
var
  i, c, cnt: integer;
  s: string;
begin
  if cmd = 'CLEAR' then
    ClearBorder(1,0)
  else if cmd = 'MODE' then
  begin
    Val(param, i, c);
    if (c = 0) and (i > -1) and (i < 4) then
      SetMode(i);
    DrawBorder('DOOR2 Monitor System');
  end
  else if cmd = 'CONFIG' then
    ShowConfig
  else if cmd = 'TIME' then
    ShowClock
  else if cmd = 'VER' then
    WriteLine('DOOR2 Version '+VER_STRING)
  else if cmd = 'TASK' then
  begin
    if MainTask <> Nil then
      WriteLine('Main Task = '+MainTask^.name)
    else
      WriteLine('No Main Task running.');
  end
  else if cmd = 'TASKS' then
  begin
    for i:=0 to 31 do
      if TaskList[i] <> Nil then
        WriteTask(TaskList[i]);
  end
  else if cmd = 'HALT' then
    SysState.running:=False
  else if cmd = 'REGISTRY' then
  begin
    for i:=0 to 15 do
      if Reg[i] <> Nil then
      begin
        Str(i, s);
        WriteLine(s+': '+Reg[i]^.key);
      end;
  end
  else if cmd = 'MEM' then
  begin
    Str(MemAvail, s);
    WriteLine('Memory available: '+s);
    for i:=0 to 31 do
      if TaskList[i] <> Nil then
      begin
        cnt:=0;
        for c:=0 to 31 do
          if TaskList[i]^.segs[c] <> Nil then
            Inc(cnt);
        Str(cnt, s);
        WriteLine(TaskList[i]^.name+': '+s+' Segments loaded.');
      end;
    cnt:=0;
    for i:=0 to 255 do
      if GlobalMem[i] = Nil then
        Inc(cnt);
    Str(cnt, s);
    WriteLine(s+' Pages of global memory free.');
  end
  else if cmd = 'INIT' then
  begin
    i:=-1;
    if param = '' then
      i:=0
    else if param = 'KEYMAP' then
      i:=1
    else
      WriteLine(' * BAD INIT!');
    if i > -1 then
      if Reg[i] <> Nil then
      begin
        Dispose(Reg[i]);
        Reg[i]:=Nil;
      end
      else
        WriteLine(' * NO DATA TO INIT!');
  end
  else if cmd = 'DRAMA' then
  begin
    if SystemConfig^.DramaMode then
      SystemConfig^.DramaMode:=False
    else
      SystemConfig^.DramaMode:=True;
    Move(SystemConfig^, Reg[0]^.data, SizeOf(SystemConfig^));
  end
  else if cmd = 'EMODE' then
  begin
    if param = 'NONE' then
      SystemConfig^.ExitMode:=emNone
    else if param = 'MONITOR' then
      SystemConfig^.ExitMode:=emMonitor
    else if param = 'HALT' then
      SystemConfig^.ExitMode:=emHalt
    else
      WriteLine(' * BAD EMODE!');
    Move(SystemConfig^, Reg[0]^.data, SizeOf(SystemConfig^));
  end
  else
  begin
    if not MonRunEXC(cmd, param) then
      WriteLine(' * SYNTAX ERROR!')
    else
      MonRunTask;
  end;
end;

procedure StartMonitor;
var
  scr: PScreen;
  running: Boolean;
  ic: TInputConfig;
  key, sp: Byte;
begin
  scr:=Nil;
  Move(InputConfig, ic, SizeOf(InputConfig));
  GetScreen(scr);
  SetGraphMode(SystemConfig^.GraphMode);
  DrawBorder('DOORS2 System Monitor');
  WriteLine('Monitor Ready.');
  InputConfig.prompt:='*';
  InputConfig.line:='';
  InputConfig.mode:=icCommand;
  WriteOut('*');
  running:=True;
  repeat
    KeyStat(key, sp);
    if (key <> 0) or (sp <> 0) then
    begin
      if (sp = ActionKeys.abort) or (sp = ActionKeys.monitor) then
        running:=False
      else if sp = ActionKeys.clock then
        ShowClock
      else if sp = ActionKeys.tasks then
        ShowTasks;
      if ProcessInput(key, sp) then
        if InputConfig.mode = icParam then
          WriteOut(InputConfig.prompt+InputConfig.line+' '+InputConfig.param)
        else
          WriteOut(InputConfig.prompt+InputConfig.line);
      if key = 13 then
        if InputConfig.line = 'EXIT' then
          running:=False
        else if InputConfig.line <> '' then
        begin
          IncRow;
          MonRoutine(InputConfig.line, InputConfig.param);
          if not SysState.running then
            running:=False;
          InputConfig.line:='';
          InputConfig.param:='';
          InputConfig.mode:=icCommand;
          WriteOut(InputConfig.prompt);
        end;
    end;
  until not running;
  Move(ic, InputConfig, SizeOf(InputConfig));
  PutScreen(scr);
  FreeScreen(scr);
end;

procedure ProcessTasks;
var
  key, sp: Byte;
  i, tcount: integer;
begin
  SysState.running:=True;
  repeat
    KeyStat(key, sp);
    tcount:=0;
    for i:=0 to 31 do
      if TaskList[i] <> Nil then
      begin
        Inc(tcount);
        if TaskList[i]^.state = tskRun then
          RunTask(TaskList[i]);
        if TaskList[i]^.state = tskStop then
        begin
          if MainTask = TaskList[i] then
            MainTask:=Nil;
          FreeTask(TaskList[i]);
        end;
      end;
    if tcount = 0 then
    begin
      if SystemConfig^.ExitMode = emMonitor then
        StartMonitor
      else if SystemConfig^.ExitMode = emHalt then
        SysState.running:=False;
    end;
    if (key > 0) or (sp > 0) then
    begin
      if sp = ActionKeys.abort then
      begin
        if MainTask = Nil then
          SysState.running:=False
        else
          MainTask^.state:=tskStop;
      end
      else if sp = ActionKeys.clock then
        ShowClock
      else if sp = ActionKeys.monitor then
        StartMonitor
      else if sp = ActionKeys.tasks then
        ShowTasks
      else if sp = ActionKeys.help then
        if MainTask = Nil then
          WriteLine('MainTask=Nil');
      if (MainTask <> Nil) and (MainTask^.state = tskInput) then
      begin
        if ProcessInput(key, sp) then
          if InputConfig.mode = icParam then
            WriteOut(InputConfig.prompt+InputConfig.line+' '+InputConfig.param)
          else
            WriteOut(InputConfig.prompt+InputConfig.line);
        if key = 13 then
        begin
          IncRow;
          MainTask^.state := tskRun;
        end;
      end;
    end;
  until not SysState.running;
  for i:=0 to 31 do
    if TaskList[i] <> Nil then
      FreeTask(TaskList[i]);
end;

end.
