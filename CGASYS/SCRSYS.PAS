unit scrsys;

interface

uses memsys, cgacrt, cgalib, graph, globals, vbd, gfxlib, utils, TileEdit,
     MapEdit, maplib, engine, scrsave;

type
  PScript = ^TScript;
  TScript = array[0..1023] of char;

  TTaskState = (tsNew, tsRunning, tsChar, tsInput, tsDone, tsError, tsLine);

  PTask = ^TTask;
  TTask = record
    name: string[20];
    state: TTaskState;
    data: PScript;
    pc: word;
    cb: string[10];
    et: char;
  end;

var
  SysTasks: array[0..31] of PTask;

procedure AddTask(tname: string; mem: PSegment);
procedure RunTasks;
procedure DoRunScript;
procedure ScriptLine(line: string);

procedure ClearTitle;
procedure MountMedia(fname: string);

implementation

var
  ActiveVBD: PVBD;
  VBDBlock: word;
  LineTask: PTask;
  StrVar: string;
  IntVar: integer;

const
  ET = '~';

function FreeTask: byte;
var
  i: integer;
begin
  for i:=0 to 31 do
    if SysTasks[i] = Nil then
    begin
      FreeTask:=i;
      Exit;
    end;
  FreeTask:=255;
end;

procedure AddTask(tname: string; mem: PSegment);
var
  tsk: byte;
begin
  tsk:=FreeTask;
  New(SysTasks[tsk]);
  with SysTasks[tsk]^ do
  begin
    name:=tname;
    state:=tsNew;
    data:=PScript(mem);
    pc:=0;
    et:='~';
  end;
end;

function GetChCode(tsk: PTask): char;
begin
  with tsk^ do
  begin
    GetChCode:=data^[pc];
    Inc(pc);
  end;
end;

function GetString(tsk: PTask; term: char): string;
var
  r: string;
  c: char;
begin
  r:='';
  repeat
    c:=GetChCode(tsk);
    if c <> term then
      r:=r+c;
  until c = term;
  if r = 'VAR' then
    GetString:=StrVar
  else if r = 'INT' then
    GetString:=AsStr(IntVar)
  else
    GetString:=r;
end;

function GetInt(tsk: PTask): integer;
var
  i, code: integer;
begin
  Val(GetString(tsk, '#'), i, code);
  GetInt:=i;
end;

procedure ClearTitle;
begin
  ClearDevice;
  DrawTitle;
  SetXY(0,6);
end;

procedure SysShell;
begin
  SysMode:=smShell;
  InputMode:=imLine;
  InputLine:='';
end;

procedure AddSysCmd(tsk: PTask);
var
  i: integer;
begin
  i:=FreeCmd;
  if i = -1 then
    Exit;
  New(SysCmds[i]);
  with SysCmds[i]^ do
  begin
    cmd:=GetString(tsk, '{');
    script:=GetString(tsk, '}');
  end;
end;

procedure SysCall(tsk: PTask; op: char);
begin
  case op of
    'S': SysShell;
    'T': TEStart;
    'M': MEStart;
    'E': EngineStart;
    'X': InputMode:=imNone;
    '!': SysRunning:=False;
    '>': AddSysCmd(tsk);
    '~': tsk^.et:=GetChCode(tsk);
  else
    WriteLn(' X Invalid SysCall: ',op);
    tsk^.state:=tsError;
  end;
end;

procedure MountMedia(fname: string);
begin
  if BootBD = @MediaBD then
  begin
    WriteLn('Media already mounted as boot.');
    Exit;
  end;
  if MediaBD.active then
    CloseVBD(@MediaBD);
  AssignVBD(@MediaBD, fname);
  ActiveVBD:=@MediaBD;
end;

procedure LoadInBlock(tsk: PTask);
var
  p: char;
begin
  p:=GetChCode(tsk);
  if p = 'V' then
    ReadVBD(ActiveVBD, IntVar)
  else if p = '#' then
    ReadVBD(ActiveVBD, GetInt(tsk))
  else
    WriteLn(' X Invalid LOAD parameter!');
end;

procedure SaveOutBlock(tsk: PTask);
var
  p: char;
begin
  p:=GetChCode(tsk);
  if p = 'V' then
    WriteVBD(ActiveVBD, IntVar)
  else if p = '#' then
    WriteVBD(ActiveVBD, GetInt(tsk))
  else
    WriteLn(' X Invalid SAVE paramater!');
end;

procedure BlockToSeg;
var
  i: word;
begin
  i:=NextFree;
  New(SysMemory[i]);
  Move(ActiveVBD^.blk^, SysMemory[i]^, BLOCK_SIZE);
  if SysVerbose then
    WriteLn('Loaded block into segment: ',i);
  IntVar:=i;
end;

procedure SegToBlock(seg: word);
begin
  Move(SysMemory[seg]^, ActiveVBD^.blk^, BLOCK_SIZE);
  if SysVerbose then
    WriteLn('Saved segment ',seg,'.');
end;

procedure VBDCall(tsk: PTask; op: char);
begin
  if ActiveVBD = Nil then
    ActiveVBD:=BootBD;
  case op of
    'R': ActiveVBD:=@RootBD;
    'M': ActiveVBD:=@MediaBD;
    'm': MountMedia(GetString(tsk, tsk^.et));
    'B': SetBootVBD(ActiveVBD, GetString(tsk, '$'));
    'I': ImportVBD(ActiveVBD, GetInt(tsk), GetString(tsk, tsk^.et));
    'i': ImportVBD(ActiveVBD, GetInt(tsk), StrVar);
    'L': LoadInBlock(tsk);
    'S': SaveOutBlock(tsk);
    '>': BlockToSeg;
    '<': SegToBlock(GetInt(tsk));
    'O': ActiveVBD^.obfuscated:=True;
    'o': ActiveVBD^.obfuscated:=False;
  else
    WriteLn(' X Invalid VBDCall: ',op);
    tsk^.state:=tsError;
  end;
end;

procedure AddSprite(x,y,idx: byte);
begin
  GetImage(x,y,x+8,y+8, SysSprites[idx]);
end;

procedure PutSprite(tsk: PTask);
var
  x,y,idx: byte;
begin
  x:=GetInt(tsk);
  y:=GetInt(tsk);
  idx:=GetInt(tsk);
  PutImage(x,y, SysSprites[idx], 0);
end;

procedure LoadMap(block: word);
begin
  ReadVBD(ActiveVBD, block);
  Move(ActiveVBD^.blk^, SysMaps[0], SizeOf(SysMaps));
end;

procedure SprCall(tsk: PTask; op: char);
begin
  case op of
    'D': DrawImage(50,50, GetString(tsk, tsk^.et));
    'A': AddSprite(50,50, GetInt(tsk));
    'P': PutSprite(tsk);
    'R': Rectangle(GetInt(tsk),GetInt(tsk),GetInt(tsk),GetInt(tsk));
    'B': Bar(GetInt(tsk),GetInt(tsk),GetInt(tsk),GetInt(tsk));
    'F': SetFillStyle(GetInt(tsk),GetInt(tsk));
    '<': Move(SysSprites[0], SysMemory[GetInt(tsk)]^, SizeOf(SysSprites));
    '>': Move(SysMemory[IntVar]^, SysSprites[0], SizeOf(SysSprites));
    'm': Move(SysMemory[IntVar]^, SysMaps[0], SizeOf(SysMaps));
    'M': LoadMap(GetInt(tsk));
    'L': SetPlayer(GetInt(tsk), GetInt(tsk));
  else
    WriteLn(' X Invalid SprCall: ',op);
    tsk^.state:=tsError;
  end;
end;

procedure RunSysScript(key: byte);
begin
  if SysScripts[key] = '' then
    Exit;
  InputLine:=SysScripts[key]+'.';
  DoRunScript;
end;

procedure ProcessTask(tsk: PTask);
var
  op: char;
begin
  op:=GetChCode(tsk);
  case op of
    '.': tsk^.state:=tsDone;
    '!': SysCall(tsk, GetChCode(tsk));
    '#': VBDCall(tsk, GetChCode(tsk));
    '%': SprCall(tsk, GetChCode(tsk));
    '$': RunSysScript(GetInt(tsk));
    'P': WriteLn(GetString(tsk, tsk^.et));
    'p': Write(GetString(tsk, tsk^.et));
    't': ClearTitle;
    'T': AppTitle:=GetString(tsk, tsk^.et);
    'C': InputCaps:=True;
    'c': InputCaps:=False;
    'X': AddTask(GetString(tsk, tsk^.et), SysMemory[GetInt(tsk)]);
    'x': AddTask(GetString(tsk, tsk^.et), SysMemory[IntVar]);
    'Z': FreeAndNil(SysMemory[GetInt(tsk)]);
    'z': FreeAndNil(SysMemory[IntVar]);
    'V': SysVerbose:=True;
    'v': SysVerbose:=False;
    'F': SetTextStyle(DefaultFont,0,1);
    'f': SetTextStyle(SmallFont,0,4);
    'i': InputLine:='';
    'I': tsk^.state:=tsLine;
    '"': SetColor(GetInt(tsk));
    #39: SetGraphMode(GetInt(tsk));
    'L': SetXY(GetInt(tsk), GetInt(tsk));
    '<': SaveScreen;
    '>': RestoreScreen;
  else
    if (op > #47) and (op < #58) then
      SysScripts[ord(op)-48]:=GetString(tsk, '`')
    else if (op <> #13) and (op <> #10) then
    begin
      WriteLn(' * Invalid ChCode: ',op, ' / ',ord(op));
      tsk^.state:=tsError;
    end;
  end;
  if tsk^.state = tsLine then
  begin
    LineTask:=tsk;
    SysMode:=smMain;
    InputMode:=imLine;
  end;
end;

procedure RunTasks;
var
  i: integer;
begin
  for i:=0 to 31 do
    if SysTasks[i] <> Nil then
      if SysTasks[i]^.state = tsRunning then
        ProcessTask(SysTasks[i])
      else if SysTasks[i]^.state = tsDone then
      begin
        Dispose(SysTasks[i]);
        SysTasks[i]:=Nil;
      end
      else if SysTasks[i]^.state = tsNew then
        SysTasks[i]^.state:=tsRunning
      else if SysTasks[i]^.state = tsError then
        SysTasks[i]^.state:=tsDone;
end;

procedure DoRunScript;
var
  i: word;
begin
  i:=NextFree;
  if i = 255 then
  begin
    WriteLn(' ! Out of segments to start task.');
    Exit;
  end;
  New(SysMemory[i]);
  Move(InputLine[1], SysMemory[i]^, Length(InputLine));
  AddTask('RUN_SCRIPT', SysMemory[i]);
end;

procedure ScriptLine(line: string);
var
  i, c: integer;
begin
  if LineTask = Nil then
    Exit;
  StrVar:=line;
  Val(line,IntVar,c);
  LineTask^.state:=tsRunning;
  LineTask:=Nil;
end;

begin
  if SysVerbose then
    WriteLn(' * Script System initializing...');
  ActiveVBD:=Nil;
  LineTask:=Nil;
  FillChar(SysTasks, SizeOf(SysTasks), #0);
end.
