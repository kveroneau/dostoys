unit exc;

interface

uses actkeys;

type
  PMemory = ^TMemory;
  TMemory = Array[0..63] of Byte;

  SegmentType = (segNone, segCode, segData, segString);
  PDoorSegment = ^TDoorSegment;
  TDoorSegment = record
    data: TMemory;
    stype: SegmentType;
  end;

  PSegString = ^TSegString;
  TSegString = String[60];

  PEXCHeader = ^TEXCHeader;
  TEXCHeader = record
    sig: Array[0..3] of Char;
    cseg, dseg: Byte;
    gui: Boolean;
  end;

  TaskState = (tskRun, tskSleep, tskStop, tskSuspend, tskInput);
  PTask = ^TTask;
  TTask = record
    name: string[10];
    state: TaskState;
    cseg, dseg, cp: Byte;
    binds: TActionKeys;
    segs: Array[0..31] of PDoorSegment;
  end;

const
  EXC_SIG = 'EXC*';

function LoadEXC(EXCFile: string; Task: PTask): Boolean;
function GetParam(Task: PTask): Byte;
function GetSegString(Task: PTask): TSegString;
procedure SetReg(task: PTask; r, v: byte);
function GetReg(task: PTask; r: byte): byte;

implementation

function LoadEXC(EXCFile: string; Task: PTask): Boolean;
var
  hdr: PEXCHeader;
  f: File of TDoorSegment;
  i: integer;
begin
  LoadEXC:=False;
  for i:=0 to 31 do
    Task^.segs[i]:=Nil;
  Assign(f, EXCFile);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
    Exit;
  for i:=0 to FileSize(f)-1 do
  begin
    New(Task^.segs[i]);
    Read(f, Task^.segs[i]^);
  end;
  Close(f);
  hdr:=@Task^.segs[0]^.data;
  if hdr^.sig <> EXC_SIG then
    Exit;
  Task^.cseg:=hdr^.cseg;
  Task^.dseg:=hdr^.dseg;
  Task^.cp:=0;
  FillChar(Task^.binds, SizeOf(Task^.binds), 0);
  LoadEXC:=True;
end;

function GetParam(Task: PTask): Byte;
begin
  GetParam:=Task^.segs[Task^.cseg]^.data[Task^.cp];
  Inc(Task^.cp);
end;

function GetSegString(Task: PTask): TSegString;
var
  ss: Byte;
  s: TSegString;
begin
  ss:=GetParam(Task);
  Move(Task^.segs[ss]^.data, s, SizeOf(TSegString));
  GetSegString:=s;
end;

procedure SetReg(task: PTask; r, v: byte);
begin
  task^.segs[task^.dseg]^.data[r]:=v;
end;

function GetReg(task: PTask; r: byte): byte;
begin
  GetReg:=task^.segs[task^.dseg]^.data[r];
end;

end.

