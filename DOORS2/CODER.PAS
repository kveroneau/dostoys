unit coder;

interface

uses grlib, graph, actkeys, exc;

procedure InitCoder;
procedure DoneCoder;
procedure ProcessLine(Task: PTask; cmd, params: string);

implementation

var
  Segments: Array[0..31] of PDoorSegment;
  CurSeg, CurPtr: integer;
  ScrBuf: PScreen;

procedure InitCoder;
var
  i: integer;
begin
  ScrBuf:=Nil;
  for i:=0 to 31 do
    Segments[i]:=Nil;
  CurSeg:=0;
  CurPtr:=0;
  New(Segments[CurSeg]);
  Segments[CurSeg]^.stype:=segNone;
  WriteLine('Coder Initialized!');
end;

procedure DoneCoder;
var
  i: integer;
begin
  for i:=0 to 31 do
    if Segments[i] <> Nil then
      Dispose(Segments[i]);
end;

procedure SetString(seg: byte; s: TSegString);
var
  ss: PSegString;
begin
  ss:=@Segments[seg]^.data;
  Segments[seg]^.stype:=segString;
  ss^:=s;
end;

function GetString(seg: byte): TSegString;
var
  ss: PSegString;
begin
  ss:=@Segments[seg]^.data;
  GetString:=ss^;
end;

procedure ListSegments;
var
  i: integer;
  s, st: string;
begin
  for i:=0 to 31 do
    if Segments[i] <> Nil then
    begin
      Str(i, s);
      case Segments[i]^.stype of
        segNone: st:='segNone';
        segCode: st:='segCode';
        segData: st:='segData';
        segString: st:=GetString(i);
      end;
      WriteLine('Segment '+s+': '+st);
    end;
end;

procedure NewProgram;
var
  hdr: PEXCHeader;
begin
  hdr:=@Segments[CurSeg]^.data;
  hdr^.sig:=EXC_SIG;
  hdr^.cseg:=1;
  hdr^.dseg:=2;
  New(Segments[hdr^.cseg]);
  New(Segments[hdr^.dseg]);
  Segments[hdr^.cseg]^.stype:=segCode;
  Segments[hdr^.dseg]^.stype:=segData;
  hdr^.gui:=False;
  CurSeg:=hdr^.cseg;
  CurPtr:=0;
end;

procedure SaveProgram(EXCFile: string);
var
  f: File of TDoorSegment;
  i: integer;
begin
  Assign(f, EXCFile);
  Rewrite(f);
  for i:=0 to 31 do
    if Segments[i] <> Nil then
      Write(f, Segments[i]^);
  Close(f);
end;

procedure LoadProgram(EXCFile: string);
var
  f: File of TDoorSegment;
  i: integer;
begin
  Assign(f, EXCFile);
  Reset(f);
  for i:=0 to 31 do
    if Segments[i] <> Nil then
    begin
      Dispose(Segments[i]);
      Segments[i]:=Nil;
    end;
  for i:=0 to FileSize(f)-1 do
  begin
    New(Segments[i]);
    Read(f, Segments[i]^);
  end;
  Close(f);
end;

procedure ProcessLine(Task: PTask; cmd, params: string);
var
  i: integer;
  s: string;
  c: integer;
begin
  if cmd = 'EXIT' then
    SetReg(Task, 3, $ff)
  else if cmd = 'LIST' then
    ListSegments
  else if cmd = 'NEW' then
    NewProgram
  else if cmd = 'CLEAR' then
    ClearBorder(1,0)
  else if cmd = 'SEG' then
  begin
    Val(params, i, c);
    CurSeg:=i;
    CurPtr:=0;
  end
  else if cmd = 'POKE' then
  begin
    Val(params, i, c);
    Segments[CurSeg]^.data[CurPtr]:=i;
    Inc(CurPtr);
  end
  else if cmd = 'PEEK' then
  begin
    Val(params, i, c);
    Str(Segments[CurSeg]^.data[CurPtr], s);
    WriteLine(' > '+s);
    Inc(CurPtr);
  end
  else if cmd = 'PTR' then
  begin
    if params = '' then
    begin
      Str(CurPtr, s);
      WriteLine('PTR = '+s);
    end
    else
    begin
      Val(params, i, c);
      CurPtr:=i;
    end;
  end
  else if cmd = 'WRITE' then
  begin
    Val(params, i, c);
    Segments[CurSeg]^.data[CurPtr]:=$20;
    Segments[CurSeg]^.data[CurPtr+1]:=i;
    Inc(CurPtr,2);
  end
  else if cmd = 'END' then
  begin
    Segments[CurSeg]^.data[CurPtr]:=$ff;
    Segments[CurSeg]^.data[CurPtr+1]:=$ff;
    Inc(CurPtr,2);
  end
  else if cmd = 'SAVE' then
    SaveProgram(params)
  else if cmd = 'LOAD' then
    LoadProgram(params)
  else if cmd = 'STR' then
  begin
    for i:=0 to 31 do
      if Segments[i] = Nil then
      begin
        New(Segments[i]);
        SetString(i, params);
        Str(i, s);
        WriteLine('String Seg: '+s);
        Exit;
      end;
  end
  else
    WriteLine('You wrote: '+cmd+' /'+params);
end;

end.
