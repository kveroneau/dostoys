program mkcoder;

uses tasksys, exc;

var
  Segs: Array[0..6] of TDoorSegment;
  Header: PEXCHeader;

procedure SaveEXC;
var
  f: File of TDoorSegment;
  i: integer;
begin
  Assign(f, 'CODER.EXC');
  Rewrite(f);
  for i:=0 to 6 do
    Write(f, Segs[i]);
  Close(f);
end;

procedure VerifyEXC;
var
  s: Array[0..6] of TDoorSegment;
  f: File of TDoorSegment;
  i: integer;
begin
  Assign(f, 'CODER.EXC');
  Reset(f);
  for i:=0 to 6 do
    Read(f, s[i]);
  Close(f);
  Header:=@s[0].data;
  if Header^.sig = EXC_SIG then
    WriteLn('Header OK');
  WriteLn('Code Segment: ',Header^.cseg);
end;

procedure SetString(seg: Byte; s: TSegString);
var
  ss: PSegString;
begin
  Segs[seg].stype:=segString;
  ss:=@Segs[seg].data;
  ss^:=s;
end;

begin
  Header:=@Segs[0].data;
  Segs[0].stype:=segNone;
  Segs[1].stype:=segCode;
  Segs[2].stype:=segData;
  Header^.sig:=EXC_SIG;
  Header^.cseg:=1;
  Header^.dseg:=2;
  Header^.gui:=True;
  SetString(3, 'Doors Coder Interface');
  SetString(4, 'Help Text Here.');
  SetString(5, 'F1');
  SetString(6, 'Coder>');
  With Segs[Header^.cseg] do
  begin
    data[0]:=$60;
    data[1]:=$3;
    data[2]:=$61;
    data[3]:=$3;
    data[4]:=$ff;
    data[5]:=$a0;
    data[6]:=$20;
    data[7]:=$4;
    data[8]:=$62;
    data[9]:=60;
    data[10]:=130;
    data[11]:=$5;
    data[12]:=$23;
    data[13]:=$6;
    data[14]:=$ff;
    data[15]:=$a2;
    data[16]:=$11;
    data[17]:=3;
    data[18]:=$ff;
    data[19]:=22;
    data[20]:=$80;
    data[21]:=12;
    data[22]:=$ff;
    data[23]:=$a1;
    data[24]:=$ff;
    data[25]:=$ff;
  end;
  SaveEXC;
  VerifyEXC;
end.
