unit memsys;

interface

type
  PSegment = ^TSegment;
  TSegment = array[0..1023] of byte;
  TMemory = array[0..63] of PSegment;

var
  SysMemory: TMemory;

procedure FreeAll;
function NextFree: byte;
procedure FreeAndNil(var seg: PSegment);

implementation

procedure FreeAll;
var
  i: integer;
begin
  for i:=0 to 63 do
    if SysMemory[i] <> Nil then
      Dispose(SysMemory[i]);
end;

function NextFree: byte;
var
  i: integer;
begin
  for i:=0 to 63 do
    if SysMemory[i] = Nil then
    begin
      NextFree:=i;
      Exit;
    end;
  NextFree:=255;
end;

procedure FreeAndNil(var seg: PSegment);
begin
  if seg = Nil then
    Exit;
  Dispose(seg);
  seg:=Nil;
end;

begin
  FillChar(SysMemory, SizeOf(SysMemory), #0);
end.
