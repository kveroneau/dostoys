unit ems;

interface

uses dos;

type
  PEmsPage = ^TEmsPage;
  TEmsPage = array[0..$4000] of byte;

var
  EmsInstalled: boolean;
  EmsAddr: word;
  EmsMem: PEmsPage;

function EmsPages(var total, avail: word): word;
function EmsAlloc(pages: word; var handle: word): word;
function EmsRealloc(handle, pages: word): word;
function EmsMap(handle, logical, physical: word): word;
function EmsFree(handle: word): word;

implementation

var
  regs: Registers;

function EmsPages(var total, avail: word): word;
begin
  regs.AH:=$42;
  Intr($67, regs);
  avail:=regs.BX;
  total:=regs.DX;
  EmsPages:=regs.AH;
end;

function EmsAlloc(pages: word; var handle: word): word;
begin
  regs.AH:=$43;
  regs.BX:=pages;
  Intr($67, regs);
  handle:=regs.DX;
  EmsAlloc:=regs.AH;
end;

function EmsRealloc(handle, pages: word): word;
begin
  regs.AH:=$51;
  regs.DX:=handle;
  regs.BX:=pages;
  Intr($67, regs);
  EmsRealloc:=regs.AH;
end;

function EmsMap(handle, logical, physical: word): word;
begin
  regs.AH:=$44;
  regs.AL:=physical;
  regs.BX:=logical;
  regs.DX:=handle;
  Intr($67, regs);
  EmsMap:=regs.AH;
end;

function EmsFree(handle: word): word;
begin
  regs.AH:=$45;
  regs.DX:=handle;
  Intr($67, regs);
  EmsFree:=regs.AH;
end;

function ems_installed: boolean;
var
  DevName: string[8];
  i: word;
begin
  DevName:='';
  regs.AH:=$35;
  regs.AL:=$67;
  Intr($21, regs);
  for i:=0 to 7 do
    DevName:=DevName+chr(mem[regs.ES:i+$0a]);
  if DevName <> 'EMMXXXX0' then
  begin
    ems_installed:=False;
    Exit;
  end;
  ems_installed:=True;
  regs.AH:=$41;
  Intr($67, regs);
  if regs.AH = 0 then
  begin
    EmsAddr:=regs.BX;
    EmsMem:=Ptr(EmsAddr, 0);
  end
  else
    ems_installed:=False;
end;

begin
  EmsInstalled:=ems_installed;
end.