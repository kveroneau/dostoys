unit zmouse;

interface

uses dos;

procedure MouseInit;
procedure MouseShow;
procedure MouseHide;
function MouseBut: word;
procedure MouseStat(var but, x, y: word);
function MousePos(but, x, y: word): boolean;
procedure MouseDone;

implementation

var
  regs: ^Registers;

procedure mouse(axi, bxi, cxi, dxi: word);
begin
  if regs = nil then
    New(regs);
  with regs^ do
  begin
    AX:=axi;
    BX:=bxi;
    CX:=cxi;
    DX:=dxi;
  end;
  Intr($33, regs^);
end;

procedure MouseInit;
begin
  mouse(0,0,0,0);
end;

procedure MouseShow;
begin
  mouse(1,0,0,0);
end;

procedure MouseHide;
begin
  mouse(2,0,0,0);
end;

function MouseBut: word;
begin
  mouse(3,0,0,0);
  MouseBut:=regs^.bx;
end;

procedure MouseStat(var but, x, y: word);
begin
  mouse(3,0,0,0);
  with regs^ do
  begin
    but:=BX;
    x:=(CX div 8)+1;
    y:=(DX div 8)+1;
  end;
end;

function MousePos(but, x, y: word): boolean;
var
  xbut, xx, xy: word;
begin
  MouseStat(xbut, xx, xy);
  if (but = xbut) and (x = xx) and (y = xy) then
    MousePos:=True
  else
    MousePos:=False;
end;

procedure MouseDone;
begin
  MouseHide;
  Dispose(regs);
  regs:=nil;
end;

begin
  regs:=nil;
end.