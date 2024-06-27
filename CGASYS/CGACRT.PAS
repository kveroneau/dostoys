unit cgacrt;

interface

uses graph, dos;

procedure AssignCGA(var f: Text);
procedure SetXY(x,y: byte);
function CGAX: byte;
function CGAY: byte;

implementation

var
  Row, Col: Byte;

function CGAX: byte;
begin
  CGAX:=Col;
end;

function CGAY: byte;
begin
  CGAY:=Row;
end;

procedure CGACharOut(c: char);
begin
  OutTextXY(Col*8,Row*8,c);
  Inc(Col);
  if Col > 40 then
  begin
    Inc(Row);
    Col:=0;
  end;
end;

{$F+}
function CGAOutput(var f: TextRec): integer;
var
  p: word;
  s: string;
  b: byte;
begin
  s[0]:=#0;
  with f do
  begin
    p:=0;
    while p<BufPos do
    begin
      b:=byte(BufPtr^[p]);
      Inc(p);
      if b = 13 then
        Inc(Row)
      else if b = 10 then
        Col:=0
      else
        CGACharOut(chr(b));
    end;
    BufPos:=0;
    s[0]:=chr(p);
  end;
  CGAOutput:=0;
end;

function CGAIgnore(var f: TextRec): integer;
begin
  CGAIgnore:=0;
end;

function CGAOpen(var f: TextRec): integer;
begin
  with f do
  begin
    if Mode = fmInput then
    begin
      CGAOpen:=1;
      Exit;
    end
    else
    begin
      Mode:=fmOutput;
      InOutFunc:=@CGAOutput;
      FlushFunc:=@CGAOutput;
    end;
    CloseFunc:=@CGAIgnore;
  end;
  CGAOpen:=0;
end;
{$F-}

procedure AssignCGA(var f: Text);
begin
  with TextRec(f) do
  begin
    Handle:=$ffff;
    Mode:=fmClosed;
    BufSize:=SizeOf(Buffer);
    BufPtr:=@Buffer;
    OpenFunc:=@CGAOpen;
    Name[0]:=#0;
  end;
  Row:=0;
  Col:=0;
end;

procedure SetXY(x,y: byte);
begin
  Col:=x;
  Row:=y;
end;

end.
