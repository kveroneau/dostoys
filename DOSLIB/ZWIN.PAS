unit zwin;

interface

uses crt;

procedure OpenWindow(title: string; x,y,w,h: Byte);

implementation

function Border(ch: char; count: byte): string;
var
  b: string;
begin
  FillChar(b,count,ch);
  b[0]:=chr(count-1);
  Border:=b;
end;

procedure OpenWindow(title: string; x,y,w,h: Byte);
var
  i: Byte;
  void: string;
begin
  if LastMode = CO80 then
    Window(1,1,80,25)
  else
    Window(1,1,40,25);
  {TextColor(White);}
  FillChar(void,w-2,' ');
  void[0]:=chr(w-3);
  GotoXY(x,y);
  Write(Border('=',w));
  GotoXY(x,y+h);
  Write(Border('-',w));
  for i:=y+1 to (y+h)-1 do
  begin
    GotoXY(x,i);
    Write('|',void,'|');
  end;
  GotoXY(x+2,y);
  Write(title);
  Window(x+1,y+1,x+w-3,y+h-1);
end;

end.