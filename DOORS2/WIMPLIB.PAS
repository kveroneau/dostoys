unit wimplib;

interface

uses graph, grlib, actkeys;

procedure InputBox(title: string; var s: string);
procedure MsgBox(title, msg: string);

implementation

procedure InputBox(title: string; var s: string);
var
  buf: PScreen;
  key, sp: byte;
  draw: boolean;
begin
  buf:=Nil;
  GetScreen(buf);
  SetFillStyle(SolidFill, 1);
  SetColor(3);
  Bar3D(50, 50, 200, 61, 20, True);
  SetColor(2);
  SetTextStyle(DefaultFont, HorizDir, 1);
  OutTextXY(70, 40, title);
  SetTextStyle(SmallFont, HorizDir, 4);
  SetColor(0);
  if s <> '' then
    draw:=True
  else
    draw:=False;
  repeat
    if draw then
    begin
      if Length(s) > 25 then
        s[0]:=chr(25);
      Bar(51, 51, 199, 60);
      OutTextXY(51, 50, s);
      draw:=False;
    end;
    KeyStat(key, sp);
    if (key > 96) and (key < 123) then
    begin
      s:=s+chr(key-32);
      draw:=True;
    end
    else if sp = 83 then
    begin
      s:='';
      draw:=True;
    end
    else if key = 8 then
    begin
      s[0]:=chr(ord(s[0])-1);
      draw:=True;
    end
    else if (key > 31) and (key < 127) then
    begin
      s:=s+chr(key);
      draw:=True;
    end;
  until key = 13;
  PutScreen(buf);
  FreeScreen(buf);
end;

procedure MsgBox(title, msg: string);
var
  buf: PScreen;
begin
  buf:=Nil;
  GetScreen(buf);
  SetFillStyle(SolidFill, 1);
  SetColor(3);
  Bar3D(50, 50, 200, 61, 20, True);
  SetColor(2);
  SetTextStyle(DefaultFont, HorizDir, 1);
  OutTextXY(70, 40, title);
  SetTextStyle(SmallFont, HorizDir, 4);
  SetColor(0);
  OutTextXY(51, 50, msg);
  ReadLn;
  PutScreen(buf);
  FreeScreen(buf);
end;

end.
