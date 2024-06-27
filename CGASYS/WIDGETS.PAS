unit widgets;

interface

uses dos, graph, utils, globals;

procedure DrawClock(x,y: byte);
procedure DrawStats(x,y: byte);

implementation

procedure DrawClock(x,y: byte);
var
  hour, min, s1, s2: word;
begin
  GetTime(hour, min, s1, s2);
  SetFillStyle(0,0);
  Bar(x,y,x+70,y+8);
  OutTextXY(x,y, AsStr(hour)+':'+AsStr(min));
end;

procedure DrawStats(x,y: byte);
begin
  Bar(x,y,x+40,y+8);
  case InputMode of
    imNone: OutTextXY(x,y,'X');
    imChar: OutTextXY(x,y,'C');
    imLine: OutTextXY(x,y,'L');
  end;
  Inc(x,8);
  case SysMode of
    smMain: OutTextXY(x,y,'.');
    smShell: OutTextXY(x,y,'>');
    smRun: OutTextXY(x,y,'@');
    smMedia: OutTextXY(x,y,'#');
  end;
  Inc(x,8);
  case InputCaps of
    True: OutTextXY(x,y,'C');
    False: OutTextXY(x,y,'c');
  end;
  Inc(x,8);
  case SysVerbose of
    True: OutTextXY(x,y,'V');
    False: OutTextXY(x,y,'v');
  end;
end;

end.
