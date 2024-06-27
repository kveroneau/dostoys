unit zscrsave;

{ File created on September 29th, 2021 }

interface

uses crt;

type
  TScrCell = record
    c: char;
    a: byte;
  end;
  PScrBuf = ^TScrBuf;
  TScrBuf = Array[1..25] of Array[1..80] of TScrCell;
  PScreen = ^TScreen;
  TScreen = record
    x,y,attr: byte;
    x1,y1,x2,y2: byte;
    buffer: TScrBuf;
  end;

var
  ScrBuf: PScrBuf;

procedure SaveScreen(s: PScreen);
procedure DrawScreen(s: PScreen);

implementation

procedure SaveScreen(s: PScreen);
begin
  s^.x:=WhereX;
  s^.y:=WhereY;
  s^.attr:=TextAttr;
  s^.x1:=Lo(WindMin)+1;
  s^.y1:=Hi(WindMin)+1;
  s^.x2:=Lo(WindMax)+1;
  s^.y2:=Hi(WindMax)+1;
  Move(ScrBuf^, s^.buffer[1,1], 4000);
end;

procedure DrawScreen(s: PScreen);
begin
  with s^ do
  begin
    Move(buffer[1,1], ScrBuf^, 4000);
    Window(x1,y1,x2,y2);
    TextAttr:=attr;
    GotoXY(x,y);
  end;
end;

begin
  ScrBuf:=Ptr($b800,0);
end.