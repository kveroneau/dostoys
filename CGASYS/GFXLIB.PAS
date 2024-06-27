unit gfxlib;

interface

uses graph;

type
  PSprite = ^TSprite;
  TSprite = array[0..21] of byte;
  PSpriteSet = ^TSpriteSet;
  TSpriteSet = array[0..31] of TSprite;

procedure DrawImage(ox,oy: byte; fname: string);

implementation

procedure DrawImage(ox,oy: byte; fname: string);
var
  f: file of char;
  c: char;
  x,y: integer;
begin
  x:=0;
  y:=0;
  Assign(f,fname);
  Reset(f);
  repeat
    read(f,c);
    case c of
      '0': PutPixel(ox+x,oy+y,0);
      '1': PutPixel(ox+x,oy+y,1);
      '2': PutPixel(ox+x,oy+y,2);
      '3': PutPixel(ox+x,oy+y,3);
      #13: Inc(y);
      #10: x:=-1;
    end;
    Inc(x);
  until eof(f);
  Close(f);
end;

end.
