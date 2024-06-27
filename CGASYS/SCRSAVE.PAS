unit scrsave;

interface

uses graph;

procedure SaveScreen;
procedure RestoreScreen;

implementation

var
  buf: Pointer;

procedure SaveScreen;
begin
  if buf <> Nil then
    Exit;
  GetMem(buf, ImageSize(0,0,GetMaxX,GetMaxY));
  GetImage(0,0,GetMaxX,GetMaxY, buf^);
end;

procedure RestoreScreen;
begin
  if buf = Nil then
    Exit;
  PutImage(0,0, buf^, 0);
  FreeMem(buf, ImageSize(0,0,GetMaxX,GetMaxY));
  buf:=Nil;
end;

begin
  buf:=Nil;
end.
