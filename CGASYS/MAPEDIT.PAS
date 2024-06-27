unit MapEdit;

interface

uses graph, cgacrt, gfxlib, scrsave, cgalib, globals, crt, vbd, utils,
     maplib;

procedure MEStart;
procedure MEDone;
procedure MECharIn(c: char);
procedure MELineIn;

implementation

type
  TAppState = (asEdit, asSave, asLoad, asExport);

var
  AppState: TAppState;
  Maps: TMapSet;
  CurX, CurY, CurTile, CurMap: byte;
  ActiveVBD: PVBD;

procedure DrawMap;
var
  x,y: byte;
begin
  SetColor(1);
  Bar(30, 50, 30+31*8+8, 50+15*8+8);
  SetColor(2);
  for x:=0 to 31 do
    for y:=0 to 15 do
    begin
      if CurMap = 0 then
        PutImage(30+x*8, 50+y*8, SysSprites[Maps[CurMap][x,y]],0)
      else
      begin
        if Maps[CurMap][x,y] = 0 then
          OutTextXY(30+x*8, 50+y*8, '.')
        else
          OutTextXY(30+x*8, 50+y*8, AsStr(Maps[CurMap][x,y]));
      end;
    end;
end;

procedure DrawCursor;
begin
  SetColor(3);
  if CurMap = 0 then
    PutImage(30+CurX*8,50+CurY*8,SysSprites[CurTile],0)
  else
  begin
    if CurTile = 0 then
      OutTextXY(30+CurX*8, 50+CurY*8, '.')
    else
      OutTextXY(30+CurX*8, 50+CurY*8, AsStr(CurTile));
  end;
end;

procedure DrawScreen;
begin
  SetColor(1);
  DrawTitle;
  SetColor(2);
  Rectangle(29,49,30+31*8+9,50+15*8+9);
  DrawMap;
  DrawCursor;
end;

procedure MEStart;
begin
  SaveScreen;
  SysMode:=smMapEdit;
  AppState:=asEdit;
  InputMode:=imChar;
  InputLine:='';
  AppTitle:='CGA System Map Editor';
  ClearDevice;
  CurX:=0;
  CurY:=0;
  CurTile:=0;
  CurMap:=0;
  FillChar(Maps, SizeOf(Maps), #0);
  DrawScreen;
  if MediaBD.active then
    ActiveVBD:=@MediaBD
  else
    ActiveVBD:=@RootBD;
end;

procedure MEDone;
begin
  RestoreScreen;
  SysMode:=smMain;
  InputMode:=imNone;
  InputLine:='';
end;

procedure RequestLine(as: TAppState);
begin
  AppState:=as;
  InputLine:='';
  InputMode:=imLine;
end;

procedure MECharIn(c: char);
var
  sp: byte;
begin
  if c = #27 then
  begin
    MEDone;
    Exit;
  end
  else if c = #0 then
  begin
    sp:=ord(ReadKey);
    case sp of
      72: Dec(CurY);
      80: Inc(CurY);
      75: Dec(CurX);
      77: Inc(CurX);
    end;
  end
  else if c = ' ' then
    Maps[CurMap][CurX,CurY]:=CurTile
  else if c = ',' then
    Dec(CurTile)
  else if c = '.' then
    Inc(CurTile)
  else if c = '<' then
    Dec(CurMap)
  else if c = '>' then
    Inc(CurMap)
  else if c = 'S' then
    RequestLine(asSave)
  else if c = 'L' then
    RequestLine(asLoad)
  else if c = 'X' then
    RequestLine(asExport)
  else if c = 'C' then
    FillChar(Maps[0], SizeOf(Maps[0]), CurTile);
  if CurTile > 31 then
    CurTile:=0;
  if CurMap > 1 then
    CurMap:=0;
  DrawMap;
  DrawCursor;
  SetXY(0,23);
  Bar(0,23*8,100,23*8+8);
  Write(CurX, ', ',CurY);
end;

procedure MELineIn;
var
  i,c: integer;
  f: File of TMapSet;
begin
  InputMode:=imChar;
  if AppState = asSave then
  begin
    Val(InputLine,i,c);
    ReadVBD(ActiveVBD, 0);
    Move(Maps[0], ActiveVBD^.blk^, SizeOf(Maps));
    WriteVBD(ActiveVBD, i);
  end
  else if AppState = asLoad then
  begin
    Val(InputLine,i,c);
    ReadVBD(ActiveVBD, i);
    Move(ActiveVBD^.blk^, Maps[0], SizeOf(Maps));
    DrawMap;
    DrawCursor;
  end
  else if AppState = asExport then
  begin
    Assign(f, InputLine);
    Rewrite(f);
    Write(f, Maps);
    Close(f);
  end;
end;

end.
