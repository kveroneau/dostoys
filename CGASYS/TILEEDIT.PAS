unit TileEdit;

interface

uses graph, cgacrt, gfxlib, scrsave, cgalib, globals, utils, crt, vbd;

procedure TEStart;
procedure TEDone;
procedure TECharIn(c: char);
procedure TELineIn;

implementation

type
  TTile = array[0..7,0..7] of byte;
  TAppState = (asEdit, asAdd, asSave, asLoad, asExport, asImport);

var
  Tile: TTile;
  CurX, CurY, CurCol: byte;
  AppState: TAppState;
  ActiveVBD: PVBD;

procedure DrawSysSprites;
var
  i, x, y: integer;
begin
  x:=30;
  y:=50;
  for i:=0 to 31 do
  begin
    PutImage(x,y,SysSprites[i],0);
    if i < 10 then
      OutTextXY(x,y-10,AsStr(i));
    Inc(x,10);
  end;
end;

procedure DrawTile;
var
  x,y: byte;
begin
  for x:=0 to 7 do
    for y:=0 to 7 do
    begin
      PutPixel(30+x,100+y,Tile[x,y]);
      SetFillStyle(1,Tile[x,y]);
      Bar(50+x*8,100+y*8,50+(x*8)+8,100+(y*8)+8);
    end;
end;

procedure DrawCursor;
begin
  PutPixel(30+CurX,100+CurY,CurCol);
  SetFillStyle(1, CurCol);
  Bar(50+CurX*8, 100+CurY*8, 50+CurX*8+8, 100+CurY*8+8);
end;

procedure DrawScreen;
begin
  DrawTitle;
  Rectangle(49,99,50+7*8+9,100+7*8+9);
  DrawSysSprites;
  DrawTile;
  DrawCursor;
end;

procedure TEStart;
begin
  SaveScreen;
  SysMode:=smTileEdit;
  AppState:=asEdit;
  InputMode:=imChar;
  InputLine:='';
  AppTitle:='CGA Tile Editor';
  ClearDevice;
  FillChar(Tile, SizeOf(Tile), #0);
  CurX:=3;
  CurY:=3;
  CurCol:=1;
  DrawScreen;
  if MediaBD.active then
    ActiveVBD:=@MediaBD
  else
    ActiveVBD:=@RootBD;
end;

procedure TEDone;
begin
  RestoreScreen;
  SysMode:=smMain;
  InputMode:=imNone;
  InputLine:='';
end;

procedure RequestLine(as: TAppState);
begin
  InputMode:=imLine;
  InputLine:='';
  AppState:=as;
end;

procedure SwitchPalette(pal: byte);
begin
  SetGraphMode(pal);
  DrawScreen;
end;

procedure TECharIn(c: char);
var
  sp: byte;
begin
  if c = #27 then
  begin
    TEDone;
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
  else if c = ',' then
    Dec(CurCol)
  else if c = '.' then
    Inc(CurCol)
  else if c = ' ' then
    Tile[CurX,CurY]:=CurCol
  else if c = 'A' then
    RequestLine(asAdd)
  else if c = 'C' then
    FillChar(Tile, SizeOf(Tile), CurCol)
  else if c = 'S' then
    RequestLine(asSave)
  else if c = 'L' then
    RequestLine(asLoad)
  else if c = '0' then
    SwitchPalette(0)
  else if c = '1' then
    SwitchPalette(1)
  else if c = '2' then
    SwitchPalette(2)
  else if c = '3' then
    SwitchPalette(3)
  else if c = 'X' then
    RequestLine(asExport)
  else if c = 'I' then
    RequestLine(asImport);
  if CurCol > 3 then
    CurCol:=0;
  if CurX > 7 then
    CurX:=7;
  if CurY > 7 then
    CurY:=7;
  DrawTile;
  DrawCursor;
end;

procedure TELineIn;
var
  i,c: integer;
  f: File of TSpriteSet;
begin
  InputMode:=imChar;
  if AppState = asAdd then
  begin
    Val(InputLine,i,c);
    DrawTile;
    GetImage(30,100,37,107,SysSprites[i]);
    DrawSysSprites;
  end
  else if AppState = asSave then
  begin
    Val(InputLine,i,c);
    if not ActiveVBD^.active then
      WriteLn('VBD not active...');
    ReadVBD(ActiveVBD, 0);
    Move(SysSprites[0], ActiveVBD^.blk^, SizeOf(SysSprites));
    WriteVBD(ActiveVBD, i);
  end
  else if AppState = asLoad then
  begin
    Val(InputLine,i,c);
    ReadVBD(ActiveVBD, i);
    Move(ActiveVBD^.blk^, SysSprites[0], SizeOf(SysSprites));
    DrawSysSprites;
  end
  else if AppState = asExport then
  begin
    Assign(f, InputLine);
    Rewrite(f);
    Write(f, SysSprites);
    Close(f);
  end
  else if AppState = asImport then
  begin
    DrawImage(30, 100, InputLine);
    for i:=0 to 7 do
      for c:=0 to 7 do
        Tile[i,c]:=GetPixel(30+i,100+c);
    DrawTile;
  end;
  InputLine:='';
end;

end.
