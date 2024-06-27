unit engine;

interface

uses maplib, graph, globals, crt;

var
  EngineScript: byte;

procedure EngineStart;
procedure EngineDone;
procedure EngineCharIn(c: char);
procedure SetPlayer(x,y: byte);

implementation

var
  PlayerX, PlayerY: integer;

procedure DrawMap;
var
  x,y: byte;
begin
  SetColor(1);
  Bar(29,49,30+31*8+9,50+15*8+9);
  for x:=0 to 31 do
    for y:=0 to 15 do
      PutImage(30+x*8, 50+y*8, SysSprites[SysMaps[0][x,y]],0);
end;

procedure DrawPlayer;
begin
  PutImage(30+PlayerX*8, 50+PlayerY*8, SysSprites[31], 0);
end;

procedure EngineStart;
begin
  SysMode:=smEngine;
  InputMode:=imChar;
  InputLine:='';
  DrawMap;
  DrawPlayer;
end;

procedure EngineDone;
begin
  SysMode:=smMain;
  InputMode:=imNone;
  InputLine:='';
end;

function ChkTile(x,y: byte): boolean;
begin
  if (SysMaps[0][x,y] = 0) or (SysMaps[1][x,y] > 0) then
    ChkTile:=True
  else
    ChkTile:=False;
end;

procedure RunMapScript(scr: byte);
begin
  if scr = 0 then
    Exit;
  if scr = 10 then
    scr:=0;
  if SysScripts[scr] = '' then
    Exit;
  EngineScript:=scr;
  SysMode:=smEngineScript;
end;

procedure EngineCharIn(c: char);
var
  sp: byte;
begin
  if c = #27 then
  begin
    EngineDone;
    Exit;
  end
  else if c = #0 then
  begin
    sp:=ord(ReadKey);
    if (sp = 72) and ChkTile(PlayerX,PlayerY-1) then
      Dec(PlayerY)
    else if (sp = 80) and ChkTile(PlayerX,PlayerY+1) then
      Inc(PlayerY)
    else if (sp = 75) and ChkTile(PlayerX-1,PlayerY) then
      Dec(PlayerX)
    else if (sp = 77) and ChkTile(PlayerX+1,PlayerY) then
      Inc(PlayerX);
    if PlayerX < 0 then
      PlayerX:=0
    else if PlayerX > 31 then
      PlayerX:=31
    else if PlayerY < 0 then
      PlayerY:=0
    else if PlayerY > 15 then
      PlayerY:=15;
    DrawMap;
    DrawPlayer;
    RunMapScript(SysMaps[1][PlayerX,PlayerY]);
  end;
end;

procedure SetPlayer(x,y: byte);
begin
  PlayerX:=x;
  PlayerY:=y;
  DrawMap;
  DrawPlayer;
end;

begin
  PlayerX:=10;
  PlayerY:=10;
end.
