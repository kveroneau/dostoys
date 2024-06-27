unit grlib;

interface

uses graph, crt, syscfg;

type
  PScreen = ^TScreen;
  TScreen = record
    buf: Pointer;
    vp: ViewPortType;
    ts: TextSettingsType;
    mode: integer;
    Row, Col, BGColor, FGColor, VPWidth: integer;
  end;

procedure InitMode(GraphDriver, GraphMode: integer);
procedure GetScreen(var scr: PScreen);
procedure PutScreen(scr: PScreen);
procedure FreeScreen(var scr: PScreen);
procedure DrawBorder(title: string);
procedure DrawButton(x, y: integer; title: string);
procedure ClearBorder(fg, bg: integer);
procedure WriteLine(s: string);
procedure WriteOut(s: string);
procedure IncRow;
function EventLoop: Boolean;

implementation

var
  Screen: TScreen;

procedure InitMode(GraphDriver, GraphMode: integer);
var
  ErrorCode: integer;
begin
  InitGraph(GraphDriver, GraphMode, 'C:\TURBO');
  ErrorCode:=GraphResult;
  if ErrorCode <> grOk then
  begin
    WriteLn('Graphics error: ', GraphErrorMsg(ErrorCode));
    Halt(1);
  end;
  Screen.VPWidth:=GetMaxX;
end;

procedure GetScreen(var scr: PScreen);
begin
  if not SysState.gui_mode then
    Exit;
  if scr = Nil then
  begin
    New(scr);
    scr^.buf:=Nil;
  end;
  scr^.mode:=GetGraphMode;
  GetViewSettings(scr^.vp);
  GetTextSettings(scr^.ts);
  With Screen do
  begin
    scr^.Row:=Row;
    scr^.Col:=Col;
    scr^.FGColor:=FGColor;
    scr^.BGColor:=BGColor;
    scr^.VPWidth:=VPWidth;
  end;
  if scr^.buf = Nil then
    GetMem(scr^.buf, ImageSize(0,0,GetMaxX,GetMaxY));
  SetViewPort(0,0,GetMaxX,GetMaxY,True);
  GetImage(0,0,GetMaxX,GetMaxY,scr^.buf^);
  with scr^.vp do
    SetViewPort(x1,y1,x2,y2,clip);
end;

procedure PutScreen(scr: PScreen);
begin
  if not SysState.gui_mode then
    Exit;
  if scr = Nil then
    Exit;
  SetViewPort(0,0,GetMaxX,GetMaxY,True);
  SetGraphMode(scr^.mode);
  if scr^.buf <> Nil then
    PutImage(0,0,scr^.buf^,0);
  with scr^.vp do
    SetViewPort(x1,y1,x2,y2,clip);
  With Screen do
  begin
    Row:=scr^.Row;
    Col:=scr^.Col;
    FGColor:=scr^.FGColor;
    BGColor:=scr^.BGColor;
    VPWidth:=scr^.VPWidth;
  end;
  SetColor(scr^.FGColor);
  with scr^.ts do
  begin
    SetTextStyle(Font, Direction, CharSize);
    SetTextJustify(Horiz, Vert);
  end;
end;

procedure FreeScreen(var scr: PScreen);
begin
  if scr = Nil then
    Exit;
  if scr^.buf <> Nil then
    FreeMem(scr^.buf, ImageSize(0,0,GetMaxX,GetMaxY));
  Dispose(scr);
  scr:=Nil;
end;

procedure DrawBorder(title: string);
begin
  if not SysState.gui_mode then
  begin
    ClrScr;
    Exit;
  end;
  ClearDevice;
  SetViewPort(0,0,GetMaxX,GetMaxY,True);
  SetFillStyle(SolidFill, 1);
  SetColor(3);
  Bar3D(20, 20, GetMaxX-20, GetMaxY-20, 20, True);
  SetColor(2);
  SetTextStyle(DefaultFont, HorizDir, 1);
  OutTextXY(40, 10, title);
  SetViewPort(22, 21, GetMaxX-22, GetMaxY-22, True);
  SetTextStyle(SmallFont, HorizDir, 4);
  SetColor(0);
  With Screen do
  begin
    Row:=0;
    Col:=0;
    FGColor:=0;
    BGColor:=1;
    VPWidth:=GetMaxX-44;
  end;
end;

procedure DrawButton(x, y: integer; title: string);
begin
  if not SysState.gui_mode then
    Exit;
  SetFillStyle(SolidFill, 3);
  Bar3D(x, y, x+20, y+20, 4, True);
  SetColor(0);
  OutTextXY(x+3, y+3, title);
end;

procedure ClearBorder(fg, bg: integer);
begin
  if not SysState.gui_mode then
  begin
    ClrScr;
    Exit;
  end;
  ClearViewPort;
  SetFillStyle(SolidFill, bg);
  FloodFill(10,10,1);
  SetColor(fg);
  With Screen do
  begin
    Row:=0;
    Col:=0;
    FGColor:=fg;
    BGColor:=bg;
  end;
end;

procedure WriteLine(s: string);
begin
  if SysState.gui_mode then
    OutTextXY(0, Screen.Row*9, s)
  else
  begin
    GotoXY(Screen.Col, Screen.Row);
    WriteLn(s);
  end;
  Inc(Screen.Row);
end;

procedure WriteOut(s: string);
begin
  if not SysState.gui_mode then
  begin
    GotoXY(Screen.Col+1, Screen.Row+1);
    ClrEOL;
    Write(s);
    Exit;
  end;
  with Screen do
  begin
    SetFillStyle(SolidFill, BGColor);
    Bar(0, (Row*9), VPWidth, (Row*9)+8);
    OutTextXY(0, Row*9, s);
  end;
end;

procedure IncRow;
begin
  Inc(Screen.Row);
end;

function EventLoop: Boolean;
begin

end;

end.

