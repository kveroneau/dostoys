unit advenlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PItem = ^TItem;
  TItem = Array[1..20] of char;
  PWorld = ^TWorld;
  TWorld = record
    m: TItem;
    d1: Array[1..80] of char;
    d2: Array[1..80] of char;
    d3: Array[1..80] of char;
    mi1: Array[1..40] of char;
    mi2: Array[1..40] of char;
    mi3: Array[1..40] of char;
    m1: word;
    m2: word;
    m3: word;
    n: word;
    s: word;
    e: word;
    w: word;
    get: TItem;
    use: TItem;
    use1: word;
    use2: word;
    use3: word;
  end;
  TPlayer = record
    i: word;
    dta: TItem;
  end;

  { TGameFile }

  TGameFile = class(TMemoryStream)
  private
    FLocation: integer;
    FWorld: PWorld;
    FDesignMode: Boolean;
    function GetGameTitle: string;
    procedure SetGameTitle(value: string);
    procedure MoveLocation(value: integer);
  public
    property GameTitle: string read GetGameTitle write SetGameTitle;
    property Location: integer read FLocation write MoveLocation;
    property DesignMode: Boolean write FDesignMode;
    property World: PWorld read FWorld write FWorld;
    destructor Destroy; override;
    procedure Initialize;
    procedure ListRooms(lst: TStrings);
    procedure SaveAt(Loc: integer);
  end;

var { DS = 1857 }
  GameFile: string[12];
  world: PWorld;
  f: File of TWorld;
  Loc: Word;
  inv: Array[1..20] of PItem;
  EmptyStr: TItem;

function rtrim(s: string): string;
procedure MoveLocation(l: Word);
function DropItem(itm: TItem): Boolean;
function HaveItem(itm: TITem): Boolean;
function TakeItem(itm: TItem): Boolean;

implementation

function rtrim(s: string): string;
var
  i,c: word;
  tmp: string;
begin
  rtrim:='';
  if s[1] = ' ' then
    Exit;
  c:=255;
  tmp:='';
  for i:=Length(s) downto 1 do
    if s[i] <> ' ' then
      if c = 255 then
        c:=i;
  for i:=1 to c do
    tmp:=tmp+s[i];
  rtrim:=tmp;
end;

procedure MoveLocation(l: Word);
begin
  Seek(f, l);
  Read(f, world^);
  if world^.use3 <> l then
    if HaveItem(world^.use) then
      MoveLocation(world^.use3);
  Loc:=l;
end;

function DropItem(itm: TItem): Boolean;
var
  i: Word;
begin
  Result:=False;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      if inv[i]^ = itm then
      begin
        Dispose(inv[i]);
        inv[i]:=Nil;
        Result:=True;
      end;
end;

function HaveItem(itm: TITem): Boolean;
var
  i: Word;
begin
  Result:=False;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      if inv[i]^ = itm then
        Result:=True;
end;

function TakeItem(itm: TItem): Boolean;
var
  i: Word;
begin
  Result:=False;
  if HaveItem(itm) then
    Exit;
  for i:=1 to 20 do
    if inv[i] = Nil then
    begin
      New(inv[i]);
      inv[i]^:=itm;
      Result:=True;
      Exit;
    end;
end;

procedure InitAdven;
var
  i: Word;
begin
  Loc:=0;
  for i:=1 to 20 do
    inv[i]:=Nil;
  New(world);
  FillChar(EmptyStr, 20, ' ');
end;

procedure CleanAdven;
var
  i: Word;
begin
  Dispose(world);
  for i:=1 to 20 do
    if inv[i] <> Nil then
      Dispose(inv[i]);
end;

{ TGameFile }

function TGameFile.GetGameTitle: string;
var
  hdr: PWorld;
begin
  Position:=0;
  New(hdr);
  Read(hdr^, SizeOf(hdr^));
  Result:=hdr^.d1;
  Dispose(hdr);
end;

procedure TGameFile.SetGameTitle(value: string);
var
  hdr: PWorld;
begin
  Position:=0;
  New(hdr);
  Read(hdr^, SizeOf(hdr^));
  hdr^.d1:=value;
  Position:=0;
  Write(hdr^, SizeOf(hdr^));
  Dispose(hdr);
end;

procedure TGameFile.MoveLocation(value: integer);
begin
  if FDesignMode then
  begin
    FLocation:=value;
    Position:=value*SizeOf(TWorld);
    Read(FWorld^, SizeOf(FWorld^));
    WriteLn(FWorld^.m);
    Exit;
  end;
  Position:=FLocation*SizeOf(TWorld);
  Read(FWorld^, SizeOf(FWorld^));
  if FWorld^.use3 <> FLocation then
    MoveLocation(FWorld^.use3);
  FLocation:=value;
  Position:=value*SizeOf(TWorld);

  {Seek(f, l);
  Read(f, world^);
  if world^.use3 <> l then
    if HaveItem(world^.use) then
      MoveLocation(world^.use3);
  Loc:=l;}
end;

destructor TGameFile.Destroy;
begin
  Dispose(FWorld);
  inherited Destroy;
end;

procedure TGameFile.Initialize;
begin
  New(FWorld);
  FDesignMode:=False;
end;

procedure TGameFile.ListRooms(lst: TStrings);
var
  i: integer;
  w: PWorld;
begin
  New(w);
  Position:=SizeOf(TWorld);
  i:=Size div SizeOf(TWorld);
  for i:=1 to (Size div SizeOf(TWorld))-1 do
  begin
    Read(w^, SizeOf(w^));
    WriteLn(lst.Add(w^.m));
    WriteLn(' - ', w^.m);
  end;
  Dispose(w);
end;

procedure TGameFile.SaveAt(Loc: integer);
begin
  Position:=Loc*SizeOf(TWorld);
  Write(FWorld^, SizeOf(FWorld^));
end;

initialization
  InitAdven;

finalization
  CleanAdven;
end.

