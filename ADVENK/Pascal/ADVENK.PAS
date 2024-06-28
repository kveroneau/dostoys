program advenk;

uses crt;

const
  NORTH = 10;
  SOUTH = 11;
  EAST  = 12;
  WEST  = 13;
  M_GET = 18;
  M_USE = 19;
  MSTAT = 20;

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

var { DS = 1857 }
  GameFile: string[12];
  world: PWorld;
  f: File of TWorld;
  Loc: Word;
  inv: Array[1..20] of PItem;

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

procedure mkTItem(s: string; var itm: TItem);
begin
  FillChar(itm, 20, ' ');
  Move(s[1],itm,Length(s));
end;

procedure Center(row: word; text: string);
begin
  GotoXY(41-Length(text) div 2,row);
  Write(text);
end;

procedure Color(fg,bg: word);
begin
  TextColor(fg);
  TextBackground(bg);
end;

procedure BlueScreen;
begin
  Color(7,1);
  ClrScr;
  WriteLn('Adventure Kernel v1.0');
  WriteLn('Copyright(c) 2006-2020 Kevin Veroneau'#10#13);
end;

procedure SetColor(opt,cur: word);
begin
  if opt = cur then
    Color(0,7)
  else
    Color(7,0);
end;

function DoMenu(m1,m2,m3: string; default: word): word;
var
  opt: Word;
  k: char;
  Done: Boolean;
begin
  Done:=False;
  opt:=default;
  repeat
    SetColor(opt,1);
    Center(10,m1);
    SetColor(opt,2);
    Center(12,m2);
    SetColor(opt,3);
    Center(14,m3);
    k:=ReadKey;
    if k = chr(27) then
      opt:=99;
    if k = chr(0) then
    begin
      k:=ReadKey;
      if k = chr(72) then
        Dec(opt)
      else if k = chr(80) then
        Inc(opt);
      if opt > 3 then
        opt:=3
      else if opt < 1 then
        opt:=1;
    end;
    k:=UpCase(k);
    case k of
      'N': opt:=NORTH;
      'S': opt:=SOUTH;
      'E': opt:=EAST;
      'W': opt:=WEST;
      'G': opt:=M_GET;
      'U': opt:=M_USE;
      'Z': opt:=MSTAT;
      #13: Done:=True;
    end;
    if opt > 3 then
      Done:=True;
  until Done;
  Color(7,0);
  DoMenu:=opt;
end;

function drop(itm: TItem): Boolean;
var
  i: Word;
begin
  drop:=False;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      if inv[i]^ = itm then
      begin
        Dispose(inv[i]);
        inv[i]:=Nil;
        drop:=True;
      end;
end;

function got(itm: TItem): Boolean;
var
  i: Word;
begin
  got:=False;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      if inv[i]^ = itm then
        got:=True;
end;

function take(itm: TItem): Boolean;
var
  i: Word;
  tmp: Boolean;
begin
  tmp:=True;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      if inv[i]^ = itm then
        tmp:=False;
  take:=tmp;
  if not tmp then
    Exit;
  take:=False;
  for i:=1 to 20 do
    if inv[i] = Nil then
    begin
      New(inv[i]);
      inv[i]^:=itm;
      take:=True;
      Exit;
    end;
end;

procedure showInv;
var
  i: word;
  k: char;
begin
  Color(14,0);
  ClrScr;
  Center(1,'---= Inventory =---');
  Color(8,0);
  for i:=1 to 20 do
    if inv[i] <> Nil then
      Center(i+1, rtrim(inv[i]^));
  k:=ReadKey;
end;

procedure SaveGame;
var
  fna: string[8];
  sf: File of TPlayer;
  hdr: TPlayer;
  i: Word;
begin
  BlueScreen;
  Write('An eight letter word for your adventure book: ');
  ReadLn(fna);
  Write('Writing your Adventure book to the library of tales...');
  Assign(sf, fna+'.sav');
  Rewrite(sf);
  hdr.i:=Loc;
  mkTItem(GameFile, hdr.dta);
  Write(sf,hdr);
  for i:=1 to 20 do
  begin
    if inv[i] <> Nil then
      hdr.dta:=inv[i]^
    else
      mkTItem('NIL',hdr.dta);
    Write(sf,hdr);
  end;
  Close(sf);
  WriteLn('Done.');
  ReadLn(fna);
end;

procedure zstats;
var
  opt: Word;
begin
  Repeat
    Color(14,0);
    ClrScr;
    Center(3,'In-Game Player Menu');
    opt:=DoMenu('Inventory', 'Write Adventure Book', 'Back to game',1);
    if opt = 1 then
      ShowInv
    else if opt = 2 then
      SaveGame;
    if opt < 3 then
      opt:=3;
  Until opt = 3;
end;

procedure Initialize;
var
  i: Word;
begin
  Loc:=0;
  for i:=1 to 20 do
    inv[i]:=Nil;
  CheckSnow:=False;
  New(world);
end;

procedure HaltError(msg: string);
begin
  Color(4,0);
  ClrScr;
  Center(1,'An Error has occurred!');
  Color(7,0);
  GotoXY(1,5);
  WriteLn(msg);
  Halt(1);
end;

procedure NewGame;
var
  i: Word;
  k: char;
begin
  BlueScreen;
  Write('Writing New Adventure book...');
  Loc:=1;
  for i:=1 to 20 do
    if inv[i] <> Nil then
    begin
      Dispose(inv[i]);
      inv[i]:=Nil;
    end;
  WriteLn('done.');
  Center(20, 'Press any key to embark on your new adventure...');
  k:=ReadKey;
end;

procedure LoadGame;
var
  fna: string[8];
  sf: File of TPlayer;
  p: TPlayer;
  buf: string;
  yn: char;
  LoadData: Boolean;
  i: Word;
begin
  yn:=' ';
  LoadData:=True;
  BlueScreen;
  Write('An eight letter word for your Adventure book: ');
  ReadLn(fna);
  Write('Reading your Adventure book from the library of tales...');
  Assign(sf,fna+'.sav');
  {$I-}
  Reset(sf);
  {$I+}
  if IOResult <> 0 then
    HaltError('Adventure book does not exist: '+fna);
  Read(sf,p);
  buf:=rtrim(p.dta);
  if GameFile <> buf then
  begin
    WriteLn(#10#13'Warning: This is a current game in progress');
    WriteLn('using the '+buf+' module.'#10#13);
    Center(20, 'Start a new quest using this character?');
    yn:=ReadKey;
    if UpCase(yn) = 'Y' then
      Loc:=1
    else
      LoadData:=False;
  end;
  if LoadData then
  begin
    if yn = ' ' then
      Loc:=p.i;
    for i:=1 to 20 do
    begin
      Read(sf,p);
      if rtrim(p.dta) = 'NIL' then
      begin
        if inv[i] <> Nil then
        begin
          Dispose(inv[i]);
          inv[i]:=Nil;
        end;
      end
      else
      begin
        if inv[i] = Nil then
          New(inv[i]);
        inv[i]^:=p.dta;
      end;
    end;
    WriteLn('done.');
    Center(20, 'Press any key to return to your adventure...');
    yn:=ReadKey;
  end;
  Close(sf);
end;

function SystemMenu: Word;
var
  opt: Word;
begin
  Seek(f,0);
  Read(f,world^);
  Color(14,0);
  ClrScr;
  Center(8, rtrim(world^.d1));
  opt:=DoMenu('Begin New Adventure book', 'Read an existing Adventure book', 'Quit', 1);
  if opt = 1 then
    NewGame
  else if opt = 2 then
    LoadGame;
  SystemMenu:=opt;
end;

procedure MoveLocation(l: Word);
begin
  Seek(f,l);
  Read(f,world^);
  Loc:=l;
end;

procedure PlaceXY(x,y: Word; s: string);
begin
  GotoXY(x,y);
  Write(s);
end;

procedure UseItem;
begin
  if got(world^.use) then
    if world^.use1 <> Loc then
      Loc:=world^.use1
    else if world^.use2 <> Loc then
      if drop(world^.use) then
        Loc:=world^.use2;
end;

procedure GameLoop;
var
  opt: Word;
  Done, SysMenu: Boolean;
  empty: TItem;
begin
  FillChar(empty, 20, ' ');
  Done:=False;
  SysMenu:=True;
  repeat
    if SysMenu then
    begin
      opt:=SystemMenu;
      if opt = 3 then
        Done:=True
      else
        if Loc > 0 then
          SysMenu:=False;
    end
    else
    begin
      MoveLocation(Loc);
      if world^.use3 <> Loc then
        if got(world^.use) then
          MoveLocation(world^.use3);
      Color(14,0);
      ClrScr;
      Center(3, rtrim(world^.m));
      Color(7,0);
      GotoXY(1,5);
      Write(world^.d1);
      Write(world^.d2);
      Write(world^.d3);
      if world^.get <> empty then
        if take(world^.get) then
          Center(20, 'You found a '+rtrim(world^.get)+'!');
      if world^.n <> Loc then
        PlaceXY(40,17,'N');
      if world^.s <> Loc then
        PlaceXY(40,19,'S');
      if world^.e <> Loc then
        PlaceXY(41,18,'E');
      if world^.w <> Loc then
        PlaceXY(39,18,'W');
      Color(14,0);
      PlaceXY(5,16,'---= Hotkeys =---');
      Color(15,0);
      PlaceXY(6,17,'Z - Status Screen');
      PlaceXY(6,18,'U - Use item');
      PlaceXY(6,19,'ESC - Menu');
      opt:=DoMenu(rtrim(world^.mi1),rtrim(world^.mi2),rtrim(world^.mi3),1);
      Case opt of
        1: Loc:=world^.m1;
        2: Loc:=world^.m2;
        3: Loc:=world^.m3;
        NORTH: Loc:=world^.n;
        SOUTH: Loc:=world^.s;
        EAST: Loc:=world^.e;
        WEST: Loc:=world^.w;
        M_USE: UseItem;
        MSTAT: zstats;
        99: SysMenu:=True;
      end;
    end;
  until Done;
end;

begin
  GameFile:='';
  if ParamCount = 0 then
    GameFile:='demo.dat'
  else
    GameFile:=ParamStr(1);
  Initialize;
  Assign(f, GameFile);
  {$I-}
  Reset(f);
  {$I+};
  if IOResult <> 0 then
    HaltError('The file '+GameFile+' could not be opened.');
  GameLoop;
  Close(f);
  Color(7,0);
  ClrScr;
end.
