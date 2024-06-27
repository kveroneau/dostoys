unit registry;

interface

type
  PRegistry = ^TRegistry;
  TRegistry = record
    key: string[8];
    data: Array[0..63] of Byte;
  end;
  PRegString = ^RegString;
  RegString = string[63];

var
  Reg: Array[0..15] of PRegistry;

function AddRegistry(entry: PRegistry): integer;
function AddString(key: string; value: RegString): integer;
function FindKey(key: string): integer;
procedure CloseRegistry;

implementation

const
  REGISTRY_FILE = 'REGISTRY.DAT';

function AddRegistry(entry: PRegistry): integer;
var
  i: integer;
begin
  for i:=0 to 15 do
    if Reg[i] = Nil then
    begin
      Reg[i]:=entry;
      AddRegistry:=i;
      Exit;
    end;
  AddRegistry:=-1;
end;

function AddString(key: string; value: RegString): integer;
var
  r: PRegistry;
begin
  New(r);
  r^.key:=key;
  Move(value, r^.data, SizeOf(value));
  AddString:=AddRegistry(r);
end;

function FindKey(key: string): integer;
var
  i: integer;
begin
  for i:=0 to 15 do
    if Reg[i] <> Nil then
      if Reg[i]^.key = key then
      begin
        FindKey:=i;
        Exit;
      end;
  FindKey:=-1;
end;

procedure OpenRegistry;
var
  f: File of TRegistry;
  i: integer;
begin
  Assign(f, REGISTRY_FILE);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
    Exit;
  for i:=0 to FileSize(f)-1 do
  begin
    New(Reg[i]);
    Read(f, Reg[i]^);
  end;
  Close(f);
end;

procedure SaveRegistry;
var
  f: File of TRegistry;
  i: integer;
begin
  Assign(f, REGISTRY_FILE);
  Rewrite(f);
  for i:=0 to 15 do
    if Reg[i] <> Nil then
      Write(f, Reg[i]^);
  Close(f);
end;

procedure CloseRegistry;
var
  i: integer;
begin
  SaveRegistry;
  for i:=0 to 15 do
    if Reg[i] <> Nil then
      Dispose(Reg[i]);
end;

procedure InitRegistry;
var
  i: integer;
begin
  for i:=0 to 15 do
    Reg[i]:=Nil;
  OpenRegistry;
end;

begin
  InitRegistry;
end.