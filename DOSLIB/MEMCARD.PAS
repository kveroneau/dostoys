unit memcard;

{ Started on May 28th, 2024 }

interface

type
  PBlockInfo = ^TBlockInfo;
  TBlockInfo = record
    title: string[20];
    ico: string[5];
    app, typ: byte;
  end;

  PMemcard = ^TMemcard;
  TMemcard = record
    fname: string[20];
    opened: boolean;
    blksize: word;
    info: array[1..15] of TBlockInfo;
    block: Pointer;
  end;

procedure AssignMemcard(AMemcard: PMemcard; AFname: string; ABlksize: word);
procedure CloseMemcard(AMemcard: PMemcard);
procedure ReadBlock(AMemcard: PMemcard; blkid: byte; buf: Pointer);
procedure WriteBlock(AMemcard: PMemcard; blkid: byte; buf: Pointer);
procedure WriteMeta(AMemcard: PMemcard);
procedure SetMeta(AMemcard: PMemcard; blkid: byte; ATitle, AIco: string; appno, typno: byte);

implementation

var
  f: File;

procedure AssignMemcard(AMemcard: PMemcard; AFname: string; ABlksize: word);
begin
  with AMemcard^ do
  begin
    fname:=AFname;
    blksize:=ABlksize;
    GetMem(block, blksize);
    opened:=True;
  end;
end;

procedure CloseMemcard(AMemcard: PMemcard);
begin
  with AMemcard^ do
  begin
    opened:=False;
    FreeMem(block, blksize);
  end;
end;

procedure FormatMemcard(AMemcard: PMemcard);
var
  i: integer;
begin
  Rewrite(f, AMemcard^.blksize);
  for i:=1 to 15 do
    with AMemcard^.info[i] do
    begin
      title:='UNTITLED';
      ico:='';
      app:=0;
      typ:=0;
    end;
  Move(AMemcard^.info[1], AMemcard^.block^, SizeOf(AMemcard^.info));
  BlockWrite(f, AMemcard^.block^, 1);
  FillChar(AMemcard^.block^, AMemcard^.blksize, #0);
  BlockWrite(f, AMemcard^.block^, 15);
  Seek(f,0);
end;

procedure OpenMemcard(AMemcard: PMemcard);
begin
  Assign(f, AMemcard^.fname);
  {$I-}
  Reset(f, AMemcard^.blksize);
  {$I+}
  if IOResult <> 0 then
    FormatMemcard(AMemcard);
end;

procedure ReadBlock(AMemcard: PMemcard; blkid: byte; buf: Pointer);
begin
  OpenMemcard(AMemcard);
  BlockRead(f, AMemcard^.block^, 1);
  Move(AMemcard^.block^, AMemcard^.info[1], SizeOf(AMemcard^.info));
  Seek(f, blkid);
  BlockRead(f, buf^, 1);
  Close(f);
end;

procedure WriteBlock(AMemcard: PMemcard; blkid: byte; buf: Pointer);
begin
  OpenMemcard(AMemcard);
  Move(AMemcard^.info[1], AMemcard^.block^, SizeOf(AMemcard^.info));
  BlockWrite(f, AMemcard^.block^, 1);
  Seek(f, blkid);
  BlockWrite(f, buf^, 1);
  Close(f);
end;

procedure WriteMeta(AMemcard: PMemcard);
begin
  OpenMemcard(AMemcard);
  Move(AMemcard^.info[1], AMemcard^.block^, SizeOf(AMemcard^.info));
  BlockWrite(f, AMemcard^.block^, 1);
  Close(f);
end;

procedure SetMeta(AMemcard: PMemcard; blkid: byte; ATitle, AIco: string; appno, typno: byte);
begin
  with AMemcard^.info[blkid] do
  begin
    title:=ATitle;
    ico:=AIco;
    app:=appno;
    typ:=typno;
  end;
end;

end.