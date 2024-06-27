unit vbd;

interface

const
  BLOCK_SIZE = 1024;

type
  PBlock = ^VBlock;
  VBlock = array[0..1023] of byte;
  PChars = ^VChars;
  VChars = array[0..1023] of char;
  PBoot = ^VBoot;
  VBoot = array[0..63] of char;

  PVBD = ^VHandle;
  VHandle = record
    fd: file;
    active: boolean;
    blkid: word;
    blk: PBlock;
    data: PChars;
    bootable, bootwait: boolean;
    bootcode: PBoot;
  end;

procedure AssignVBD(f: PVBD; fname: string);
procedure CloseVBD(f: PVBD);
function NewVBD(f: PVBD): word;
procedure WriteVBD(f: PVBD; block: word);
procedure ReadVBD(f: PVBD; block: word);
procedure SetBootVBD(f: PVBD; bootcode: string);
procedure ImportVBD(f: PVBD; block: word; fname: string);

implementation

type
  VSignature = array[0..3] of char;

  PHeader = ^VHeader;
  VHeader = record
    sig: VSignature;
    blocksize: word;
    bootable: boolean;
    bootcode: VBoot;
    wait: boolean;
  end;

const
  VBD_SIG: VSignature = 'VBD*';

procedure InitVBD(var fd: File);
var
  buf: PBlock;
  hdr: PHeader;
begin
  Rewrite(fd, BLOCK_SIZE);
  New(buf);
  hdr:=@buf^[0];
  with hdr^ do
  begin
    sig:=VBD_SIG;
    blocksize:=BLOCK_SIZE;
    bootable:=False;
    wait:=True;
  end;
  BlockWrite(fd, buf^,1);
  Seek(fd,0);
  Dispose(buf);
end;

procedure AssignVBD(f: PVBD; fname: string);
var
  blk: PBlock;
  hdr: PHeader;
begin
  if f = Nil then
    Exit;
  if f^.active then
    Exit;
  Assign(f^.fd, fname);
  {$I-}
  Reset(f^.fd, BLOCK_SIZE);
  {$I+}
  if IOResult <> 0 then
    InitVBD(f^.fd);
  New(blk);
  BlockRead(f^.fd, blk^, 1);
  hdr:=@blk^[0];
  if hdr^.sig <> VBD_SIG then
  begin
    Close(f^.fd);
    Exit;
  end;
  f^.bootable:=hdr^.bootable;
  if f^.bootable then
  begin
    New(f^.bootcode);
    Move(hdr^.bootcode, f^.bootcode^, SizeOf(hdr^.bootcode));
  end;
  Dispose(blk);
  f^.blk:=Nil;
  f^.data:=Nil;
  f^.bootwait:=hdr^.wait;
  f^.active:=True;
end;

procedure CloseVBD(f: PVBD);
begin
  if f = Nil then
    Exit;
  if not f^.active then
    Exit;
  Close(f^.fd);
  if f^.blk <> Nil then
    Dispose(f^.blk);
  f^.blk:=Nil;
  f^.data:=Nil;
  if f^.bootable then
    Dispose(f^.bootcode);
  f^.active:=False;
end;

function NewVBD(f: PVBD): word;
var
  fs: word;
begin
  if not f^.active then
    Exit;
  fs:=FileSize(f^.fd);
  Seek(f^.fd, fs);
  f^.blkid:=fs;
  if f^.blk <> Nil then
    Dispose(f^.blk);
  New(f^.blk);
  f^.data:=@f^.blk^[0];
  NewVBD:=fs;
end;

procedure WriteVBD(f: PVBD; block: word);
begin
  if not f^.active then
    Exit;
  if f^.blk = Nil then
    Exit;
  Seek(f^.fd, block);
  BlockWrite(f^.fd, f^.blk^, 1);
  {$IFDEF DEBUG}
  WriteLn('Block ',block,' written.');
  {$ENDIF}
  f^.blkid:=block;
end;

procedure ReadVBD(f: PVBD; block: word);
begin
  if not f^.active then
    Exit;
  if f^.blk = Nil then
    New(f^.blk);
  f^.data:=@f^.blk^[0];
  Seek(f^.fd, block);
  BlockRead(f^.fd, f^.blk^, 1);
  f^.blkid:=block;
end;

procedure SetBootVBD(f: PVBD; bootcode: string);
var
  buf: PBlock;
  hdr: PHeader;
begin
  if f = Nil then
    Exit;
  if not f^.active then
    Exit;
  if f^.bootable then
    Dispose(f^.bootcode);
  Seek(f^.fd,0);
  New(buf);
  BlockRead(f^.fd, buf^, 1);
  hdr:=@buf^[0];
  hdr^.bootable:=True;
  Move(bootcode[1], hdr^.bootcode, Length(bootcode));
  Seek(f^.fd,0);
  BlockWrite(f^.fd, buf^, 1);
  f^.bootable:=True;
  New(f^.bootcode);
  Move(bootcode[1], f^.bootcode^, Length(bootcode));
  Dispose(buf);
end;

procedure ImportVBD(f: PVBD; block: word; fname: string);
var
  src: File;
  buf: PBlock;
  bs: word;
begin
  if f = Nil then
    Exit;
  if not f^.active then
    Exit;
  {$IFDEF DEBUG}
  WriteLn('Importing ',fname,' into block ',block,'.');
  {$ENDIF}
  Assign(src, fname);
  {$I-}
  Reset(src, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('File not found.');
    Exit;
  end;
  New(buf);
  if FileSize(src) > BLOCK_SIZE then
    bs:=BLOCK_SIZE
  else
    bs:=FileSize(src);
  BlockRead(src, buf^, bs);
  Close(src);
  Seek(f^.fd, block);
  BlockWrite(f^.fd, buf^, 1);
  Dispose(buf);
end;

end.
