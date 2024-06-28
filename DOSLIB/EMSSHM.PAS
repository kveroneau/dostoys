unit emsshm;

interface

uses ems;

function InitEMSShm: word;
function LoadEMSShm: word;
procedure FreeEMS;

implementation

type
  TSignature = array[0..3] of char;

const
  SHMFILE = 'EMSSHM.SYS';
  SHM_SIG: TSignature = ('S','H','M','*');

type
  TShmFile = record
    sig: TSignature;
    handle: word;
    addr: word;
  end;

var
  shm: TShmFile;

procedure FreeEms;
var
  f: File of TShmFile;
begin
  {$IFDEF DEBUG}
  WriteLn('EMS Handle: ',shm.handle);
  {$ENDIF}
  if shm.handle <> 0 then
  begin
    if EmsFree(shm.handle) <> 0 then
      WriteLn(' * Error trying to free EMS memory.')
    else
      shm.handle:=0;
    Assign(f, SHMFILE);
    Erase(f);
  end;
end;

procedure HaltError(msg: string);
begin
  FreeEms;
  WriteLn(' * ',msg);
  Halt(1);
end;

procedure MapEms(phy,log: Word);
begin
  if EmsMap(shm.handle,phy,log) <> 0 then
    HaltError('Unable to map EMS memory!');
end;

function InitEMSShm: word;
var
  total, avail: word;
  f: File of TShmFile;
begin
  shm.handle:=0;
  if not EmsInstalled then
    HaltError('EMS Not Installed.');
  if EmsPages(total, avail) <> 0 then
    HaltError('Unable to dermine EMS pages.');
  {$IFDEF DEBUG}
  WriteLn(total, ' pages of EMS found.');
  WriteLn(avail, ' pages of EMS available.');
  {$ENDIF}
  if EmsAlloc(4, shm.handle) <> 0 then
    HaltError('Unable to allocate 4 pages of EMS!');
  {$IFDEF DEBUG}
  WriteLn('Pages allocated.');
  {$ENDIF}
  if EmsAddr = 0 then
    HaltError('Unable to obtain EMS Page frame address.');
  MapEms(0,0);
  MapEms(1,1);
  shm.sig:=SHM_SIG;
  shm.addr:=EmsAddr;
  Assign(f, SHMFILE);
  Rewrite(f);
  Write(f, shm);
  Close(f);
  InitEMSShm:=shm.addr;
end;

function LoadEMSShm: word;
var
  f: File of TShmFile;
begin
  Assign(f, SHMFILE);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(' * This program cannot be run outside of KevLoader.');
    Halt(2);
  end;
  Read(f, shm);
  Close(f);
  if shm.sig <> SHM_SIG then
  begin
    WriteLn(' * EMS Shm ERROR!');
    Halt(2);
  end;
  MapEms(0,0);
  MapEms(1,1);
  LoadEMSShm:=shm.addr;
end;

end.
