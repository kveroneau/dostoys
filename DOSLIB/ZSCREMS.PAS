unit zscrems;

{ File created on Sept 29, 2021 }

interface

uses ems, zscrsave;

procedure ScrToEMS(page: byte);
procedure ScrFromEMS(page: byte);
procedure ScrSwap(cur, target: byte);

implementation

var
  emmHandle: word;
  NextExit: Pointer;
  curHandle: byte;

procedure ScrToEMS(page: byte);
var
  emm: PScreen;
begin
  if page > 4 then
    Exit;
  if EmsMap(emmHandle, 0, 0) <> 0 then
    Exit;
  emm:=Ptr(EmsAddr,page*4000);
  SaveScreen(emm);
end;

procedure ScrFromEMS(page: byte);
var
  emm: PScreen;
begin
  if page > 4 then
    Exit;
  if EmsMap(emmHandle, 0, 0) <> 0 then
    Exit;
  emm:=Ptr(EmsAddr,page*4000);
  DrawScreen(emm);
end;

procedure ScrSwap(cur, target: byte);
begin
  ScrToEMS(cur);
  ScrFromEMS(target);
end;

{$F+}
procedure doneproc;{$F-}
begin
  ExitProc:=NextExit;
  if emmHandle > 0 then
    if EmsFree(emmHandle) <> 0 then
      WriteLn(' * Error freeing EMS memory!')
    else
      WriteLn('EMS Freed.');
end;

procedure initems;
begin
  emmHandle:=0;
  if not EmsInstalled then
    Exit;
  if EmsAlloc(1, emmHandle) <> 0 then
    Exit;
  WriteLn('EMS Allocated handle: ',emmHandle);
end;

begin
  NextExit:=ExitProc;
  ExitProc:=@doneproc;
  curHandle:=0;
  initems;
end.