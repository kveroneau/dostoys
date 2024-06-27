unit leakchk;

interface

implementation

var
  Avail: longint;
  NextExit: Pointer;

{$F+}
procedure MemStat;{$F-}
begin
  ExitProc:=NextExit;
  WriteLn('Memory at start of program: ',Avail);
  WriteLn('Memory at end of program: ',MemAvail);
  WriteLn('There are ',Avail-MemAvail,' unfreed bytes of memory.');
end;

begin
  Avail:=MemAvail;
  NextExit:=ExitProc;
  ExitProc:=@MemStat;
end.