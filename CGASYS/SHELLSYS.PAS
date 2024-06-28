unit shellsys;

interface

uses cgacrt, globals, scrsys, vbd, graph, cgalib, memsys;

procedure StartShell;
procedure DoShell;

implementation

var
  ShellVBD: PVBD;

procedure StartShell;
begin
  SysMode:=smShell;
  InputMode:=imLine;
  ShellVBD:=@RootBD;
end;

procedure ShowHelp;
begin
  AppTitle:='Shell Help';
  SetColor(3);
  ClearTitle;
  WriteLn('Available Commands:');
  WriteLn(' EXIT, HALT, LS, CLS, RUN, MOUNT, MEDIA,');
  WriteLn(' BOOTCODE, VER, SEG');
end;

procedure DumpSegment(seg: PSegment);
var
  i: integer;
begin
  for i:=0 to 1023 do
  begin
    if (seg^[i] > 31) and (seg^[i] < 128) then
      Write(chr(seg^[i]));
  end;
  Write(#13#10);
end;

procedure SegList;
var
  i: integer;
begin
  WriteLn('Next Free: ',NextFree);
  for i:=0 to 63 do
    if SysMemory[i] <> Nil then
      WriteLn('Segment ',i,' used.');
end;

procedure RunScript(scr: string);
begin
  InputLine:=scr+'.';
  DoRunScript;
end;

procedure DoShell;
var
  sc: PSysCmd;
begin
  if ShellVBD = Nil then
    ShellVBD:=@RootBD;
  if InputLine = 'EXIT' then
  begin
    SysMode:=smMain;
    SetColor(1);
    ClearTitle;
    Exit;
  end;
  if InputLine = 'HALT' then
  begin
    SysRunning:=False;
    Exit;
  end;
  sc:=FindCmd(InputLine);
  if sc <> Nil then
    RunScript(sc^.script)
  else if InputLine = 'CLS' then
    ClearTitle
  else if InputLine = 'LS' then
    WriteLn('VBD Blocks: ',FileSize(ShellVBD^.fd))
  else if InputLine = '?' then
    ShowHelp
  else if InputLine = 'RUN' then
  begin
    InputLine:='';
    SysMode:=smRun;
  end
  else if InputLine = 'MOUNT' then
  begin
    InputLine:='';
    SysMode:=smMedia;
  end
  else if InputLine = 'BOOTCODE' then
  begin
    if ShellVBD^.bootable then
      WriteLn(ShellVBD^.bootcode^)
    else
      WriteLn(' X VBD Not bootable!');
  end
  else if InputLine = 'VER' then
    WriteLn('CGA System v0.1b')
  else if InputLine = 'MEDIA' then
  begin
    if not MediaBD.active then
      WriteLn(' X VBD Not Mounted!')
    else
      ShellVBD:=@MediaBD
  end
  else if InputLine = 'SEG' then
    SegList
  {$IFDEF DEBUG}
  else if InputLine = 'SEG0' then
    DumpSegment(SysMemory[0])
  else if InputLine = 'SEG1' then
    DumpSegment(SysMemory[1])
  else if InputLine = 'SEG2' then
    DumpSegment(SysMemory[2])
  else if InputLine = 'SEG3' then
    DumpSegment(SysMemory[3])
  else if InputLine = 'TEST' then
    RunScript('#R#L#1##>xTest~')
  else if InputLine = 'RUN3' then
    RunScript('XTest~3#')
  else if InputLine = 'LOAD1' then
    ReadVBD(ShellVBD, 1)
  else if InputLine = 'IMPORT' then
    RunScript('pFile to Import?~iIPVAR~#i1#')
  {$ENDIF}
  else
    WriteLn(' X Syntax Error: ',InputLine);
  InputMode:=imLine;
end;

begin
  if SysVerbose then
    WriteLn(' * Shell SubSystem initializing...');
  ShellVBD:=Nil;
end.
