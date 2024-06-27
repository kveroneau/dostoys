program cgasys;

{$IFDEF DEBUG}
{$DEFINE PREPSYS}
{$ENDIF}

uses graph, crt, globals, utils, cgalib, cgacrt, vbd, memsys, scrsys,
     shellsys, helpsys, widgets, tileedit, mapedit, engine;

type
  Digits = set of 0..9;
  Letters = set of 'A'..'Z';

const
  EvenDigits: Digits = [0,2,4,6,8];
  HexDigits: set of '0'..'z' = ['0'..'9','A'..'F','a'..'f'];
  MAIN_VBD = 'MAIN.VBD';

var
  DrawLine: boolean;
  ExitSave: Pointer;

procedure InitMode(GraphDriver, GraphMode: integer);
var
  ErrorCode: integer;
begin
  InitGraph(GraphDriver, GraphMode, 'C:\');
  ErrorCode:=GraphResult;
  if ErrorCode <> grOk then
  begin
    WriteLn('Graphics error: ', GraphErrorMsg(ErrorCode));
    Halt(1);
  end;
  SetFillStyle(0,0);
  SetColor(1);
  {SetTextStyle(SmallFont, 0, 4);}
end;

procedure HandleSpecial(sp: byte);
begin
  if sp = 71 then
    ClearTitle
  else if sp = 79 then
    InputMode:=imNone
  else if sp = 59 then
    HandleHelp;
end;

procedure HandleScript(key: byte);
begin
  if SysScripts[key] = '' then
    Exit;
  InputLine:=SysScripts[key]+'.';
  DoRunScript
end;

procedure HandleSystem(c: char);
begin
  if c = #0 then
    HandleSpecial(ord(ReadKey))
  else if (c = 'Q') or (c = #3) then
    SysRunning:=False
  {$IFDEF DEBUG}
  else if c = 'C' then
    InputMode:=imChar
  else if c = 'L' then
    InputMode:=imLine
  else if c = ' ' then
    InputCaps:=not InputCaps
  else if c = 'l' then
  begin
    InputLine:='Line Set!';
    DrawLine:=True;
  end
  {$ENDIF}
  else if c = 'R' then
  begin
    InputLine:='!S.';
    DrawLine:=True;
    InputMode:=imLine;
    SysMode:=smRun;
  end
  else if (c > #47) and (c < #58) then
    HandleScript(ord(c)-48);
end;

procedure HandleChar(c: char);
begin
  if SysMode = smTileEdit then
  begin
    TECharIn(c);
    Exit;
  end
  else if SysMode = smMapEdit then
  begin
    MECharIn(c);
    Exit;
  end
  else if SysMode = smEngine then
  begin
    EngineCharIn(c);
    Exit;
  end;
  WriteLn('You pressed: ',c,' / ',ord(c));
  if c = #0 then
    WriteLn('  Special Key: ',ord(ReadKey));
  InputMode:=imNone;
end;

procedure LineCallback;
begin
  InputMode:=imNone;
  case SysMode of
    smMain: ScriptLine(InputLine);
    smShell: DoShell;
    smRun: DoRunScript;
    smMedia: MountMedia(InputLine);
    smTileEdit: TELineIn;
    smMapEdit: MELineIn;
  end;
end;

procedure HandleLine(c: byte);
begin
  if c = 0 then
  begin
    HandleSpecial(ord(ReadKey));
    Exit;
  end
  else if c = 3 then
  begin
    SysRunning:=False;
    Exit;
  end;
  if c = 13 then
    LineCallback
  else if c = 27 then
  begin
    InputLine:='';
    DrawLine:=True;
  end
  else if (c > 31) and (c < 127) then
  begin
    InputLine:=InputLine+chr(c);
    DrawLine:=True;
  end
  else if c = 8 then
  begin
    if Length(InputLine) > 0 then
      Dec(InputLine[0]);
    DrawLine:=True;
  end;
end;

procedure MainSystem;
var
  c: char;
  line: string;
  {$IFDEF IDLE}idle: word;{$ENDIF}
begin
  InputMode:=imNone;
  InputLine:='';
  DrawLine:=False;
  InputCaps:=True;
  SysMode:=smMain;
  SysRunning:=True;
  repeat
    repeat
      if DrawLine then
      begin
        Bar(30,27,200,37);
        SetColor(2);
        OutTextXY(30,27,InputLine);
        DrawLine:=False;
      end;
      RunTasks;
      {$IFDEF IDLE}
      Inc(idle);
      if idle = 0 then
      begin
        idle:=0;
        DrawClock(GetMaxX-70, 0);
        DrawStats(0,0);
      end;
      {$ENDIF}
    until KeyPressed;
    DrawClock(GetMaxX-70, 0);
    c:=ReadKey;
    if InputCaps and (c > #96) and (c < #123) then
      c:=chr(ord(c)-32);
    case InputMode of
      imNone: HandleSystem(c);
      imChar: HandleChar(c);
      imLine: HandleLine(ord(c));
    end;
    DrawStats(0,0);
    if SysMode = smEngineScript then
    begin
      HandleScript(EngineScript);
      SysMode:=smEngine;
    end;
  until not SysRunning;
end;

{$F+}
procedure SysExit;
var
  i: integer;
begin
  Assign(Output, '');
  Rewrite(Output);
  CloseGraph;
  FreeAll;
  if RootBD.active then
    CloseVBD(@RootBD);
  if MediaBD.active then
    CloseVBD(@MediaBD);
  for i:=0 to 31 do
    if SysTasks[i] <> Nil then
      Dispose(SysTasks[i]);
  WriteLn('Safely exited.');
  ExitProc:=ExitSave;
end;
{$F-}

procedure MountVBD(f: PVBD; fname: string);
begin
  AssignVBD(f, fname);
  if not f^.active then
  begin
    WriteLn('failed.');
    WriteLn(' X Failed to active Virtual Block Device!');
    Halt(1);
  end;
  WriteLn(FileSize(f^.fd), ' blocks.');
end;

procedure BootFail(msg: string);
begin
  WriteLn(' X ',msg);
  if RootBD.active then
    CloseVBD(@RootBD);
  if MediaBD.active then
    CloseVBD(@MediaBD);
  Halt(2);
end;

procedure CheckBoot;
begin
  WriteLn(' * Checking Boot Media...');
  {if FileSize(BootBD^.fd) = 1 then
    BootFail('Empty Boot Medium found!');}
  if not BootBD^.bootable then
    BootFail('Boot Media not bootable!');
  New(SysMemory[0]);
  Move(BootBD^.bootcode^, SysMemory[0]^, SizeOf(BootBD^.bootcode^));
  AddTask('BOOT', SysMemory[0]);
end;

{$IFDEF PREPSYS}
procedure PrepSys;
begin
  TextBackground(1);
  WriteLn(' ! System Prep started.');
  Seek(BootBD^.fd,1);
  Truncate(BootBD^.fd);
  SetBootVBD(BootBD, 'fV#M#L#1##>xPrepSys~.');
  ImportVBD(BootBD, 1, 'prepsys.scr');
  ImportVBD(BootBD, 2, 'preplib.scr');
  TextBackground(0);
end;
{$ENDIF}

begin
  StartMem:=MemAvail;
  WriteLn(' * CGA System is initializing...');
  if ParamCount = 0 then
  begin
    Write(' > Mounting default Virtual Block Device...');
    MountVBD(BootBD, MAIN_VBD);
  end
  else if ParamCount > 0 then
  begin
    Write(' > Mounting selected Virtual Block Device...');
    MountVBD(BootBD, ParamStr(1));
  end;
  if ParamCount = 2 then
  begin
    Write(' > Mounting Boot Media...');
    BootBD:=@MediaBD;
    MountVBD(BootBD, ParamStr(2));
    {$IFDEF PREPSYS}
    if ParamStr(2) = 'prepsys.vbd' then
      PrepSys;
    {$ENDIF}
  end;
  CheckBoot;
  Write(' ? Press Enter to start system...');
  if BootBD^.bootwait then
    ReadLn;
  InitMode(CGA,0);
  AssignCGA(Output);
  Rewrite(Output);
  ExitSave:=ExitProc;
  ExitProc:=@SysExit;
  MainSystem;
end.
