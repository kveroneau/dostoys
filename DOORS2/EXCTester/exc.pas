unit exc;

{$mode objfpc}{$H+}{$PACKENUM 1}

interface

uses Classes, sysutils, cmdsys, fgl, StrUtils;

type
  PMemory = ^TMemory;
  TMemory = Array[0..63] of Byte;

  SegmentType = (segNone, segCode, segData, segString);
  PDoorSegment = ^TDoorSegment;
  TDoorSegment = record
    data: TMemory;
    stype: SegmentType;
  end;

  PSegString = ^TSegString;
  TSegString = String[60];

  PEXCHeader = ^TEXCHeader;
  TEXCHeader = record
    sig: Array[0..3] of Char;
    cseg, dseg: Byte;
    gui: Boolean;
  end;

  TaskState = (tskNew, tskRun, tskSleep, tskStop, tskSuspend, tskInput);

  TEXCFile = class;
  TDeviceProc = procedure(exc: TEXCFile) of object;
  TSysCallProc = procedure(exc: TEXCFile; syscall: byte) of object;

  ECompileError = class(Exception);

  { TEXCFile }

  TEXCFile = class(TObject)
  private
    FName: string[10];
    FState: TaskState;
    FCSeg, FDSeg, FCP: Byte;
    FSegs: Array[0..31] of PDoorSegment;
    FDevices: Array[0..255] of TDeviceProc;
    FOnSysCall: TSysCallProc;
    procedure IsReg(jmp, r2, r1: byte);
    procedure SysRoutine(syscall: byte);
    procedure SetSegString(s: TSegString; seg: byte);
    function FreeSegment: byte;
    procedure Write2Op(op, param: byte);
    procedure Write3Op(op, param2, param1: byte);
    procedure Write4Op(op, param3, param2, param1: byte);
    function GetStrOrSeg(param: string): byte;
    procedure SegJMP(seg: byte);
    procedure CallDevice(dev: byte);
    procedure CheckDevice(dev: byte);
    function GetOrSetLabel(param: byte): byte;
  public
    property State: TaskState read FState;
    property CP: byte read FCP;
    property OnSysCall: TSysCallProc read FOnSysCall write FOnSysCall;
    constructor Create;
    destructor Destroy; override;
    function LoadEXC(EXCFile: string): Boolean;
    procedure SaveEXC(EXCFile: string);
    function GetParam: Byte;
    function GetSegString: TSegString;
    function GetSegString(seg: byte): TSegString;
    procedure SetReg(v, r: byte);
    function GetReg(r: byte): byte;
    procedure RunTask;
    function Compile(lines: TStrings): Boolean;
    procedure AddDevice(dev: byte; cb: TDeviceProc);
    function GetSegType(seg: byte): SegmentType;
  end;

const
  EXC_SIG = 'EXC*';

implementation

type
  TMemoryMap = Array[0..255] of PMemory;
  TLabels = specialize TFPGMap<string, byte>;
  TAddrList = specialize TFPGList<byte>;
  TJMPList = specialize TFPGMap<string, TAddrList>;

var
  TaskList: Array[0..31] of TEXCFile;
  MainTask: TEXCFile;
  GlobalMem: TMemoryMap;

{ TEXCFile }

procedure TEXCFile.IsReg(jmp, r2, r1: byte);
begin
  if GetReg(r1) = r2 then
    FCP:=jmp;
end;

procedure TEXCFile.SysRoutine(syscall: byte);
begin
  case syscall of
    $a0: WriteLn(' * InitCoder');
    $a1: WriteLn(' * DoneCoder');
    $a2: WriteLn(' * ProcessLine');
    $fe: CheckDevice(GetParam);
    $ff: FState:=tskStop;
  else
    if Assigned(FOnSysCall) then
      FOnSysCall(Self, syscall);
  end;
  SetReg($ff, 3);
end;

constructor TEXCFile.Create;
begin
  FState:=tskNew;
  FillByte(FSegs, SizeOf(FSegs), 0);
end;

destructor TEXCFile.Destroy;
var
  i: integer;
begin
  for i:=0 to 31 do
    if Assigned(FSegs[i]) then
      Dispose(FSegs[i]);
end;

function TEXCFile.LoadEXC(EXCFile: string): Boolean;
var
  f: TMemoryStream;
  hdr: PEXCHeader;
  i, segcnt: integer;
begin
  Result:=False;
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(EXCFile);
    New(FSegs[0]);
    f.Read(FSegs[0]^, SizeOf(FSegs[0]^));
    hdr:=@FSegs[0]^.data;
    if hdr^.sig <> EXC_SIG then
      Exit;
    FCSeg:=hdr^.cseg;
    FDSeg:=hdr^.dseg;
    FCP:=0;
    segcnt:=(f.Size div SizeOf(FSegs[0]^));
    for i:=1 to segcnt do
    begin
      New(FSegs[i]);
      f.Read(FSegs[i]^, SizeOf(FSegs[i]^));
    end;
    FState:=tskRun;
    Result:=True;
  finally
    f.Free;
  end;
end;

procedure TEXCFile.SaveEXC(EXCFile: string);
var
  f: TMemoryStream;
  hdr: PEXCHeader;
  i: integer;
begin
  if not Assigned(FSegs[0]) then
    New(FSegs[0]);
  FSegs[0]^.stype:=segNone;
  hdr:=@FSegs[0]^.data;
  hdr^.sig:=EXC_SIG;
  hdr^.cseg:=FCSeg;
  hdr^.dseg:=FDSeg;
  hdr^.gui:=True;
  f:=TMemoryStream.Create;
  try
    for i:=0 to 31 do
      if Assigned(FSegs[i]) then
        f.Write(FSegs[i]^, SizeOf(FSegs[i]^));
    f.SaveToFile(EXCFile);
  finally
    f.Free;
  end;
end;

function TEXCFile.GetParam: Byte;
begin
  Result:=FSegs[FCSeg]^.data[FCP];
  Inc(FCP);
end;

function TEXCFile.GetSegString: TSegString;
var
  ss: Byte;
  s: TSegString;
begin
  ss:=GetParam;
  Move(FSegs[ss]^.data, s, SizeOf(TSegString));
  Result:=s;
end;

function TEXCFile.GetSegString(seg: byte): TSegString;
var
  s: TSegString;
begin
  Move(FSegs[seg]^.data, s, SizeOf(TSegString));
  Result:=s;
end;

procedure TEXCFile.SetSegString(s: TSegString; seg: byte);
var
  ss: PSegString;
begin
  if not Assigned(FSegs[seg]) then
    New(FSegs[seg]);
  FSegs[seg]^.stype:=segString;
  ss:=@FSegs[seg]^.data;
  ss^:=s;
end;

function TEXCFile.FreeSegment: byte;
var
  i: integer;
begin
  Result:=0;
  for i:=1 to 31 do
    if not Assigned(FSegs[i]) then
    begin
      New(FSegs[i]);
      Result:=i;
      Exit;
    end;
end;

procedure TEXCFile.Write2Op(op, param: byte);
begin
  FSegs[FCSeg]^.data[FCP]:=op;
  FSegs[FCSeg]^.data[FCP+1]:=param;
  Inc(FCP, 2);
end;

procedure TEXCFile.Write3Op(op, param2, param1: byte);
begin
  FSegs[FCSeg]^.data[FCP]:=op;
  FSegs[FCSeg]^.data[FCP+1]:=param1;
  FSegs[FCSeg]^.data[FCP+2]:=param2;
  Inc(FCP, 3);
end;

procedure TEXCFile.Write4Op(op, param3, param2, param1: byte);
begin
  Write2Op(op, param1);
  Write2Op(param2, param3);
end;

function TEXCFile.GetStrOrSeg(param: string): byte;
var
  s: integer;
begin
  if not TryStrToInt(param, s) then
  begin
    s:=FreeSegment;
    SetSegString(param, s);
  end;
  Result:=s;
end;

procedure TEXCFile.SegJMP(seg: byte);
begin
  FCSeg:=seg;
  FCP:=0;
end;

procedure TEXCFile.CallDevice(dev: byte);
begin
  if Assigned(FDevices[dev]) then
    FDevices[dev](Self);
end;

procedure TEXCFile.CheckDevice(dev: byte);
begin
  if Assigned(FDevices[dev]) then
    SetReg($ff, 0)
  else
    SetReg(0, 0);
end;

function TEXCFile.GetOrSetLabel(param: byte): byte;
begin

end;

procedure TEXCFile.SetReg(v, r: byte);
begin
  FSegs[FDSeg]^.data[r]:=v;
end;

function TEXCFile.GetReg(r: byte): byte;
begin
  Result:=FSegs[FDSeg]^.data[r];
end;

procedure TEXCFile.RunTask;
var
  op: Byte;
begin
  op:=GetParam;
  case op of
    $10: SetReg(GetParam, GetParam);
    $11: IsReg(GetParam, GetParam, GetParam);
    $20: WriteLn(GetSegString);
    $22: WriteLn(' * LineInput: ', GetSegString);
    $23: WriteLn(' * CommandInput: ', GetSegString);
    $60: WriteLn(' * SetGraphMode: ',GetParam);
    $61: WriteLn(' * DrawBorder: ', GetSegString);
    $62: WriteLn(' * DrawBtn: ', GetParam, ', ',GetParam, ', ',GetSegString);
    $80: FCP:=GetParam;
    $81: SegJMP(GetParam);
    $fe: CallDevice(GetParam);
    $ff: SysRoutine(GetParam);
  end;
end;

function TEXCFile.Compile(lines: TStrings): Boolean;
var
  i, s: integer;
  b: byte;
  line, cmd, tmp: string;
  lbl: TLabels;
  jmplist: TJMPList;
  addrlist: TAddrList;
begin
  Result:=False;
  for i:=0 to 31 do
    if Assigned(FSegs[i]) then
    begin
      Dispose(FSegs[i]);
      FSegs[i]:=Nil;
    end;
  lbl:=TLabels.Create;
  jmplist:=TJMPList.Create;
  try
    for i:=0 to lines.Count-1 do
    begin
      line:=lines.Strings[i];
      cmd:=getToken(line);
      if cmd[1] = ':' then
      begin
        lbl.AddOrSetData(RightStr(cmd, Length(cmd)-1), FCP);
        if jmplist.TryGetData(RightStr(cmd, Length(cmd)-1), addrlist) then
        begin
          for s:=0 to addrlist.Count-1 do
            FSegs[FCSeg]^.data[addrlist.Items[s]]:=FCP;
        end;
      end
      else if cmd = '.CSEG' then
      begin
        FCSeg:=StrToInt(getToken(line));
        if not Assigned(FSegs[FCSeg]) then
          New(FSegs[FCSeg]);
        FSegs[FCSeg]^.stype:=segCode;
        FCP:=0;
      end
      else if cmd = '.DSEG' then
      begin
        FDSeg:=StrToInt(getToken(line));
        if not Assigned(FSegs[FDSeg]) then
          New(FSegs[FDSeg]);
        FSegs[FDSeg]^.stype:=segData;
      end
      else if cmd = '.REG' then
      begin
        if FDSeg = 0 then
          Exit;
        SetReg(StrToInt(getToken(line)), StrToInt(getToken(line)));
      end
      else if cmd = '.STRING' then
      begin
        {i:=FreeSegment;}
        SetSegString(getToken(line), StrToInt(getToken(line)));
      end
      else if cmd = 'write' then
        Write2Op($20, GetStrOrSeg(getToken(line)))
      else if cmd = 'input' then
        Write2Op($22, GetStrOrSeg(getToken(line)))
      else if cmd = 'cmd' then
        Write2Op($23, GetStrOrSeg(getToken(line)))
      else if cmd = 'end' then
        Write2Op($ff, $ff)
      else if cmd = 'jmp' then
      begin
        tmp:=getToken(line);
        if lbl.TryGetData(tmp, b) then
          Write2Op($80, b)
        else
        begin
          if not jmplist.TryGetData(tmp, addrlist) then
            addrlist:=TAddrList.Create;
          addrlist.Add(FCP+1);
          jmplist.AddOrSetData(tmp, addrlist);
          Write2Op($80, $ff);
        end;
      end
      else if cmd = 'segjmp' then
        Write2Op($81, StrToInt(getToken(line)))
      else if cmd = 'setreg' then
        Write3Op($10, StrToInt(getToken(line)), StrToInt(getToken(line)))
      else if cmd = 'isreg' then
      begin
        Write3Op($11, StrToInt(getToken(line)), StrToInt(getToken(line)));
        tmp:=getToken(line);
        if lbl.TryGetData(tmp, b) then
          FSegs[FCSeg]^.data[FCP]:=b
        else
        begin
          if not jmplist.TryGetData(tmp, addrlist) then
            addrlist:=TAddrList.Create;
          addrlist.Add(FCP);
          jmplist.AddOrSetData(tmp, addrlist);
          FSegs[FCSeg]^.data[FCP]:=$ff;
        end;
        Inc(FCP);
      end
      else if cmd = 'graphmode' then
        Write2Op($60, StrToInt(getToken(line)))
      else if cmd = 'border' then
        Write2Op($61, GetStrOrSeg(getToken(line)))
      else if cmd = 'button' then
        Write4Op($62, GetStrOrSeg(getToken(line)), StrToInt(getToken(line)), StrToInt(getToken(line)))
      else if cmd = 'chkdev' then
        Write3Op($ff, StrToInt(getToken(line)), $fe)
      else if cmd = 'syscall' then
        Write2Op($ff, StrToInt(getToken(line)))
      else if cmd <> '' then
        raise ECompileError.Create('Line '+IntToStr(i+1)+' is invalid!');
    end;
    SaveEXC('TEST.EXC');
    FCP:=0;
    Result:=True;
  finally
    lbl.Free;
    for i:=0 to jmplist.Count-1 do
    begin
      addrlist:=jmplist.Data[i];
      addrlist.Free;
    end;
    jmplist.Free;
  end;
end;

procedure TEXCFile.AddDevice(dev: byte; cb: TDeviceProc);
begin
  FDevices[dev]:=cb;
end;

function TEXCFile.GetSegType(seg: byte): SegmentType;
begin
  if Assigned(FSegs[seg]) then
    Result:=FSegs[seg]^.stype
  else
    Result:=segNone;
end;

procedure FreeAll;
var
  i: Integer;
begin
  for i:=0 to 255 do
    if Assigned(GlobalMem[i]) then
      Dispose(GlobalMem[i]);
  for i:=0 to 31 do
    if Assigned(TaskList[i]) then
      TaskList[i].Free;
end;

initialization
  FillByte(GlobalMem, SizeOf(GlobalMem), 0);
  FillByte(TaskList, SizeOf(TaskList), 0);

finalization
  FreeAll;

end.

