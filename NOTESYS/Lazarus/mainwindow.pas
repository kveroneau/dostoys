unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EGAConsole, StdCtrls;

type

  PScreen = ^TScreen;
  TScreen = record
    x,y,attr: byte;
    x1,y1,x2,y2: byte;
    buffer: TEGAScreen;
  end;

  { TNoteSysForm }

  TNoteSysForm = class(TForm)
    EGAConsole: TEGAConsole;
    NextBtn: TButton;
    PriorBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PriorBtnClick(Sender: TObject);
  private
    FScr: TScreen;
    FBuf: File of TScreen;
    FScrID: integer;
    procedure SetScrID(AValue: integer);
  public
    property ScrID: integer read FScrID write SetScrID;
  end;

var
  NoteSysForm: TNoteSysForm;

implementation

{$R *.lfm}

{ TNoteSysForm }

procedure TNoteSysForm.FormCreate(Sender: TObject);
begin
  FScrID:=-1;
  system.Assign(FBuf, 'FEDIT.SCR');
  {$I-}
  Reset(FBuf);
  {$I+}
  if IOResult <> 0 then
    Exit;
  ScrID:=0;
end;

procedure TNoteSysForm.FormDestroy(Sender: TObject);
begin
  system.Close(FBuf);
end;

procedure TNoteSysForm.NextBtnClick(Sender: TObject);
begin
  ScrID:=ScrID+1;
end;

procedure TNoteSysForm.PriorBtnClick(Sender: TObject);
begin
  ScrID:=ScrID-1;
end;

procedure TNoteSysForm.SetScrID(AValue: integer);
begin
  if FScrID=AValue then Exit;
  if AValue < 0 then
    AValue:=FileSize(FBuf)-1
  else if AValue > FileSize(FBuf)-1 then
    AValue:=0;
  FScrID:=AValue;
  Seek(FBuf, FScrID);
  Read(FBuf, FScr);
  Move(FScr.buffer, EGAConsole.EGABuffer^, SizeOf(FScr.buffer));
  EGAConsole.Invalidate;
end;

end.

