unit advenk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, advenlib;

type

  { TAdvenForm }

  TAdvenForm = class(TForm)
    Action1: TButton;
    Action2: TButton;
    Action3: TButton;
    InventoryList: TListBox;
    LNotification: TLabel;
    OpenDialog: TOpenDialog;
    TakeButton: TButton;
    UseButton: TButton;
    WestButton: TButton;
    EastButton: TButton;
    SouthButton: TButton;
    NorthButton: TButton;
    LDesc3: TLabel;
    LDesc2: TLabel;
    LDesc1: TLabel;
    LLocation: TLabel;
    MainMenu: TMainMenu;
    GameMenu: TMenuItem;
    OpenGame: TMenuItem;
    SaveGame: TMenuItem;
    LoadGame: TMenuItem;
    MenuItem5: TMenuItem;
    GameExit: TMenuItem;
    StatusBar: TStatusBar;
    procedure Action1Click(Sender: TObject);
    procedure Action2Click(Sender: TObject);
    procedure Action3Click(Sender: TObject);
    procedure EastButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GameExitClick(Sender: TObject);
    procedure NorthButtonClick(Sender: TObject);
    procedure OpenGameClick(Sender: TObject);
    procedure SouthButtonClick(Sender: TObject);
    procedure TakeButtonClick(Sender: TObject);
    procedure UseButtonClick(Sender: TObject);
    procedure WestButtonClick(Sender: TObject);
  private
    FGameFile: string;
    FGameOpened: Boolean;
    procedure DisableControls;
    procedure RenderLocation;
    procedure SyncInventory;
  public

  end;

var
  AdvenForm: TAdvenForm;

implementation

{$R *.lfm}

{ TAdvenForm }

procedure TAdvenForm.GameExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TAdvenForm.NorthButtonClick(Sender: TObject);
begin
  MoveLocation(world^.n);
  RenderLocation;
end;

procedure TAdvenForm.OpenGameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
    Exit;
  FGameFile:=OpenDialog.FileName;
  system.Assign(f, FGameFile);
  Reset(f);
  StatusBar.SimpleText:='Game Opened: '+OpenDialog.FileName;
  FGameOpened:=True;
  Seek(f, 0);
  Read(f, world^);
  Caption:=rtrim(world^.d1);
  MoveLocation(1);
  RenderLocation;
end;

procedure TAdvenForm.SouthButtonClick(Sender: TObject);
begin
  MoveLocation(world^.s);
  RenderLocation;
end;

procedure TAdvenForm.TakeButtonClick(Sender: TObject);
begin
  If TakeItem(world^.get) then
  begin
    SyncInventory;
    StatusBar.SimpleText:='You took the '+rtrim(world^.get);
    LNotification.Caption:='';
  end;
end;

procedure TAdvenForm.UseButtonClick(Sender: TObject);
begin
  if HaveItem(world^.use) then
    if world^.use1 <> Loc then
      MoveLocation(world^.use1)
    else if world^.use2 <> Loc then
      if DropItem(world^.use) then
        MoveLocation(world^.use2);
  RenderLocation;
end;

procedure TAdvenForm.WestButtonClick(Sender: TObject);
begin
  MoveLocation(world^.w);
  RenderLocation;
end;

procedure TAdvenForm.FormCreate(Sender: TObject);
begin
  DisableControls;
  FGameFile:='';
  FGameOpened:=False;
end;

procedure TAdvenForm.EastButtonClick(Sender: TObject);
begin
  MoveLocation(world^.e);
  RenderLocation;
end;

procedure TAdvenForm.Action1Click(Sender: TObject);
begin
  MoveLocation(world^.m1);
  RenderLocation;
end;

procedure TAdvenForm.Action2Click(Sender: TObject);
begin
  MoveLocation(world^.m2);
  RenderLocation;
end;

procedure TAdvenForm.Action3Click(Sender: TObject);
begin
  MoveLocation(world^.m3);
  RenderLocation;
end;

procedure TAdvenForm.FormDestroy(Sender: TObject);
begin
  if FGameOpened then
    system.Close(f);
end;

procedure TAdvenForm.DisableControls;
begin
  Action1.Enabled:=False;
  Action2.Enabled:=False;
  Action3.Enabled:=False;
  NorthButton.Enabled:=False;
  SouthButton.Enabled:=False;
  EastButton.Enabled:=False;
  WestButton.Enabled:=False;
  UseButton.Enabled:=False;
  TakeButton.Enabled:=False;
  LNotification.Caption:='';
end;

procedure TAdvenForm.RenderLocation;
begin
  DisableControls;
  LLocation.Caption:=rtrim(world^.m);
  LDesc1.Caption:=world^.d1;
  LDesc2.Caption:=world^.d2;
  LDesc3.Caption:=world^.d3;
  if world^.get <> EmptyStr then
    if not HaveItem(world^.get) then
      begin
        TakeButton.Enabled:=True;
        LNotification.Caption:='You see a '+rtrim(world^.get)+' here.';
      end;
  if world^.use <> EmptyStr then
    UseButton.Enabled:=True;
  if world^.n <> Loc then
    NorthButton.Enabled:=True;
  if world^.s <> Loc then
    SouthButton.Enabled:=True;
  if world^.e <> Loc then
    EastButton.Enabled:=True;
  if world^.w <> Loc then
    WestButton.Enabled:=True;
  Action1.Caption:=rtrim(world^.mi1);
  Action2.Caption:=rtrim(world^.mi2);
  Action3.Caption:=rtrim(world^.mi3);
  if world^.m1 <> Loc then
    Action1.Enabled:=True;
  if world^.m2 <> Loc then
    Action2.Enabled:=True;
  if world^.m3 <> Loc then
    Action3.Enabled:=True;
  {$IFDEF DEBUG}
  StatusBar.SimpleText:='Location #'+IntToStr(Loc);
  {$ENDIF}
end;

procedure TAdvenForm.SyncInventory;
var
  i: Word;
begin
  InventoryList.Items.Clear;
  for i:=1 to 20 do
    if inv[i] <> Nil then
      InventoryList.Items.Add(inv[i]^);
end;

end.

