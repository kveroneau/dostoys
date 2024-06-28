unit adveneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  advenlib;

type

  { TEditorForm }

  TEditorForm = class(TForm)
    AreaTitle: TEdit;
    ActionButtons: TGroupBox;
    Action1: TEdit;
    Action2: TEdit;
    Action3: TEdit;
    SaveBtn: TButton;
    UseMove: TEdit;
    Label6: TLabel;
    UseDrop: TCheckBox;
    EastDir: TEdit;
    UseLoc: TEdit;
    ItemUse: TEdit;
    ItemGet: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    WestDir: TEdit;
    SouthDir: TEdit;
    NorthDir: TEdit;
    LocNumber: TLabel;
    GoAction1: TSpeedButton;
    GoAction2: TSpeedButton;
    GoAction3: TSpeedButton;
    Target3: TComboBox;
    Target2: TComboBox;
    Target1: TComboBox;
    Label1: TLabel;
    AreaDesc: TMemo;
    Label2: TLabel;
    RoomList: TListBox;
    procedure EastDirDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GoAction1Click(Sender: TObject);
    procedure GoAction3Click(Sender: TObject);
    procedure NorthDirDblClick(Sender: TObject);
    procedure RoomListClick(Sender: TObject);
    procedure GoAction2Click(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SouthDirDblClick(Sender: TObject);
    procedure UseLocDblClick(Sender: TObject);
    procedure UseMoveDblClick(Sender: TObject);
    procedure WestDirDblClick(Sender: TObject);
  private
    FGameFile: TGameFile;
    procedure PopulateForm;
  public

  end;

var
  EditorForm: TEditorForm;

implementation

{$R *.lfm}

{ TEditorForm }

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  FGameFile:=TGameFile.Create;
  FGameFile.LoadFromFile('DEMO.DAT');
  FGameFile.Initialize;
  FGameFile.DesignMode:=True;
  FGameFile.ListRooms(RoomList.Items);
  Target1.Items.Text:=RoomList.Items.Text;
  Target2.Items.Text:=RoomList.Items.Text;
  Target3.Items.Text:=RoomList.Items.Text;
  GoAction2.Glyph.LoadFromRawImage(GoAction1.Glyph.RawImage, False);
  GoAction3.Glyph.LoadFromRawImage(GoAction1.Glyph.RawImage, False);
end;

procedure TEditorForm.EastDirDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(EastDir.Text);
  PopulateForm;
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  FGameFile.Free;
end;

procedure TEditorForm.GoAction1Click(Sender: TObject);
begin
  FGameFile.Location:=Target1.ItemIndex+1;
  PopulateForm;
end;

procedure TEditorForm.GoAction3Click(Sender: TObject);
begin
  FGameFile.Location:=Target3.ItemIndex+1;
  PopulateForm;
end;

procedure TEditorForm.NorthDirDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(NorthDir.Text);
  PopulateForm;
end;

procedure TEditorForm.RoomListClick(Sender: TObject);
begin
  FGameFile.Location:=RoomList.ItemIndex+1;
  PopulateForm;
end;

procedure TEditorForm.GoAction2Click(Sender: TObject);
begin
  FGameFile.Location:=Target2.ItemIndex+1;
  PopulateForm;
end;

procedure TEditorForm.SaveBtnClick(Sender: TObject);
begin
  with FGameFile.World^ do
  begin
    m:=AreaTitle.Text;
    d1:=AreaDesc.Lines.Strings[0];
    d2:=AreaDesc.Lines.Strings[1];
    d3:=AreaDesc.Lines.Strings[2];
    mi1:=Action1.Text;
    mi2:=Action2.Text;
    mi3:=Action3.Text;
    m1:=Target1.ItemIndex+1;
    m2:=Target2.ItemIndex+1;
    m3:=Target3.ItemIndex+1;
    n:=StrToInt(NorthDir.Text);
    s:=StrToInt(SouthDir.Text);
    e:=StrToInt(EastDir.Text);
    w:=StrToInt(WestDir.Text);
    get:=ItemGet.Text;
    use:=ItemUse.Text;
    if use <> '' then
      if UseDrop.Checked then
      begin
        use1:=FGameFile.Location;
        use2:=StrToInt(UseLoc.Text);
        use3:=FGameFile.Location;
      end
      else if StrToInt(UseMove.Text) <> FGameFile.Location then
      begin
        use1:=FGameFile.Location;
        use2:=FGameFile.Location;
        use3:=StrToInt(UseMove.Text);
      end
      else
      begin
        use1:=StrToInt(UseLoc.Text);
        use2:=FGameFile.Location;
        use3:=FGameFile.Location;
      end;
  end;
  FGameFile.SaveAt(FGameFile.Location);
end;

procedure TEditorForm.SouthDirDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(SouthDir.Text);
  PopulateForm;
end;

procedure TEditorForm.UseLocDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(UseLoc.Text);
  PopulateForm;
end;

procedure TEditorForm.UseMoveDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(UseMove.Text);
  PopulateForm;
end;

procedure TEditorForm.WestDirDblClick(Sender: TObject);
begin
  FGameFile.Location:=StrToInt(WestDir.Text);
  PopulateForm;
end;

procedure TEditorForm.PopulateForm;
begin
  if RoomList.ItemIndex+1 <> FGameFile.Location then
  begin
    RoomList.ItemIndex:=FGameFile.Location-1;
    RoomList.MakeCurrentVisible;
  end;
  LocNumber.Caption:=IntToStr(FGameFile.Location);
  with FGameFile.World^ do
  begin
    AreaTitle.Text:=m;
    AreaDesc.Clear;
    AreaDesc.Lines.Add(d1);
    AreaDesc.Lines.Add(d2);
    AreaDesc.Lines.Add(d3);
    Action1.Text:=mi1;
    Action2.Text:=mi2;
    Action3.Text:=mi3;
    Target1.ItemIndex:=m1-1;
    Target2.ItemIndex:=m2-1;
    Target3.ItemIndex:=m3-1;
    NorthDir.Text:=IntToStr(n);
    SouthDir.Text:=IntToStr(s);
    EastDir.Text:=IntToStr(e);
    WestDir.Text:=IntToStr(w);
    ItemGet.Text:=get;
    ItemUse.Text:=use;
    UseDrop.Checked:=False;
    if use2 <> FGameFile.Location then
    begin
      UseDrop.Checked:=True;
      UseLoc.Text:=IntToStr(use2);
    end
    else if use1 <> FGameFile.Location then
      UseLoc.Text:=IntToStr(use1)
    else
      UseLoc.Text:=IntToStr(FGameFile.Location);
    if use3 <> FGameFile.Location then
      UseMove.Text:=IntToStr(use3)
    else
      UseMove.Text:=IntToStr(FGameFile.Location);
  end;
end;

end.

