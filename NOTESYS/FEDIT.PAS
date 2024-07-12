program fedit;

{ Started on July 9th, 2024}
{ Full-screen text editor test/toy }

uses crt, zscrsave, zwin;

var
  key: char;
  scr: PScreen;
  f: File of TScreen;
  scrid: integer;
  fg, bg: byte;

begin
  scr:=Nil;
  scrid:=-1;
  Assign(f, 'fedit.scr');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
    Rewrite(f);
  ClrScr;
  repeat
    repeat
    until KeyPressed;
    key:=ReadKey;
    if key = #0 then
    begin
      key:=ReadKey;
      case key of
        'P': GotoXY(WhereX, WhereY+1);
        'K': GotoXY(WhereX-1, WhereY);
        'H': GotoXY(WhereX, WhereY-1);
        'M': GotoXY(WhereX+1, WhereY);
        #15: GotoXY(WhereX+4, WhereY);
        #71: GotoXY(1, WhereY);
        #79: GotoXY(80, WhereY);
        #73: begin
          Dec(scrid);
          if scrid < 0 then
            scrid:=FileSize(f)-1;
          Seek(f, scrid);
          if scr = Nil then
            New(scr);
          Read(f, scr^);
          DrawScreen(scr);
        end;
        #81: begin
          Inc(scrid);
          if scrid > FileSize(f)-1 then
            scrid:=0;
          WriteLn(scrid);
          Seek(f, scrid);
          if scr = Nil then
            New(scr);
          Read(f, scr^);
          DrawScreen(scr);
        end;
        #59: begin
          if scr = Nil then
            New(scr);
          SaveScreen(scr);
          TextColor(2);
          TextBackground(4);
          OpenWindow('Color Select', 10,10,40,10);
          Write('Text Color: ');
          ReadLn(fg);
          Write('Text Background: ');
          ReadLn(bg);
          DrawScreen(scr);
          TextColor(fg);
          TextBackground(bg);
        end;
        #60: begin
          if scr = Nil then
            New(scr);
          SaveScreen(scr);
          TextColor(14);
          TextBackground(4);
          OpenWindow('Save Screen', 10, 10, 40, 10);
          if scrid = -1 then
            scrid:=FileSize(f);
          Seek(f, scrid);
          Write(f, scr^);
          WriteLn('Screen Saved to ',scrid);
          ReadLn;
          DrawScreen(scr);
        end;
        #61: begin
          if scr <> Nil then
            DrawScreen(scr)
          else
          begin
            New(scr);
            SaveScreen(scr);
          end;
          TextColor(14);
          TextBackground(1);
          OpenWindow('Load Screen', 10, 10, 40, 10);
          WriteLn('Screens available: ',FileSize(f));
          Write('Screen to load? ');
          ReadLn(scrid);
          if scrid < FileSize(f) then
          begin
            Seek(f, scrid);
            Read(f, scr^);
          end;
          DrawScreen(scr);
        end;
      else
        Write('^',ord(key));
      end;
    end
    else if key = #13 then
      WriteLn
    else if key = #8 then
      GotoXY(WhereX-1, WhereY)
    else if key = #9 then
      Write('    ')
    else if key > #31 then
      Write(key)
    else
      Write(ord(key));
  until key = #27;
  ClrScr;
  if scr <> Nil then
    Dispose(scr);
end.
