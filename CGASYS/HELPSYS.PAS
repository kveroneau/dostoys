unit helpsys;

interface

uses scrsave, crt, graph, globals;

procedure HandleHelp;

implementation

var
  Row: byte;

procedure HO(s: string);
begin
  OutTextXY(0,Row*8,s);
  Inc(Row);
end;

procedure HelpTitle(title: string);
begin
  SetColor(1);
  HO(title);
  SetColor(2);
  HO('====================');
  SetColor(3);
end;

procedure HelpPage(title: string);
var
  c: char;
begin
  repeat
  until KeyPressed;
  c:=ReadKey;
  ClearDevice;
  Row:=0;
  HelpTitle(title);
end;

procedure HelpMain;
begin
  HelpTitle('Main Mode');
  HO('In this mode...');
end;

procedure HelpShell;
begin
  if InputMode <> imLine then
  begin
    SysMode:=smMain;
    Exit;
  end;
  HelpTitle('Shell Mode');
  HO(' EXIT - Exit Shell');
  HO(' HALT - Exit CGA System');
  HO(' CLS - Clear Screen restoring title');
  HO(' LS -  Show Total Blocks on Active VBD');
  HO(' RUN - Run a Script');
  HO(' MOUNT - Mount a Media VBD');
  HO(' MEDIA - Switch Active VBD to Media VBD');
  HO(' BOOTCODE - Show Boot Script from Active VBD');
  HO(' VER - Show current CGA System version');
  HO(' SEG - Show next free memory segment');
end;

procedure HelpRun;
begin
  if InputMode <> imLine then
  begin
    SysMode:=smMain;
    Exit;
  end;
  HelpTitle('Run Script Mode');
  HO(' [.] End Script  ** NEVER FORGET! **');
  HO(' [!] System Call');
  HO(' [#] VBD Call');
  HO(' [%] Sprite Call');
  HO(' [P] WriteLn');
  HO(' [p] Write');
  HO(' [T] Set App Title');
  HO(' [t] Clear and display App Title');
  HO(' [C] Enable Caps Mode');
  HO(' [c] Disable Caps Mode');
  HO(' [X] Execute Segment, eg. X1#Proc~');
  HO(' [x] Execute Segment from variable');
  HO(' [V] Enable Verbose Mode');
  HO(' [v] Disable Verbose Mode');
  HO(' [F] System 8x8 Font');
  HO(' [f] Small Font');
  HO(' [Z] Free Segment, eg. Z0#');
  HO(' [z] Free Segment from variable');
  HO(' [i] Clear Input Line buffer');
  HO(' [I] Request input into variable');
  HO(' ["] Set Color to 0-3, eg. "3#');
  HO(' [''] Set CGA Mode to 0-3, eg. ''3#');
  HelpPage('Script Code Page 2');
  HO(' [L] Set Cursor position, eg. L0#6#');
  HO(' [<] Save Screen to buffer');
  HO(' [>] Restore saved screen from buffer');
  HelpTitle('Using the RUN interface');
  HO('When running in system mode, you');
  HO('currently have two options, press ESC');
  HO('or press a capital ''R'', you can also');
  HO('use any of the 0-9 hotkeys in this mode.');
  HO('Pressing ''R'' will put you into the RUN');
  HO('interface, where you can directly enter');
  HO('any script code you want to execute');
  HO('immediately, much like a command line,');
  HO('but for running script code.');
  HelpTitle('Popular Script Code with RUN');
  HO('!S. - This will launch you into shell.');
  HO('      The other editor sys calls too.');
  HO('#mSHELL.VBD~#M#B!S.$.');
  HO('  Creates a bootable shell vbd.');
  HO('#L#2##>xApp2~.');
  HO('  Load script from block 2 and run it.');
  HelpPage('System Calls');
  HO(' [S] Start Shell, eg. !S.');
  HO(' [T] Start Tile Editor, eg. !T.');
  HO(' [E] Start Map Engine, eg. !E.');
  HO(' [M] Start Map Editor, eg. !M.');
  HO(' [X] Clear InputMode, eg. !X');
  HO(' [>] Add Shell command, eg.');
  HO('     !>hello{PHello World!~}');
  HO(' [~] Change String end terminator, eg.');
  HO('     !~^');
  HO(' [!] Exit CGA System, eg. !!.');
  HelpTitle('VBD Calls');
  HO(' [R] Select Root VBD, eg. #R');
  HO(' [M] Select Media VBD, eg. #M');
  HO(' [m] Mount Media, eg. #mTEST.VBD~');
  HO(' [B] Set Boot Code, eg. #BPBooted!~.`');
  HO('     - Use ` to end Boot Code.');
  HO(' [I] Import, eg. #I4#TEST.BIN~');
  HO(' [i] Import from variable');
  HO(' [L] Load Block, eg. #L#4#');
  HO(' [S] Save Block, eg. #S#4#');
  HO(' [>] Copy Block into next free segment.');
  HO(' [<] Copy Segment into Block, eg. #<4#');
  HelpPage('Sprite Calls');
  HO(' [D] Draw/Import Sprite, eg. %DTEST.SPR~');
  HO(' [A] Add Drawn Sprite to Set, eg. %A3#');
  HO(' [P] Put Sprite from Set, eg. %P50#50#3#');
  HO(' [R] Draw Rectangle,eg. %R10#10#100#100#');
  HO(' [B] Draw Filled Bar, refer to above eg');
  HO(' [F] Set Fill Style, eg. %F3#2#');
  HO(' [>] Copy memory into sprite buffer');
  HO('     #L#2##>%>');
  HO(' [m] Copy map data from memory, eg. ');
  HO('     #L#3##>%m');
  HO(' [M] Copy map data from VBD, eg. ');
  HO('     %M3# - Preferred method.');
  HO(' [L] Set Player location, eg. %L10#10#');
end;

procedure HelpMedia;
begin
  if InputMode <> imLine then
  begin
    SysMode:=smMain;
    Exit;
  end;
  HelpTitle('Media Virtual Block Device Help');
  HO('You are currently in a situation where');
  HO('you need to enter a filename of a');
  HO('Virtual Block Device, usually in the');
  HO('.VBD extension.');
end;

procedure HelpINone;
begin
  HelpTitle('System Commands');
  HO(' [Q] Quit CGA System');
  {$IFDEF DEBUG}
  HO(' [C] Enter Character Mode Debug');
  HO(' [L] Enter Line Input Mode Debug');
  HO(' [Space] Swap CapsMode');
  HO(' [l] Inject Line and enter mode');
  {$ENDIF}
  HO(' [R] Run Script');
end;

procedure HelpIChar;
begin
  HelpTitle('Character Mode');
  HO('A Rare mode to be in...');
end;

procedure HelpILine;
begin
  HelpTitle('Line Input Mode');
  HO('A simple line input entry mode.');
  HO('There are some special keys, other');
  HO('than backspace...');
  HO('  [Home] Clear and display App Title');
  HO('  [End] Exit Line Input Mode');
  HO('  [Esc] Clear the Input Line');
end;

procedure HandleHelp;
begin
  SaveScreen;
  ClearDevice;
  Row:=0;
  HO('System Status Help Info:');
  case SysMode of
    smMain: HelpMain;
    smShell: HelpShell;
    smRun: HelpRun;
    smMedia: HelpMedia;
  end;
  HO('--------------');
  case InputMode of
    imNone: HelpINone;
    imChar: HelpIChar;
    imLine: HelpILine;
  end;
  repeat
  until KeyPressed;
  RestoreScreen;
end;

end.
