DECLARE SUB saveditor ()
DECLARE SUB editor ()
DECLARE FUNCTION got% (inv$(), itm$)
DECLARE FUNCTION take% (inv$(), itm$)
DECLARE FUNCTION drop% (inv$(), itm$)
DECLARE SUB center (row%, text$)
DECLARE FUNCTION getkey$ ()
DECLARE FUNCTION domenu% (m1$, m2$, m3$, d%)
DEFINT A-Z
TYPE worldtype
  m AS STRING * 20
  d1 AS STRING * 80
  d2 AS STRING * 80
  d3 AS STRING * 80
  mi1 AS STRING * 40
  mi2 AS STRING * 40
  mi3 AS STRING * 40
  m1 AS INTEGER
  m2 AS INTEGER
  m3 AS INTEGER
  n AS INTEGER
  s AS INTEGER
  e AS INTEGER
  w AS INTEGER
  get AS STRING * 20
  use AS STRING * 20
  use1 AS INTEGER
  use2 AS INTEGER
  use3 AS INTEGER
END TYPE
TYPE playertype
  i AS INTEGER
  dta AS STRING * 20
END TYPE
c$ = COMMAND$
IF c$ = "/EDITOR" THEN
  editor
'  SYSTEM
ELSEIF c$ = "/SAVE" THEN
  saveditor
ELSE
  gam$ = c$
END IF
IF gam$ = "" THEN gam$ = "demo.dat"
DIM world AS worldtype
DIM player AS playertype
DIM inv$(1 TO 20)
OPEN gam$ FOR RANDOM AS #1 LEN = LEN(world)
GET #1, 1, world
title$ = RTRIM$(world.d1)
menu:
COLOR 0, 0: CLS
COLOR 14: center 8, title$
opt = 1
DO
opt = domenu("Begin New Adventure book", "Read an existing Adventure book", "Quit", opt)
IF opt = 1 THEN
  COLOR 7, 1: CLS
  PRINT "Adventure Kernel v1.0"
  PRINT "Copyright(c) 2006 Zyrix Networks"
  PRINT
  PRINT "Writing New Adventure book...";
  l = 1
  FOR i = 1 TO 20
    inv$(i) = ""
  NEXT i
  PRINT "done."
  center 20, "Press any key to ambark on your new adventure..."
  k$ = getkey$
  GOTO gamebegin
ELSEIF opt = 2 THEN
  COLOR 7, 1: CLS
  PRINT "Adventure Kernel v1.0"
  PRINT "Copyright(c) 2006 Zyrix Networks"
  PRINT
  INPUT "An eight letter word for your Adventure book: ", fi$
  PRINT "Reading your Adventure book from the library of tales...";
  y = POS(0)
  OPEN fi$ + ".sav" FOR RANDOM AS #2 LEN = LEN(player)
  GET #2, 1, player
  l = player.i
  IF RTRIM$(player.dta) <> gam$ THEN
    PRINT : PRINT "Warning: This is a current game in progress"
    PRINT "using the "; RTRIM$(player.dta); " module.": PRINT
    center 20, "Start a new quest using this character?"
    yn$ = getkey$
    IF LCASE$(yn$) = "y" THEN
      l = 1
    ELSE
      CLOSE #2
      GOTO menu
    END IF
    LOCATE 5, y
  END IF
  FOR i = 1 TO 20
    GET #2, i + 1, player
    inv$(i) = RTRIM$(player.dta)
  NEXT i
  CLOSE #2
  PRINT "done."
  center 20, "Press any key to return to your adventure..."
  k$ = getkey$
  GOTO gamebegin
ELSEIF opt = 3 THEN
  COLOR 7, 0: CLS : SYSTEM
ELSEIF opt = 99 THEN
  IF l > 0 THEN GOTO gamebegin
END IF
LOOP

gamebegin:
DO
  GET #1, l + 1, world
  m$ = RTRIM$(world.m)
  d1$ = RTRIM$(world.d1): d2$ = RTRIM$(world.d2): d3$ = RTRIM$(world.d3)
  m1$ = RTRIM$(world.mi1): m2$ = RTRIM$(world.mi2): m3$ = RTRIM$(world.mi3)
  m1 = world.m1: m2 = world.m2: m3 = world.m3
  n = world.n: s = world.s: e = world.e: w = world.w
  get$ = RTRIM$(world.get): use$ = RTRIM$(world.use)
  use = world.use1: use2 = world.use2: use3 = world.use3
  IF use3 <> l AND got(inv$(), use$) = 1 THEN l = use3: GOTO gamebegin
  COLOR 0, 0: CLS
  COLOR 14, 0: center 3, m$
  COLOR 7, 0: LOCATE 5, 1
  PRINT d1$
  PRINT d2$
  PRINT d3$
  IF get$ <> "" THEN
    IF take(inv$(), get$) = 1 THEN
      center 20, "You found a " + get$ + "!"
    END IF
  END IF
  IF n <> l THEN LOCATE 17, 40: PRINT "N"
  IF s <> l THEN LOCATE 19, 40: PRINT "S"
  IF e <> l THEN LOCATE 18, 41: PRINT "E"
  IF w <> l THEN LOCATE 18, 39: PRINT "W"
  COLOR 14, 0
  LOCATE 16, 5: PRINT "---=  Legend  =---"
  COLOR 15, 0
  LOCATE 17, 6: PRINT "Z - Status Screen"
  LOCATE 18, 6: PRINT "U - Use item"
  LOCATE 19, 6: PRINT "ESC - Menu"
  opt = domenu(m1$, m2$, m3$, 1)
  IF opt = 1 THEN l = m1
  IF opt = 2 THEN l = m2
  IF opt = 3 THEN l = m3
  IF opt = 10 THEN l = n
  IF opt = 11 THEN l = s
  IF opt = 12 THEN l = e
  IF opt = 13 THEN l = w
  IF opt = 19 AND got(inv$(), use$) = 1 THEN
    IF use <> l THEN
      l = use
    ELSEIF use2 <> l THEN
      IF drop(inv$(), use$) = 1 THEN l = use2
    END IF
  END IF
  IF opt = 20 THEN GOSUB zstats
LOOP UNTIL opt = 99
GOTO menu

zstats:
DO
COLOR 14, 0: CLS
center 3, "In-Game Player Menu"
opt = domenu("Inventory", "Write Adventure book", "Back to game", 1)
IF opt = 1 THEN
  COLOR 14, 0: CLS
  center 1, "---=  Inventory  =---"
  COLOR 8, 0
  FOR i = 1 TO 20
    center i + 1, inv$(i)
  NEXT i
  k$ = getkey$
  opt = 3
ELSEIF opt = 2 THEN
  COLOR 7, 1: CLS
  PRINT "Adventure Kernel v1.0"
  PRINT "Copyright(c) 2006 Zyrix Networks"
  PRINT
  INPUT "An eight letter word for your Adventure book: ", fi$
  PRINT "Writing your Adventure book to the library of tales...";
  OPEN fi$ + ".sav" FOR RANDOM AS #2 LEN = LEN(player)
  player.i = l
  player.dta = gam$
  PUT #2, 1, player
  FOR i = 1 TO 20
    player.dta = inv$(i)
    PUT #2, i + 1, player
  NEXT i
  CLOSE #2
  PRINT "done."
  k$ = getkey$
  opt = 3
END IF
LOOP UNTIL opt = 3
RETURN

SUB center (row, text$)
LOCATE row, 41 - LEN(text$) / 2
PRINT text$
END SUB

FUNCTION domenu (m1$, m2$, m3$, d)
opt = d
DO
IF opt = 1 THEN COLOR 0, 7
center 10, m1$
IF opt = 1 THEN COLOR 7, 0
IF opt = 2 THEN COLOR 0, 7
center 12, m2$
IF opt = 2 THEN COLOR 7, 0
IF opt = 3 THEN COLOR 0, 7
center 14, m3$
IF opt = 3 THEN COLOR 7, 0
k$ = getkey$
IF k$ = CHR$(27) THEN opt = 99: EXIT DO
lk$ = LCASE$(k$)
IF lk$ = "n" THEN opt = 10: EXIT DO
IF lk$ = "s" THEN opt = 11: EXIT DO
IF lk$ = "e" THEN opt = 12: EXIT DO
IF lk$ = "w" THEN opt = 13: EXIT DO
IF lk$ = "g" THEN opt = 18: EXIT DO
IF lk$ = "u" THEN opt = 19: EXIT DO
IF lk$ = "z" THEN opt = 20: EXIT DO
IF k$ = CHR$(0) + CHR$(72) THEN opt = opt - 1
IF k$ = CHR$(0) + CHR$(80) THEN opt = opt + 1
IF opt > 3 THEN opt = 3
IF opt < 1 THEN opt = 1
LOOP UNTIL k$ = CHR$(13) OR opt > 3
domenu = opt
END FUNCTION

FUNCTION drop (inv$(), itm$)
tmp = 0
FOR i = 1 TO 20
  IF inv$(i) = itm$ THEN inv$(i) = "": tmp = 1: EXIT FOR
NEXT i
drop = tmp
END FUNCTION

SUB editor
DIM world AS worldtype
COLOR 7, 1: CLS
PRINT "Adventure Kernel v1.0"
PRINT "Copyright(c) 2006 Zyrix Networks"
PRINT
PRINT "Adventure editor started."
INPUT "Adventure filename: ", fi$
IF fi$ = "" THEN fi$ = "demo.dat"
OPEN fi$ FOR RANDOM AS #1 LEN = LEN(world)
IF LOF(1) = 0 THEN
  LINE INPUT "World Title: ", world.d1
  PUT #1, 1, world
END IF
GET #1, 1, world
PRINT "World: "; RTRIM$(world.d1)
DO
  IF l > 0 THEN PRINT "Location#", l - 1
  INPUT " >", c$
  IF LEFT$(c$, 1) = "a" THEN
    IF l > 0 THEN PUT #1, l, world
    l = VAL(MID$(c$, 2)) + 1
    IF l = 1 THEN
      PUT #1, l, world
      l = 0
    ELSE
    GET #1, l, world
    IF world.n = 0 THEN
      world.m = "Untitled Area": world.d1 = "": world.d2 = "": world.d3 = ""
      world.mi1 = "": world.mi2 = "": world.mi3 = ""
      world.m1 = l - 1: world.m2 = l - 1: world.m3 = l - 1
      world.n = l - 1: world.s = l - 1: world.e = l - 1: world.w = l - 1
      world.use1 = l - 1: world.use2 = l - 1: world.use3 = l - 1
      world.get = "": world.use = ""
    END IF
    PRINT "Area", world.m
    PRINT world.d1
    PRINT world.d2
    PRINT world.d3
    PRINT "Item 1", world.mi1
    PRINT "Item 2", world.mi2
    PRINT "Item 3", world.mi3
    PRINT "M1", "M2", "M3"
    PRINT world.m1, world.m2, world.m3
    PRINT "N", "S", "E", "W"
    PRINT world.n, world.s, world.e, world.w
    PRINT "GET", "USE"
    PRINT RTRIM$(world.get), RTRIM$(world.use)
    PRINT "USE", "USE2", "USE3"
    PRINT world.use1, world.use2, world.use3
    END IF
  END IF
  IF l > 0 THEN
    l1$ = LEFT$(c$, 1)
    l2$ = LEFT$(c$, 2)
    IF l1$ = "n" THEN world.n = VAL(MID$(c$, 2)): PRINT "N-SET"
    IF l1$ = "s" THEN world.s = VAL(MID$(c$, 2)): PRINT "S-SET"
    IF l1$ = "e" THEN world.e = VAL(MID$(c$, 2)): PRINT "E-SET"
    IF l1$ = "w" THEN world.w = VAL(MID$(c$, 2)): PRINT "W-SET"
    IF l1$ = "c" THEN
      cl = VAL(MID$(c$, 2)) + 1
      GET #1, cl, world
      PUT #1, l, world
      PRINT "COPIED"
    END IF
    IF l2$ = "u1" THEN world.use1 = VAL(MID$(c$, 3)): PRINT "USE1-SET"
    IF l2$ = "u2" THEN world.use2 = VAL(MID$(c$, 3)): PRINT "USE2-SET"
    IF l2$ = "u3" THEN world.use3 = VAL(MID$(c$, 3)): PRINT "USE3-SET"
    IF l2$ = "m1" THEN world.m1 = VAL(MID$(c$, 3)): PRINT "M1-SET"
    IF l2$ = "m2" THEN world.m2 = VAL(MID$(c$, 3)): PRINT "M2-SET"
    IF l2$ = "m3" THEN world.m3 = VAL(MID$(c$, 3)): PRINT "M3-SET"
    IF l2$ = "m " THEN world.m = MID$(c$, 3): PRINT "M-SET"
    IF c$ = "items" THEN
      INPUT "GET=", world.get
      INPUT "USE=", world.use
    END IF
    IF c$ = "menu" THEN
      LINE INPUT "Menu1=", m1$
      IF m1$ <> "" THEN world.mi1 = m1$
      LINE INPUT "Menu2=", m2$
      IF m2$ <> "" THEN world.mi2 = m2$
      LINE INPUT "Menu3=", m3$
      IF m3$ <> "" THEN world.mi3 = m3$
    END IF
    IF c$ = "desc" THEN
      LINE INPUT "Desc1=", d1$
      IF d1$ <> "" THEN world.d1 = d1$
      LINE INPUT "Desc2=", d2$
      IF d2$ <> "" THEN world.d2 = d2$
      LINE INPUT "Desc3=", d3$
      IF d3$ <> "" THEN world.d3 = d3$
    END IF
    IF c$ = "v" THEN
      PRINT "Area", world.m
      PRINT world.d1
      PRINT world.d2
      PRINT world.d3
      PRINT "Item 1", world.mi1
      PRINT "Item 2", world.mi2
      PRINT "Item 3", world.mi3
      PRINT "M1", "M2", "M3"
      PRINT world.m1, world.m2, world.m3
      PRINT "N", "S", "E", "W"
      PRINT world.n, world.s, world.e, world.w
      PRINT "GET", "USE"
      PRINT RTRIM$(world.get), RTRIM$(world.use)
      PRINT "USE", "USE2", "USE3"
      PRINT world.use1, world.use2, world.use3
    END IF
  END IF
  IF c$ = "ls" THEN
    max = LOF(1) / LEN(world)
    max = max
    cnt = 0
    FOR i = 2 TO max
      GET #1, i, world
      PRINT i - 1, world.m
      cnt = cnt + 1
      IF cnt > 20 THEN k$ = getkey$: cnt = 0
    NEXT i
  END IF
LOOP UNTIL c$ = "exit"
CLOSE #1
COLOR 7, 0: CLS
END SUB

DEFSNG A-Z
FUNCTION getkey$
DO
  k$ = INKEY$
LOOP UNTIL k$ <> ""
getkey$ = k$
END FUNCTION

DEFINT A-Z
FUNCTION got (inv$(), itm$)
tmp = 0
FOR i = 1 TO 20
  IF inv$(i) = itm$ THEN tmp = tmp + 1
NEXT i
got = tmp
END FUNCTION

SUB saveditor
DIM player AS playertype
DIM world AS worldtype
COLOR 7, 1: CLS
PRINT "Adventure Kernel v1.0"
PRINT "Copyright(c) 2006 Zyrix Networks"
PRINT
PRINT "Savefile editor started."
INPUT "Savegame filename: ", fi$
OPEN fi$ + ".sav" FOR RANDOM AS #1 LEN = LEN(player)
GET #1, 1, player
PRINT "Current Quest World: ", player.dta
PRINT "Current World Location: ", player.i
PRINT "Scanning question world...";
OPEN RTRIM$(player.dta) FOR RANDOM AS #2 LEN = LEN(world)
GET #2, player.i + 1, world
desc$ = RTRIM$(world.m)
CLOSE #2
PRINT "done."
PRINT "Exact location in quest world: ", desc$
PRINT : PRINT "Displaying Inventory..."
FOR i = 1 TO 20
  GET #1, i + 1, player
  IF RTRIM$(player.dta) <> "" THEN PRINT i + 1, player.dta
NEXT i
DO
INPUT "Modify which data?", r
GET #1, r, player
IF r = 1 THEN
  PRINT "Saveheader:"
  PRINT "Location:", player.i
  PRINT "Quest file:", player.dta
  INPUT "New Location:", player.i
  INPUT "New Quest file:", q$
  IF q$ <> "" THEN player.dta = q$
ELSE
  PRINT "Inventory item:"
  PRINT "Current:", player.dta
  INPUT "New:", q$
  IF q$ <> "" THEN player.dta = q$
END IF
PUT #1, r, player
PRINT "Quit?"
k$ = getkey$
LOOP UNTIL k$ = "y"
CLOSE #1
END SUB

FUNCTION take (inv$(), itm$)
tmp = 0
FOR i = 1 TO 20
  IF inv$(i) = itm$ THEN take = 0: EXIT FUNCTION
NEXT i
FOR i = 1 TO 20
  IF inv$(i) = "" THEN inv$(i) = itm$: tmp = 1: EXIT FOR
NEXT i
take = tmp
END FUNCTION

