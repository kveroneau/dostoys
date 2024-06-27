# CGA System Virtual Block Devices

In this directory are some ready to run VBDs which you can use after compiling the main
program.  In this document, I will explain what you will expect when running each one.

## PREPSYS.VBD

This is a very special VBD in that it is meant to prep a new VBD and make it bootable.

`CGASYS.EXE MYFILE.VBD PREPSYS.VBD`

The above is an example of how it should be run, as running it alone will not provide usable
results.

## MKENGINE.VBD

This is another special VBD in that, it is meant to bootstrap game engines with a tileset and
a map file to load in, along with a bootable script for the map.

`CGASYS.EXE MYGAME.VBD MKENGINE.VBD`

Follow the prompts as directed for each of the 3 needed files:

  * A Map Script file.
  * A Tileset file.
  * A Map file.

## TEST1.VBD

A fully working tile engine example program, just run it with CGASYS as such to see:

`CGASYS.EXE TEST1.VBD`

## TOUR.VBD

My attempts at creating a *tour program* to show the program off and to answer some questions
about the system while guiding a new user through it.  It's incomplete as of this writing.

## WELCOME.VBD

A simple welcome program, which will actually launch the above **TOUR.VBD** with the new
built-in shell command `tour`.
