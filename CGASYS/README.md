# CGA System Source Information Sheet

In this document, I will go through each of the source files to explain what they each do.

## CGASSYS.PAS

The main entry point of the program, the source file you should compile to obtain a runnable
binary image.

The main program code handles the main event loop of the system, along with most of the code
needed to get the system up and running.

## GLOBALS.PAS

A bunch of global variables used throughout the application code, usually needed by most
other units.  I also contains a few global type definitions, and namely the global enumerators
for the various system and input states available during runtime.

## UTILS.PAS

Currently only has one utility function, similar to modern ObjectPascal's `IntToStr` function.

## CGALIB.PAS

Currently only has one utility function to draw the upper title.

## CGACRT.PAS

Contains the code for a Text file compatible Output driver for text output to a CGA display.

## VBD.PAS

The *Virtual Block Device* driver code.  This unit manages all access to the underlying
**.VBD** files.

## MEMSYS.PAS

The *Memory System* unit, where all the heap memory is located and managed by the application
code.

## SCRSYS.PAS

The *Script System* unit, this is where the scripting engine is, and it manages all the loaded
tasks.  This unit is special in a way in that not many units can `use` it, as it needs to be
able to allow programmable access to most, if not all the systems available.

## SCRSAVE.PAS

A short unit file to easily manage the saving and restoring of a single CGA graphical screen.

## SHELLSYS.PAS

The built-in *Shell System* which can be extended using the Scripting engine to add new shell
commands which can be run by the user interacting with the system.

## HELPSYS.PAS

Another stand-alone unit file which will save the screen and display full-screen help to the
user depending on which system mode they are in.  I try and keep it up to date, but it is
usually a bit off.

## WIDGETS.PAS

Contains various screen widgets, such as the upper left status display, and the upper right
clock display.

## TILEEDIT.PAS

The *CGA Tile Editor*, which can be used to either create sprites for use in programs, or for
use with the map editor and map engine.

## MAPEDIT.PAS

Takes the tiles created with the *Tile Editor* and allows the user to lay them out into a map.

This map can be either used with the tile engine, or displayed to the user as-is via a script.

When used with the *CGA Map Engine*, script events can be assigned to map tiles which can then
be executed when the player character steps onto them.

## ENGINE.PAS

A basic *CGA Tile Map Engine* for use with basic Adventure or RPG like games.  With the map
editor, a user can even assign events which can then be run when the player is interacting
with the map during runtime to create a more interactive experience.

## GFXLIB.PAS

A small unit file with the type information needed for the Sprite format used by the tile
engines.

## MAPLIB.PAS

A small unit file with the type information needed for the map format used by the map engines.
