# Various DOS related Units

In this directory is a library of some useful DOS related functions I have used in a variety of programs.

## EMS.PAS

This library is a slightly modified version of the EMS.PAS example program which shipped with Turbo
Pascal for MS-DOS, but modified to run within a reusable unit file, rather than as a program example.

## LEAKCHK.PAS

A simple unit to compile in during development which is similar, but also much more limited to the
modern `heaptrc` seen in modern ObjectPascal, but is very useful for MS-DOS development to detect
memory leaks within the program.  Program should be run after each substantial update which allocates
memory to determine the exact potential location of the leak, unlike heaptrc, it cannot export a nice
leak file which can then be opened to see exactly where the leak is, this is much more limited than
heaptrc in modern ObjectPascal.

## ZMOUSE.PAS

A simple mouse unit which is reusable in mostly any program.  The Z prefix is part of how I've named
this library when it was originally built for *QuickBasic PDS 7.1* back in the day before I ported it
over to Turbo Pascal, as seen here.

## ZSCREMS.PAS

Why let all that *expanded memory* go to waste?  With this unit, you can store a textual screen into
EMS memory, which can then later be loaded and displayed back to the user.  Very handy when developing
windowing based systems, or systems where you may need to temporarily swap the sceen out with new
information.

## ZSCRSAVE.PAS

A unit which allows the saving and loading of the text screen buffer located in the standard IBM PC
location of `0xB800`.

## ZWIN.PAS

Very primitive window drawing, should be combined with other systems to be more useful, but by itself,
it can draw a simple window on the display for the user to slighly interact with.
