# DOS Toys
Some MS-DOS based projects I work on for fun.

I feel like I wanted a place to back up these projects, and they might be of interest to some people
into MS-DOS development/tinkering/development.

An interesting side note is this Raspberry Pi Zero project recently featured on *Tom's Hardware*:
[PicoVision and Emulation](https://www.tomshardware.com/maker-stem/rp2040-boards/windows-10-goes-back-to-the-future-with-the-help-of-raspberry-pi-picovision-and-emulation-let-you-gui-like-its-1985)

Here is a rundown on each project:

## DOORS2

Will be added to the repo in my next commit, I am adding this one first in my list as it was the first
one I was working on.  As the name implies, I had a first version of this, which I actually wrote as a
kid, and it's name is essentially a paraody to the *Windows* brand-name.  The OG version was made in
QBASIC I believe, and was only text mode, but did include a similar scripting engine to what you can see
in my next project in this list, *CGA System*.

Anyways, with the history lesson out of the way, this was an entirely new system built from scratch, as
my old childhood source code is gone.  It was built using CGA display output in mind, and had a few
interesting features.  I stopped working on it around the time I began developing the custom bytecode
engine at the time.  I may come back to this *toy* at some point in the future.

## CGA System

My latest attempt at writing a self-contained system running in CGA graphics mode, and the code I've been
mainly working on while essentially homeless and living out of my car.  I love to code, and I feel to
keep myself mentally sharp, coding and being creative is very important for the brain.  I give myself
tasks I want to add to this program and implement them as best as I can.  One huge highlight in this
project is the `CGACRT` unit which allows `Write` and `WriteLn` to otherwise output to the graphics mode
without needing to contantly make calls to `OutTextXY`.
