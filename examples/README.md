
There are several programs, illustrating some very *basic*
interactions with MineCraft-PI.

*) freefall

Source code: Freefall.hs 

Usage:
  freefall
  freefall jump

Increase the height of the player by jump blocks. If not specified the
jump is 100 blocks. A check is made to ensure that the jump does not
place the player into a block. Messages are sent to MineCraft to
inform the player what is happening.

There is no check that jump is valid (e.g. is positive, or not so
great that it exceeds the limits of the world), but this could be
added quite easily.
 
*) flatten

Source code: Flatten.hs 

Usage:
  flatten
  flatten radius

Flatten the area surrounding the user, converting the floor into gold
and removing any blocks above it. The area converted is a circle of
the given radius (this defaults to 5 if not given). The central tile
is not counted, so that a radius of 1 will change the blocks in front
of, behind, and to the side of the player.

There is no check that the blocks that are added are actually
supported, although this could be added by an enterprising programmer.

*) hmcpi

Source code: HMCPI.hs

Usage:
  hmcpi
  hmcpi < filename

This provides a direct interface to MineCraft-PI. You enter the full
commands, such as chat.post(Hello World!) or world.getBlock(0,0,0),
and see the response from the game. The program is essentially telnet
but specialized to only talk to MineCraft. There is currently no
attempt at providing a nicer interface, such as help, automatic
completion of commands, or cleaning user input.

Note that this program does not take use the hmcpi library, since it
is a low-level interface.

*) xjump

Source code: XJump.hs

Usage:
  xjump

Move the player by 10 tiles in the X direction if the tile is not
filled.
