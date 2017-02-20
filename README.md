SYS6502 - a minimal MOS6502 simulator
=====================================


## Description
SYS6502 is a MOS6502 simulator written in FreePascal. It consists of a
command line simulator interface to load and run programs and to inspect
the state of the simulator, especially the CPU and RAM.

### Compiling the simulator
If FreePascal is installed the simulator can be compiled by simply running

	fpc sys6502

### The virtual machine
The CPU simulation currently supports all documented 6502 opcodes.

The virtual 6502 machine consists of 64k RAM ($0000-$FFFF). Programs to
be executed are expected to be loaded at address $0200, directly following
the stack memory ($0100-$01FF). The 6502 reset vector located at $FFFC is set
to $0200 upon reset.

Termination of programs loaded into the simulator is achieved by an absolute
jump to the reset vector $FFFC:

	JMP	$FFFC	; terminates the program

The simulator currently can load and execute plain binary files as programs.


### Using the simulator
After starting the simulator the user is presented a command line displaying
the current program counter (PC):

	PC=0200>

One can inspect the current CPU state by giving the command `c`:

	PC=FFFC> c
	PC=FFFC  A=00 X=00 Y=00 S=FA ...I..

The command displays the PC, the registers A, X, Y, the stack pointer and
flags.

The command `l <file>` loads the binary file `<file>` at address $0200.
Displaying memory can be accomplished page wise by typing `m <page>`,
where `<page>` is the page number to display. A page consists of 256 bytes.

The following example loads a 40 byte program at address $0200 and displays
the memory contents of page two containing the program.

	PC=FFFC> l prg/testaddr.prg
	40 bytes loaded at $0200 from prg/testaddr.prg
	PC=FFFC> m 2
	0200: A2 01 A0 00 A9 03 85 0C A9 01 85 02 85 03 8D 00
	0210: 03 18 69 01 65 02 75 02 6D 00 03 7D FF 02 79 00
	0220: 03 61 0A 71 0B 4C FC FF 00 00 00 00 00 00 00 00
	0230: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
	...
	... [ 0240-02EF skipped in documentation ]
	...
	02F0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00

To run a program until termination one has to issue the `r` (run) command.
To execute the next instration one can perform a `s` (step) operation.

Finally, to terminate the simulator, issue `q` (quit).

