SYS6502 - a minimal MOS6502 simulator
=====================================


## Introduction
SYS6502 is a high level MOS6502 simulator. It consists of two modules, a
6502 CPU core (CPU6502) emulation and a minimal system interface (SYS6502)
to interact with the CPU core.

The CPU6502 core is implemented as a Pascal unit witch can be embedded in
other projects. The SYS6502 interface consists of a command line interface
to load and run programs and to inspect the state of the simulator, especially
the CPU and RAM. The CPU simulation currently supports all documented
6502 opcodes.

It is NOT a timing accurate CPU simulation capable of emulating real 8-bit
systems. There is currently no way to trigger external interrupts or NMIs, but
the BRK instruction may be used.

## Compiling the simulator
If FreePascal is installed the simulator can be compiled by simply running

	fpc sys6502

## The virtual machine
The virtual 6502 machine consists of 64k RAM ($0000-$FFFF). Programs to
be executed are expected to be loaded at address $0200, directly following
the stack memory ($0100-$01FF). The 6502 reset vector located at $FFFC is set
to $0200 upon reset.

Termination of programs loaded into the simulator is achieved by an absolute
jump to the reset vector $FFFC:

	JMP	$FFFC	; terminates the program

The simulator currently can load and execute plain binary files as programs.

## Using the simulator
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


## Input and Output
There are two simulator functions accessible from 6502. One for reading
characters from stdin and one for writing characters to stdout.

The functions are called like the C64 KERNAL functions `CHRIN` and `CHROUT`.
To get a character from stdin into the register A one can use the following
assembler routine:

    JSR $FFCF
    
To output the value of the Accumulator as a character to stdout one can write

    LDA <character value>
    JSR $FFD2

The following complete sample program reads a name from input and displays a
greeting message.

     1                          * = $0200
     2                          
     3                          CHRIN = $FFCF
     4                          CHROUT = $FFD2
     5                          
     6                                  ; echo prompt
     7  0200 a200                       LDX     #0
     8  0202 bd3102             -       LDA     prompt,X
     9  0205 f007                       BEQ     input
    10  0207 20d2ff                     JSR     CHROUT
    11  020a e8                         INX
    12  020b 4c0202                     JMP     -
    13                          
    14                                  ; read name from input
    15  020e a200               input   LDX     #0
    16  0210 20cfff             -       JSR     CHRIN
    17  0213 9d4a02                     STA     name,X
    18  0216 e8                         INX
    19  0217 c90a                       CMP     #$0A
    20  0219 f003                       BEQ     +
    21  021b 4c1002                     JMP     -
    22  021e a900               +       LDA     #0
    23  0220 9d4a02                     STA     name,X
    24                          
    25                          
    26                                  ; display greeting
    27  0223 aa                         TAX
    28  0224 bd4302             -       LDA     greet,X
    29  0227 f007                       BEQ     exit
    30  0229 20d2ff                     JSR     CHROUT
    31  022c e8                         INX
    32  022d 4c2402                     JMP     -
    33                          
    34                                  ; return to OS
    35  0230 60                 exit    RTS
    36                          
    37                          
    38  0231 456e74657220796f...prompt: !text "Enter your name: ", 0
    39  0243 48656c6c6f2c20     greet:  !text "Hello, "
    40                          name:

It follows the output of the simulator session:

    PC=0200> l prg/testchr.prg
    74 bytes loaded at $0200 from prg/testchr.prg
    PC=0200> r
    Enter your name: Bernd
    Hello, Bernd
    PC=FFFC> q

