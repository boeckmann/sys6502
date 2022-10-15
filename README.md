SYS6502 - a minimal MCS6502 simulator
=====================================


## Introduction
SYS6502 is a high level MCS6502 simulator. It consists of two modules: CPU6502,
an embeddable CPU core implemented as Pascal unit and SYS6502, a minimal
system / virtual machine to interact with the CPU core. It consists of a command
line interface to load and run programs and to inspect the state of the
simulator, especially the CPU and RAM.

The CPU simulation currently supports all documented 6502 opcodes.
It is NOT a timing accurate CPU simulation capable of emulating real 8-bit
systems. There is currently no way to trigger external interrupts or NMIs, but
the BRK instruction may be used.

## Compiling the simulator
If FreePascal is installed the simulator can be compiled by simply running

	fpc sys6502

## Embedding the CPU6502 core
The CPU core requires two external routines which implement memory access
(reads and writes). The routine types are

    TLoadFunc = function(const addr: word) : byte;
    TStoreProc = procedure(const addr: word; const m: byte);

The CPU of type `TCpu6502` must be initialized by the `Init` method which
requires as arguments pointers to the memory access routines:

    var
      cpu: TCpu6502;
    begin
      cpu.Init(@LoadMem, @StoreMem);
    end;

To emulate 6502 operating system kernel features one can register builtin
subroutines written in Pascal. The declaration of the register function is

    procedure TCpu6502.InstallBuiltinProc(const addr: word; func: TBuiltinProc);

and

    TBuiltinProc = procedure(cpu: PCpu6502);

Once the program counter of CPU6502 reaches an address corresponding to a
registered builtin function, that function gets called instead of
executing the 6502 instruction stored at the given memory location.

The following example registeres a character output routine. Note that the
routine is expected to be called via a JSR (call subroutine) instruction and
therefore a RTS (return from subroutine) instruction is executed as last
statement of the SysChrout routine.

    procedure SysChrout(cpu: PCpu6502);
    begin
      Write(Chr(cpu^.A)); Flush(Stdout);
      cpu^.OpRTS;
    end;

In the following example the routine is installed to memory location $FFD2,
in accordance to the C64 CHROUT KERNAL routine.

    cpu.InstallBuiltinProc($FFD2, @SysChrout);

## The SYS6502 simulator / virtual machine
The SYS6502 virtual 6502 machine consists of 64k RAM ($0000-$FFFF). Programs to
be executed are expected to be loaded at address $0200, directly following
the stack memory ($0100-$01FF). The 6502 reset vector located at $FFFC is set
to $0200 upon reset.

Termination of programs loaded into the simulator is achieved by an absolute
jump to the reset vector $FFFC:

	JMP	$FFFC	; terminates the program

Alternatively the program may be terminated by a RTS instruction, because
at program start the stack contains the termination address $FFFC as its
only element.

The simulator currently can load and execute plain binary files as programs.

## Using the simulator
A 6502 program can be started from command line by giving it as command line
argument to the simulator. The program is loaded at address $0200 and executed.

Alternatively the simulator may be started in interactive mode by not entering
any command line arguments. Then, after starting the simulator,
the user is greeted by command prompt displaying the current program counter (PC):

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
greeting message. It may be assembled with my
[ASM6502 assembler](https://github.com/boeckmann/asm6502).

	; Hello world example for sys6502 emulator

	.ORG $0200

	CHRIN = $FFCF
	CHROUT = $FFD2

	start:
		; echo prompt
		LDX	#0
	@l	LDA	prompt,X
		BEQ	input
		JSR	CHROUT
		INX
		JMP	@l

		; read name from input
	input	LDX	#0
	@l1	JSR	CHRIN
		STA	name,X
		INX
		CMP	#$0A
		BEQ	@l2
		JMP	@l1
	@l2	LDA	#0
		STA	name,X


		; echo greeting
	output	TAX
	@l	LDA	greet,X
		BEQ	exit
		JSR	CHROUT
		INX
		JMP	@l

		; return to OS
	exit	RTS


	prompt:	.byte "Enter your name: ", 0
	greet:	.byte "Hello, "
	name:

It follows the output of the simulator session:

    PC=0200> l prg/testchr.prg
    74 bytes loaded at $0200 from prg/testchr.prg
    PC=0200> r
    Enter your name: Bernd
    Hello, Bernd
    PC=FFFC> q

