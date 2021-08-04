* = $0200

CHRIN = $FFCF
CHROUT = $FFD2

start:
	; echo prompt
	LDX	#0
.l	LDA	prompt,X
	BEQ	input
	JSR	CHROUT
	INX
	JMP	.l

	; read name from input
input	LDX	#0
.l1	JSR	CHRIN
	STA	name,X
	INX
	CMP	#$0A
	BEQ	.l2
	JMP	.l1
.l2	LDA	#0
	STA	name,X


	; echo greeting
	TAX
.l3	LDA	greet,X
	BEQ	exit
	JSR	CHROUT
	INX
	JMP	.l3

	; return to OS
exit	RTS


prompt:	string "Enter your name: "
greet:	text "Hello, "

name:
