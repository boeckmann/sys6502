unit cpu6502;

{$MODE objfpc}{$H+}

{$OVERFLOWCHECKS off}   // some code depends on value wrap around
{$RANGECHECKS off}      // also disable rangechecks for speed

interface

type

TLoadFunc = function(const addr: word) : byte;
TStoreProc = procedure(const addr: word; const m: byte);

TCpu6502 = object
  { cpu registers }
  { }
  PC: word;		//< 16-bit program counter PC

  A: byte;		//< 8-bit accumulator register A
  X: byte;		//< 8-bit register X
  Y: byte;		//< 8-bit register Y
  FlagC: boolean;	//< carry flag
  FlagZ: boolean;	//< zero flag
  FlagI: boolean;	//< interrupt disable flag
  FlagD: boolean;	//< decimal flag
  FlagB: boolean;	//< break flag
  FlagV: boolean;	//< overflow flag
  FlagN: boolean;	//< negative (sign) flag

  { function pointers to external memory interface }
  { }
  LoadByte: TLoadFunc;		//< load byte from external memory
  StoreByte: TStoreProc;	//< store byte to external memory

  { opcode function dispatch table }
  OpTbl: array[0..255] of procedure of object;

  constructor Init(const load: TLoadFunc; const store: TStoreProc);
  procedure ResetCPU;

  { fetch and execute next instruction }
  procedure ExecuteNext;
  { fetch and execute next instruction end dexplay cpu status }
  procedure ExecuteTo(pcBreak: word);
  procedure ExecuteToWithDump(pcBreak: word);

  { fetch next byte from PC address and increment PC by 1 }
  function LoadByteIncPC : byte; inline;
  { fetch next word from PC address and increment PC by 2 }
  function LoadWord(const addr: word) : word; inline;
  function LoadWordIncPC : word; inline;

  { add a signed byte to current PC }
  procedure PCAddByteSigned(const addr: byte);

  { addressing functions }
  { }
  function LoadImm : byte; inline;
  function LoadZp : byte; inline;
  function LoadZpWithAddr(out addr: word) : byte; inline;
  procedure StoreZp(m: byte); inline;
  function LoadZpX : byte; inline;
  function LoadZpXWithAddr(out addr: word) : byte; inline;
  procedure StoreZpX(const m: byte);
  function LoadZpY : byte; inline;
  procedure StoreZpY(const m: byte);
  function LoadAbs : byte; inline;
  function LoadAbsWithAddr(out addr: word) : byte; inline;
  procedure StoreAbs(const m: byte); inline;
  function LoadAbsX : byte; inline;
  function LoadAbsXWithAddr(out addr: word) : byte; inline;
  procedure StoreAbsX(const m: byte); inline;
  function LoadAbsY : byte; inline;
  procedure StoreAbsY(const m: byte); inline;
  function LoadIndX : byte; inline;
  procedure StoreIndX(const m: byte); inline;
  function LoadIndY : byte; inline;
  procedure StoreIndY(const m: byte); inline;

  { ALU routines }
  { }
  procedure AluADC(const m: byte); inline;
  procedure AluSBC(const m: byte); inline;
  procedure AluAND(const m: byte); inline;
  procedure AluORA(const m: byte); inline;
  procedure AluEOR(const m: byte); inline;
  procedure AluCMP(const m: byte); inline;
  function AluASL(const m: byte) : byte; inline;

  procedure AluUpdateFlags(const op1: byte; const op2: byte; const res: word); inline;
  procedure AluUpdateNZ(const op: byte); inline;
  procedure AluUpdateNZC(const op: byte); inline;

  { opcode execution routines }
  { }
  procedure InitOpTbl;
  procedure OpASLzp;  //< opcode $06 - arithmetic shift left zp
  procedure OpASL;    //< opcode $0A - arithmetic shift left accumulator
  procedure OpASLabs; //< opcode $0E - arithmetic shift left abs
  procedure OpBPL;    //< opcode $10 - branch on PLus
  procedure OpASLzpX; //< opcode $16 - arithmetic shift left zp,X
  procedure OpCLC;    //< opcode $18 - clear carry
  procedure OpASLabsX;//< opcode $1E - arithmetic shift left abs,X
  procedure OpANDindX;//< opcode $21 - and A with (ind,X)
  procedure OpANDzp;  //< opcode $25 - and A with zp
  procedure OpANDimm; //< opcode $29 - and A with imm
  procedure OpANDabs; //< opcode $2D - and A with abs
  procedure OpBMI;    //< opcode $30 - branch on MInus
  procedure OpANDindY;//< opcode $31 - and A with (ind),Y
  procedure OpANDzpX; //< opcode $35 - and A with zp,X
  procedure OpSEC;    //< opcode $38 - set carry flag
  procedure OpANDabsY;//< opcode $39 - and A with abs,Y
  procedure OpANDabsX;//< opcode $3D - and A with abs,X
  procedure OpJMPabs; //< opcode $4C - jump absolute
  procedure OpBVC;    //< opcode $50 - branch if overflow clear
  procedure OpCLI;    //< opcode $58 - clear interrupt enable flag
  procedure OpADCindX;//< opcode $61 - add (ind,X) to accumulator with carry
  procedure OpADCzp;  //< opcode $65 - add zp to accumulator with carry
  procedure OpADCimm; //< opcode $69 - add imm to accumulator with carry
  procedure OpADCindY;//< opcode $71 - add (ind),Y to accumulator with carry
  procedure OpADCabs; //< opcode $6D - add abs to accumulator with carry
  procedure OpBVS;    //< opcode $70 - branch if overflow set
  procedure OpADCzpX; //< opcode $75 - add zp,X to accumulator with carry
  procedure OpADCabsY;//< opcode $79 - add abs,Y to accumulator with carry
  procedure OpADCabsX;//< opcode $7D - add abs,X to accumulator with carry
  procedure OpSEI;    //< opcode $78 - set interrupt enable flag
  procedure OpSTAindX;//< opcode $81 - store A at (ind,X)
  procedure OpSTYzp;  //< opcode $84 - store Y at zp
  procedure OpSTAzp;  //< opcode $85 - store accumulator at zp
  procedure OpSTXzp;  //< opcode $86 - store X at zp
  procedure OpDEY;    //< opcode $88 - decrement y
  procedure OpTXA;    //< opcode $8A - transfer X to A
  procedure OpSTYabs; //< opcode $8C - store Y at abs
  procedure OpSTAabs; //< opcode $8D - store A at abs
  procedure OpSTXabs; //< opcode $8E - store X at abs
  procedure OpBCC;    //< opcode $90 - branch if carry clear
  procedure OpSTAindY;//< opcode $91 - store A at (ind),Y
  procedure OpSTYzpX; //< opcode $94 - store Y at zp,X
  procedure OpSTAzpX; //< opcode $95 - store A at zp,X
  procedure OpSTXzpY; //< opcode $96 - store X at zp,Y
  procedure OpTYA;    //< opcode $98 - transfer Y to A
  procedure OpSTAabsY;//< opcode $99 - store A at abs,Y
  procedure OpSTAabsX;//< opcode $9D - store A at abs,X
  procedure OpLDYimm; //< opcode $A0 - load Y with imm
  procedure OpLDAindX;//< opcode $A1 - load A with (ind,X)
  procedure OpLDXimm; //< opcode $A2 - load X with imm
  procedure OpLDYzp;  //< opcode $A4 - load Y with zp
  procedure OpLDAzp;  //< opcode $A5 - load A with zp
  procedure OpLDXzp;  //< opcode $A6 - load X with zp
  procedure OpTAY;    //< opcode $A8 - transfer A to Y
  procedure OpLDAimm; //< opcode $A9 - load accumulator with immediate
  procedure OpTAX;    //< opcode $AA - transfer A to X
  procedure OpLDYabs; //< opcode $AC - load Y with abs
  procedure OpLDAabs; //< opcode $AD - load A with abs
  procedure OpLDXabs; //< opcode $AE - load X with abs
  procedure OpBCS;    //< opcode $B0 - branch if carry set
  procedure OpLDAindY;//< opcode $B1 - load A with (ind),Y
  procedure OpLDYzpX; //< opcode $B4 - load Y with zp,X
  procedure OpLDAzpX; //< opcode $B5 - load A with zp,X
  procedure OpLDXzpY; //< opcode $B6 - load X with zp,Y
  procedure OpCLV;    //< opcode $B8 - clear overflow flag
  procedure OpLDAabsY;//< opcode $B9 - load A with abs,Y
  procedure OpLDYabsX;//< opcode $BC - load Y with abs,X
  procedure OpLDAabsX;//< opcode $BD - load A with abs,X
  procedure OpLDXabsY;//< opcode $BE - load X with abs,Y
  procedure OpCMPindX;//< opcode $C1 - compare A with (ind,X)
  procedure OpCMPzp;  //< opcode $C5 - compare A with zp
  procedure OpINY;    //< opcode $C8 - increment Y
  procedure OpCMPimm; //< opcode $C9 - compare accumulator with immediate
  procedure OpDEX;    //< opcode $CA - decrement X
  procedure OpCMPabs; //< opcode $CD - compare A with abs
  procedure OpBNE;    //< opcode $D0 - branch if not equal
  procedure OpCMPindY;//< opcode $D1 - compare A with (ind),Y
  procedure OpCMPzpX; //< opcode $D5 - compare A with zp,X
  procedure OpCLD;    //< opcode $D8 - clear decimal flag
  procedure OpCMPabsY;//< opcode $D9 - compare A with abs,Y
  procedure OpCMPabsX;//< opcode $DD - compare A with abs,X
  procedure OpINX;    //< opcode $E8 - increment X
  procedure OpNOP;    //< opcode $EA - no operation
  procedure OpBEQ;    //< opcode $F0 - branch if equal
  procedure OpSED;    //< opcode $F8 - set decimal flag

  { debugging routines }
  { }
  procedure DumpRegs;	//< dump all registers
end;

implementation

uses
  sysutils;

const
  BIT_0 = $001; BIT_1 = $002; BIT_2 = $004; BIT_3 = $008;
  BIT_4 = $010; BIT_5 = $020; BIT_6 = $040; BIT_7 = $080;
  BIT_8 = $100;


constructor TCpu6502.Init(const load: TLoadFunc; const store: TStoreProc);
begin
  LoadByte := load;
  StoreByte := store;

  InitOpTbl;  
end;

procedure TCpu6502.ResetCPU;
begin
  PC := LoadWord($FFFC);
end;

{--- high level execution routines  ------------------------------------------}

procedure TCpu6502.ExecuteNext;
var
  opcode: byte;
begin
  opcode := LoadByteIncPC;
  OpTbl[opcode]();
end;

procedure TCpu6502.ExecuteTo(pcBreak: word);
var
  opcode: byte;
begin
  while PC <> pcBreak do begin
    opcode := LoadByteIncPC;
    OpTbl[opcode]();
  end;
end;

procedure TCpu6502.ExecuteToWithDump(pcBreak: word);
var
  opcode: byte;
begin
  while PC <> pcBreak do begin
    opcode := LoadByteIncPC;
    OpTbl[opcode]();
    DumpRegs;
  end;
end;

{--- PC and memory access functions -------------------------------------------}

{ load byte from PC abs, PC <- PC + 1 }
function TCpu6502.LoadByteIncPC : byte;
begin
  LoadByteIncPC := LoadByte(PC);
  inc(PC);
end;

function TCpu6502.LoadWord(const addr : word) : word;
var l, h: word;
begin
  l := LoadByte(addr);
  h := LoadByte(addr+1) shl 8;
  LoadWord := l or h;
end;

{ load word from PC abs, PC <- PC + 2 }
function TCpu6502.LoadWordIncPC : word;
var
  tmp: word;
begin
  tmp := LoadWord(PC);
  PC := PC + 2;
  LoadWordIncPC := tmp;
end;

{ addressing modes implementation }

function TCpu6502.LoadImm : byte;
begin
  LoadImm := LoadByte(PC);
  inc(PC);
end;

function TCpu6502.LoadZp : byte;
var
  addr: word;
begin
  addr := LoadByteIncPC;
  LoadZP := LoadByte(addr);
end;

function TCpu6502.LoadZpWithAddr(out addr: word) : byte;
begin
  addr := LoadByteIncPC;
  LoadZPWithAddr := LoadByte(addr);
end;

procedure TCpu6502.StoreZp(m: byte); inline;
var
  addr: word;
begin
  addr := LoadByteIncPC;
  StoreByte(addr, m);
end;

function TCpu6502.LoadZpX : byte;
var
  addr: word;
begin
  addr := (LoadByteIncPC + X) and $FF;
  LoadZpX := LoadByte(addr);
end;

function TCpu6502.LoadZpXWithAddr(out addr: word) : byte;
begin
  addr := (LoadByteIncPC + X) and $FF;
  LoadZpXWithAddr := LoadByte(addr);
end;

procedure TCpu6502.StoreZpX(const m: byte);
var
  addr: word;
begin
  addr := (LoadByteIncPC + X) and $FF;
  StoreByte(addr, m);
end;

function TCpu6502.LoadZpY : byte;
var
  addr: word;
begin
  addr := (LoadByteIncPC + Y) and $FF;
  LoadZpY := LoadByte(addr);
end;

procedure TCpu6502.StoreZpY(const m: byte);
var
  addr: word;
begin
  addr := (LoadByteIncPC + Y) and $FF;
  StoreByte(addr, m);
end;

function TCpu6502.LoadAbs : byte;
var
  addr: word;
begin
  addr := LoadWordIncPC;
  LoadAbs := LoadByte(addr);
end;

function TCpu6502.LoadAbsWithAddr(out addr: word) : byte;
begin
  addr := LoadWordIncPC;
  LoadAbsWithAddr := LoadByte(addr);
end;

procedure TCpu6502.StoreAbs(const m : byte);
var
  addr: word;
begin
  addr := LoadWordIncPC;
  StoreByte(addr, m);
end;

function TCpu6502.LoadAbsX : byte;
var
  addr: word;
begin
  addr := LoadWordIncPC + X;
  LoadAbsX := LoadByte(addr);
end;

function TCpu6502.LoadAbsXWithAddr(out addr: word) : byte;
begin
  addr := LoadWordIncPC + X;
  LoadAbsXWithAddr := LoadByte(addr);
end;

procedure TCpu6502.StoreAbsX(const m : byte);
var
  addr: word;
begin
  addr := LoadWordIncPC + X;
  StoreByte(addr, m);
end;

function TCpu6502.LoadAbsY : byte;
var
  addr: word;
begin
  addr := LoadWordIncPC + Y;
  LoadAbsY := LoadByte(addr);
end;

procedure TCpu6502.StoreAbsY(const m : byte);
var
  addr: word;
begin
  addr := LoadWordIncPC + Y;
  StoreByte(addr, m);
end;

function TCpu6502.LoadIndX : byte;
var
  zp: byte;
  addr: word;
begin
  zp := (LoadByteIncPC + X) and $ff;
  addr := LoadByte(zp) or LoadByte((zp + 1) and $ff) shl 8;
  LoadIndX := LoadByte(addr);
end;

procedure TCpu6502.StoreIndX(const m: byte);
var
  zp: byte;
  addr: word;
begin
  zp := (LoadByteIncPC + X) and $ff;
  addr := LoadByte(zp) or LoadByte((zp + 1) and $ff) shl 8;
  StoreByte(addr, m);
end;

function TCpu6502.LoadIndY : byte;
var
  zp: byte;
  addr: word;
begin
  zp := LoadByteIncPC;
  addr := LoadByte(zp) or LoadByte((zp + 1) and $ff) shl 8;
  LoadIndY := LoadByte(addr + Y);
end;

procedure TCpu6502.StoreIndY(const m: byte);
var
  zp: byte;
  addr: word;
begin
  zp := LoadByteIncPC;
  addr := LoadByte(zp) or LoadByte((zp + 1) and $ff) shl 8;
  StoreByte(addr + Y, m);
end;


{ add signed byte to PC }
procedure TCpu6502.PCAddByteSigned(const addr: byte);
var
  tmp: byte;
begin
  if (addr and BIT_7) <> 0 then begin
    { negative relative value }
    tmp := (addr xor $FF) + 1;
    PC := PC - tmp;
  end else begin
    PC := PC + addr;
  end;
end;


{--- ALU functions -----------------------------------------------------------}

procedure TCpu6502.AluADC(const m: byte);
var
  oldA: byte;
  tmp: word;
begin
  oldA := A;
  tmp := A + m;
  if FlagC then inc(tmp);
  A := byte(tmp);

  AluUpdateFlags(oldA, m, tmp);
end;

procedure TCpu6502.AluSBC(const m: byte); inline;
begin
  AluADC(m xor $ff);
end;

procedure TCpu6502.AluAND(const m: byte); inline;
begin
  A := A and m;
  AluUpdateNZ(A);
end;

procedure TCpu6502.AluORA(const m: byte); inline;
begin
  A := A and m;
  AluUpdateNZ(A);
end;

procedure TCpu6502.AluEOR(const m: byte);
begin
  A := A xor m;
  AluUpdateNZ(A);
end;

procedure TCpu6502.AluCMP(const m: byte);
var
  tmp: byte;
begin
  tmp := A - m;
  AluUpdateNZC(tmp);
end;

function TCpu6502.AluASL(const m: byte) : byte;
begin
  FlagC := (m and $80) <> 0;
  AluASL := m shl 1;
  AluUpdateNZ(AluASL);
end;

procedure TCpu6502.AluUpdateFlags(const op1: byte; const op2: byte; const res: word);
begin
  FlagC := ((res and BIT_8) <> 0);
  FlagZ := ((res and $ff) = 0);
  FlagN := ((res and BIT_7) <> 0);
  FlagV := (((op1 and BIT_7) = (op2 and BIT_7))) and ((op1 and BIT_7) <> (res and BIT_7));
end;

procedure TCpu6502.AluUpdateNZ(const op: byte);
begin
  FlagZ := ((op and $ff) = 0);
  FlagN := ((op and BIT_7) <> 0);
end;

procedure TCpu6502.AluUpdateNZC(const op: byte);
begin
  FlagC := ((op and BIT_8) <> 0);
  FlagZ := ((op and $ff) = 0);
  FlagN := ((op and BIT_7) <> 0);
end;


{--- opcode implementation ---------------------------------------------------}

procedure TCpu6502.OpASLzp; {opcode $06 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluASL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpASL; { opcode $0A }
begin
  A := AluASL(A);
end;

procedure TCpu6502.OpASLabs; {opcode $0E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluASL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpASLzpX; {opcode $16 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluASL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpBPL;    { opcode $10 - branch on PLus }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagN then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpCLC;    { opcode $18 - clear carry flag }
begin
  FlagC := false;
end;

procedure TCpu6502.OpASLabsX; {opcode $1E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluASL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpANDindX; { opcode $21 }
begin
  AluAND(LoadIndX);
end;

procedure TCpu6502.OpANDzp; { opcode $25 }
begin
  AluAND(LoadZp);
end;

procedure TCpu6502.OpANDimm; { opcode $29 }
begin
  AluAND(LoadImm);
end;

procedure TCpu6502.OpANDabs; { opcode $2D }
begin
  AluAND(LoadAbs);
end;

procedure TCpu6502.OpBMI;    { opcode $30 - branch on MInus }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagN then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpANDindY; { opcode $31 }
begin
  AluAND(LoadIndY);
end;

procedure TCpu6502.OpANDzpX; { opcode $35 }
begin
  AluAND(LoadZpX);
end;

procedure TCpu6502.OpSEC;    { opcode $38 - set carry flag }
begin
  FlagC := true;
end;

procedure TCpu6502.OpANDabsY; { opcode $39 }
begin
  AluAND(LoadAbsY);
end;

procedure TCpu6502.OpANDabsX; { opcode $3D }
begin
  AluAND(LoadAbsX);
end;

procedure TCpu6502.OpJMPabs; { opcode $4C - jump absolute }
begin
  PC := LoadWordIncPC;
end;

procedure TCpu6502.OpBVC;    { opcode $50 - branch if overflow clear }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagV then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpCLI;    { opcode $58 - clear interrupt enable flag  }
begin
  FlagI := false;
end;

procedure TCpu6502.OpADCindX; { opcode $61 }
var
  m: byte;
begin
  m := LoadIndX;
  AluADC(m);
end;

procedure TCpu6502.OpADCzp; { opcode $65 }
var
  m: byte;
begin
  m := LoadZP;
  AluADC(m);
end;

procedure TCpu6502.OpADCimm; { opcode $69 - add immediate to accumulator with carry }
var
  m: byte;
begin
  m := LoadImm;
  AluADC(m);
end;

procedure TCpu6502.OpADCabs; { opcode $6D }
var
  m: byte;
begin
  m := LoadAbs;
  AluADC(m);
end;

procedure TCpu6502.OpBVS;    { opcode $70 - branch if overflow set }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagV then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpADCindY; { opcode $71 }
var
  m: byte;
begin
  m := LoadIndY;
  AluADC(m);
end;

procedure TCpu6502.OpADCzpX; { opcode $75 }
var
  m: byte;
begin
  m := LoadZpX;
  AluADC(m);
end;

procedure TCpu6502.OpSEI;    { opcode $78 - set interrupt enable flag  }
begin
  FlagI := true;
end;

procedure TCpu6502.OpADCabsY; { opcode $79 }
var
  m: byte;
begin
  m := LoadAbsY;
  AluADC(m);
end;

procedure TCpu6502.OpADCabsX; { opcode $7D }
var
  m: byte;
begin
  m := LoadAbsX;
  AluADC(m);
end;

procedure TCpu6502.OpSTAindX; { opcode $81 }
begin
  StoreIndX(A);
end;

procedure TCpu6502.OpSTYzp; { opcode $84 }
begin
  StoreZP(Y);
end;

procedure TCpu6502.OpSTAzp; { opcode $85 }
begin
  StoreZP(A);
end;

procedure TCpu6502.OpSTXzp; { opcode $86 }
begin
  StoreZP(X);
end;

procedure TCpu6502.OpDEY;    { opcode $88 }
begin
  dec(Y);
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpTXA;    { opcode $8A }
begin
  A := X;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpSTYabs; { opcode $8C }
begin
  StoreAbs(Y);
end;

procedure TCpu6502.OpSTAabs; { opcode $8D }
begin
  StoreAbs(A);
end;

procedure TCpu6502.OpSTXabs; { opcode $8E }
begin
  StoreAbs(X);
end;

procedure TCpu6502.OpBCC;    { opcode $90 - branch if carry clear }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagC then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpSTAindY; { opcode $91 }
begin
  StoreIndY(A);
end;

procedure TCpu6502.OpSTYzpX; { opcode $94 }
begin
  StoreZpX(Y);
end;

procedure TCpu6502.OpSTAzpX; { opcode $95 }
begin
  StoreZpX(A);
end;

procedure TCpu6502.OpSTXzpY; { opcode $96 }
begin
  StoreZpY(X);
end;

procedure TCpu6502.OpTYA;    { opcode $98 }
begin
  A := Y;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpSTAabsY; { opcode $99 }
begin
  StoreAbsY(A);
end;

procedure TCpu6502.OpSTAabsX; { opcode $9D }
begin
  StoreAbsX(A);
end;

procedure TCpu6502.OpLDYimm; { opcode $A0 }
begin
  Y := LoadByteIncPC;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAindX; { opcode $A1 }
begin
  A := LoadIndX;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDXimm; { opcode $A2 }
begin
  X := LoadByteIncPC;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpLDYzp; { opcode $A4 }
begin
  Y := LoadZp;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAzp; { opcode $A5 }
begin
  A := LoadZp;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDXzp; { opcode $A6 }
begin
  X := LoadZp;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpTAY;    { opcode $A8 }
begin
  Y := A;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAimm; { opcode $A9 - load accumulator with immediate }
begin
  A := LoadByteIncPC;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpTAX;    { opcode $AA }
begin
  X := A;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpLDYabs; { opcode $AC }
begin
  Y := LoadAbs;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAabs; { opcode $AD }
begin
  A := LoadAbs;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDXabs; { opcode $AE }
begin
  X := LoadAbs;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpBCS;    { opcode $B0 - branch if carry set }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagC then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpLDAindY; { opcode $B1 }
begin
  A := LoadIndY;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDYzpX; { opcode $B4 }
begin
  Y := LoadZpX;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAzpX; { opcode $B5 }
begin
  A := LoadZpX;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDXzpY; { opcode $B6 }
begin
  X := LoadZpY;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpCLV;    { opcode $B8 - clear overflow flag }
begin
  FlagV := false;
end;

procedure TCpu6502.OpLDAabsY; { opcode $B9 }
begin
  A := LoadAbsY;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDYabsX; { opcode $BC }
begin
  Y := LoadAbsX;
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpLDAabsX; { opcode $BD }
begin
  A := LoadAbsX;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpLDXabsY; { opcode $BE }
begin
  X := LoadAbsY;
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpCMPindX; { opcode $C1 }
begin
  AluCMP(LoadIndX);
end;

procedure TCpu6502.OpCMPzp; { opcode $C5 }
begin
  AluCMP(LoadZp);
end;

procedure TCpu6502.OpINY;    { opcode $C8 }
begin
  inc(Y);
  AluUpdateNZ(Y);
end;

procedure TCpu6502.OpCMPimm; { opcode $C9 - compare accumulator with immediate }
begin
  AluCMP(LoadImm);
end;

procedure TCpu6502.OpDEX;    { opcode $CA }
begin
  dec(X);
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpCMPabs; { opcode $CD - compare accumulator with abs }
begin
  AluCMP(LoadAbs);
end;

procedure TCpu6502.OpBNE;    { opcode $D0 - branch if not equal }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagZ then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpCMPindY; { opcode $D1 }
begin
  AluCMP(LoadIndY);
end;

procedure TCpu6502.OpCMPzpX; { opcode $D5 }
begin
  AluCMP(LoadZpX);
end;

procedure TCpu6502.OpCLD;    { opcode $D8 - clear decimal flag }
begin
  FlagD := false;
end;

procedure TCpu6502.OpCMPabsY; { opcode $D9 }
begin
  AluCMP(LoadAbsY);
end;

procedure TCpu6502.OpCMPabsX; { opcode $DD }
begin
  AluCMP(LoadAbsX);
end;

procedure TCpu6502.OpINX;    { opcode $E8 }
begin
  inc(X);
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpNOP;    { opcode $EA - no operation }
begin
end;

procedure TCpu6502.OpBEQ;    { opcode $D0 - branch if equal }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagZ then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpSED;    { opcode $F8 - set decimal flag }
begin
  FlagD := true;
end;


procedure TCpu6502.InitOpTbl;
var
  i: integer;
begin
  for i := low(OpTbl) to high(OpTbl) do begin
    OpTbl[i] := @OpNOP;
  end;

  OpTbl[$06] := @OpASLzp;
  OpTbl[$0A] := @OpASL;
  OpTbl[$0E] := @OpASLabs;
  OpTbl[$10] := @OpBPL;      { branch if plus }
  OpTbl[$16] := @OpASLzpX;
  OpTbl[$18] := @OpCLC;      { clear carry flag }
  OpTbl[$1E] := @OpASLabsX;
  OpTbl[$21] := @OpANDindX;
  OpTbl[$25] := @OpANDzp;
  OpTbl[$29] := @OpANDimm;
  OpTbl[$2D] := @OpANDabs;
  OpTbl[$30] := @OpBMI;      { branch if minus }
  OpTbl[$31] := @OpANDindY;
  OpTbl[$35] := @OpANDzpX;
  OpTbl[$38] := @OpSEC;
  OpTbl[$39] := @OpANDabsY;
  OpTbl[$3D] := @OpANDabsX;
  OpTbl[$4C] := @OpJMPabs;   { jump absolute }
  OpTbl[$50] := @OpBVC;      { branch if overflow clear }
  OpTbl[$58] := @OpCLI;      { clear interrupt flag }
  OpTbl[$61] := @OpADCindX;
  OpTbl[$65] := @OpADCzp;
  OpTbl[$69] := @OpADCimm;   { add immediate to accumulator with carry }
  OpTbl[$6D] := @OpADCabs;
  OpTbl[$70] := @OpBVS;      { branch if overflow set }
  OpTbl[$71] := @OpADCindY;
  OpTbl[$75] := @OpADCzpX;
  OpTbl[$78] := @OpSEI;      { set interrupt flag }
  OpTbl[$79] := @OpADCabsY;
  OpTbl[$7D] := @OpADCabsX;
  OpTbl[$81] := @OpSTAindX;
  OpTbl[$84] := @OpSTYzp;
  OpTbl[$85] := @OpSTAzp;
  OpTbl[$86] := @OpSTXzp;
  OpTbl[$88] := @OpDEY;
  OpTbl[$8A] := @OpTXA;
  OpTbl[$8C] := @OpSTYabs;
  OpTbl[$8D] := @OpSTAabs;
  OpTbl[$8E] := @OpSTXabs;
  OpTbl[$90] := @OpBCC;      { branch if carry clear }
  OpTbl[$91] := @OpSTAindY;
  OpTbl[$94] := @OpSTYzpX;
  OpTbl[$95] := @OpSTAzpX;
  OpTbl[$96] := @OpSTXzpY;
  OpTbl[$98] := @OpTYA;
  OpTbl[$99] := @OpSTAabsY;
  OpTbl[$9D] := @OpSTAabsX;
  OpTbl[$A0] := @OpLDYimm;
  OpTbl[$A1] := @OpLDAindX;
  OpTbl[$A2] := @OpLDXimm;
  OpTbl[$A4] := @OpLDYzp;
  OpTbl[$A5] := @OpLDAzp;
  OpTbl[$A6] := @OpLDXzp;
  OpTbl[$A8] := @OpTAY;
  OpTbl[$A9] := @OpLDAimm;   { load accumulator with immediate }
  OpTBL[$AA] := @OpTAX;
  OpTbl[$AC] := @OpLDYabs;
  OpTbl[$AD] := @OpLDAabs;
  OpTbl[$AE] := @OpLDXabs;
  OpTbl[$B0] := @OpBCS;      { branch if carry set }
  OpTbl[$B1] := @OpLDAindY;
  OpTbl[$B4] := @OpLDYzpX;
  OpTbl[$B5] := @OpLDAzpX;
  OpTbl[$B6] := @OpLDXzpY;
  OpTbl[$B8] := @OpCLV;      { clear overflow flag }
  OpTbl[$B9] := @OpLDAabsY;
  OpTbl[$BC] := @OpLDYabsX;
  OpTbl[$BD] := @OpLDAabsX;
  OpTbl[$BE] := @OpLDXabsY;
  OpTbl[$C1] := @OpCMPindX;
  OpTbl[$C5] := @OpCMPzp;
  OpTbl[$C8] := @OpINY;
  OpTbl[$C9] := @OpCMPimm;
  OpTbl[$CA] := @OpDEX;
  OpTbl[$CD] := @OpCMPabs;
  OpTbl[$D0] := @OpBNE;      { branch if not equal }
  OpTbl[$D1] := @OpCMPindY;
  OpTbl[$D5] := @OpCMPzpX;
  OpTbl[$D8] := @OpCLD;      { clear decimal flag }
  OpTbl[$D9] := @OpCMPabsY;
  OpTbl[$DD] := @OpCMPabsX;
  OpTbl[$E8] := @OpINX;
  OpTbl[$EA] := @OpNOP;      { no operation }
  OpTbl[$F0] := @OpBEQ;      { branch if equal }
  OpTbl[$F8] := @OpSED;	     { set decimal flag }
end;

procedure TCpu6502.DumpRegs;
begin
  Write(Format('PC=%4.4x  A=%2.2x X=%2.2x Y=%2.2x  ', [PC, A, X, Y]));
  if FlagN then Write('N') else Write('.');
  if FlagV then Write('V') else Write('.');
  if FlagB then Write('B') else Write('.');
  if FlagD then Write('D') else Write('.');
  if FlagI then Write('I') else Write('.');
  if FlagZ then Write('Z') else Write('.');
  if FlagC then Write('C') else Write('.');
  WriteLn;
end;

end.
