unit cpu6502;

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

  { fetch and execute next instruction }
  procedure ExecuteNext;
  { fetch and execute next instruction end dexplay cpu status }
  procedure ExecuteToWithDump(pcBreak: word);

  { fetch next byte from PC address and increment PC by 1 }
  function LoadByteIncPC : byte; inline;
  { fetch next word from PC address and increment PC by 2 }
  function LoadWordIncPC : word; inline;
  function LoadZPIncPC : byte; inline;
  procedure StoreZPIncPC(m: byte); inline;
  { add a signed byte to current PC }
  procedure PCAddByteSigned(const addr: byte);

  { addressing functions }
  { }
  function LoadImm : byte; inline;
  function LoadAbs : byte; inline;

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
  procedure OpASL;    //< opcode $0A - arithmetic shift left accumulator
  procedure OpBPL;    //< opcode $10 - branch on PLus
  procedure OpCLC;    //< opcode $18 - clear carry
  procedure OpBMI;    //< opcode $30 - branch on MInus
  procedure OpSEC;    //< opcode $38 - set carry flag
  procedure OpJMPabs; //< opcode $4C - jump absolute
  procedure OpBVC;    //< opcode $50 - branch if overflow clear
  procedure OpCLI;    //< opcode $58 - clear interrupt enable flag
  procedure OpADCimm; //< opcode $69 - add imm to accumulator with carry
  procedure OpBVS;    //< opcode $70 - branch if overflow set
  procedure OpSEI;    //< opcode $78 - set interrupt enable flag
  procedure OpSTAzp;  //< opcode $85 - store accumulator at zp
  procedure OpDEY;    //< opcode $88 - decrement y
  procedure OpTXA;    //< opcode $8A - transfer X to A
  procedure OpBCC;    //< opcode $90 - branch if carry clear
  procedure OpTYA;    //< opcode $98 - transfer Y to A
  procedure OpTAY;    //< opcode $A8 - transfer A to Y
  procedure OpLDAimm; //< opcode $A9 - load accumulator with immediate
  procedure OpTAX;    //< opcode $AA - transfer A to X
  procedure OpBCS;    //< opcode $B0 - branch if carry set
  procedure OpCLV;    //< opcode $B8 - clear overflow flag
  procedure OpINY;    //< opcode $C8 - increment Y
  procedure OpCMPimm; //< opcode $C9 - compare accumulator with immediate
  procedure OpDEX;    //< opcode $CA - decrement X
  procedure OpCMPabs; //< opcode $CD - compare accumulator with abs
  procedure OpBNE;    //< opcode $D0 - branch if not equal
  procedure OpCLD;    //< opcode $D8 - clear decimal flag
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


{--- high level execution routines  ------------------------------------------}

procedure TCpu6502.ExecuteNext;
var
  opcode: byte;
begin
  opcode := LoadByteIncPC;
  OpTbl[opcode]();
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

{ load word from PC abs, PC <- PC + 2 }
function TCpu6502.LoadWordIncPC : word;
var
  tmp: word;
begin
  tmp := LoadByteIncPC;
  tmp := tmp or (word(LoadByteIncPC()) shl 8);
  LoadWordIncPC := tmp;
end;

function TCpu6502.LoadZPIncPC : byte;
var
  addr: word;
begin
  addr := LoadByteIncPC;
  LoadZPIncPC := LoadByte(addr);
end;

procedure TCpu6502.StoreZPIncPC(m: byte); inline;
var
  addr: word;
begin
  addr := LoadByteIncPC;
  StoreByte(addr, m);
end;

function TCpu6502.LoadImm : byte;
begin
  LoadImm := LoadByte(PC);
  inc(PC);
end;

function TCpu6502.LoadAbs : byte;
var
  addr: word;
begin
  addr := LoadWordIncPC;
  LoadAbs := LoadByte(addr);
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

procedure TCpu6502.OpASL; { opcode $0A }
begin
  A := AluASL(A);
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

procedure TCpu6502.OpBMI;    { opcode $30 - branch on MInus }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagN then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpSEC;    { opcode $38 - set carry flag }
begin
  FlagC := true;
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

procedure TCpu6502.OpADCimm; { opcode $69 - add immediate to accumulator with carry }
var
  m: byte;
begin
  m := LoadByteIncPC;
  AluADC(m);
end;

procedure TCpu6502.OpBVS;    { opcode $70 - branch if overflow set }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagV then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpSEI;    { opcode $78 - set interrupt enable flag  }
begin
  FlagI := true;
end;

procedure TCpu6502.OpSTAzp;
begin
  StoreZPIncPC(A);
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

procedure TCpu6502.OpBCC;    { opcode $90 - branch if carry clear }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagC then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpTYA;    { opcode $98 }
begin
  A := Y;
  AluUpdateNZ(A);
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

procedure TCpu6502.OpBCS;    { opcode $B0 - branch if carry set }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagC then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpCLV;    { opcode $B8 - clear overflow flag }
begin
  FlagV := false;
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

procedure TCpu6502.OpCLD;    { opcode $D8 - clear decimal flag }
begin
  FlagD := false;
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

  OpTbl[$0A] := @OpASL;
  OpTbl[$10] := @OpBPL;      { branch if plus }
  OpTbl[$18] := @OpCLC;      { clear carry flag }
  OpTbl[$30] := @OpBMI;      { branch if minus }
  OpTbl[$38] := @OpSEC;      { set carry flag }
  OpTbl[$4C] := @OpJMPabs;   { jump absolute }
  OpTbl[$50] := @OpBVC;      { branch if overflow clear }
  OpTbl[$58] := @OpCLI;      { clear interrupt flag }
  OpTbl[$69] := @OpADCimm;   { add immediate to accumulator with carry }
  OpTbl[$70] := @OpBVS;      { branch if overflow set }
  OpTbl[$78] := @OpSEI;      { set interrupt flag }
  OpTbl[$85] := @OpSTAzp;
  OpTbl[$88] := @OpDEY;
  OpTbl[$8A] := @OpTXA;
  OpTbl[$90] := @OpBCC;      { branch if carry clear }
  OpTbl[$98] := @OpTYA;
  OpTbl[$A8] := @OpTAY;
  OpTbl[$A9] := @OpLDAimm;   { load accumulator with immediate }
  OpTBL[$AA] := @OpTAX;
  OpTbl[$B0] := @OpBCS;      { branch if carry set }
  OpTbl[$B8] := @OpCLV;      { clear overflow flag }
  OpTbl[$C8] := @OpINY;
  OpTbl[$C9] := @OpCMPimm;
  OpTbl[$CA] := @OpDEX;
  OpTbl[$CD] := @OpCMPabs;
  OpTbl[$D0] := @OpBNE;      { branch if not equal }
  OpTbl[$D8] := @OpCLD;      { clear decimal flag }
  OpTbl[$E8] := @OpINX;
  OpTbl[$EA] := @OpNOP;      { no operation }
  OpTbl[$F0] := @OpBEQ;      { branch if equal }
  OpTbl[$F8] := @OpSED;	     { set decimal flag }
end;

procedure TCpu6502.DumpRegs;
begin
  Write(Format('PC=%4.4x[%2.2x %2.2x %2.2x]  A=%2.2x X=%2.2x Y=%2.2x  ', [PC, LoadByte(PC), LoadByte(PC+1), LoadByte(PC+2), A, X, Y]));
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
