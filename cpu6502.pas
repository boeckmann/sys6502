unit cpu6502;

interface

type

TLoadFunc = function(const addr: word) : byte;
TStoreProc = procedure(const addr: word; const m: byte);

TCpu6502 = object
  PC: word;

  A: byte;
  X: byte;
  Y: byte;

  FlagC, FlagZ, FlagI, FlagD, FlagB, FlagV, FlagN : boolean;

  LoadByte: TLoadFunc;
  StoreByte: TStoreProc;

  OpTbl: array[0..255] of procedure of object;

  procedure ExecuteNext;
  procedure ExecuteToWithDump(pcBreak: word);

  function LoadIncPC : byte; inline;
  procedure PCAddRel(const addr: byte);

  procedure AluADC(const m: byte); inline;
  procedure AluSBC(const m: byte); inline;
  procedure AluAND(const m: byte); inline;
  procedure AluORA(const m: byte); inline;
  procedure AluEOR(const m: byte); inline;
  procedure AluUpdateFlags(const op1: byte; const op2: byte; const res: word); inline;
  procedure AluUpdateNZ(const op: byte); inline;

  procedure InitOpTbl;
  procedure OpBPL;    { opcode $10 - branch on PLus }
  procedure OpCLC;    { opcode $18 - clear carry }
  procedure OpBMI;    { opcode $30 - branch on MInus }
  procedure OpSEC;    { opcode $38 - set carry flag }
  procedure OpJMPabs; { opcode $4C - jump absolute }
  procedure OpBVC;    { opcode $50 - branch if overflow clear }
  procedure OpCLI;    { opcode $58 - clear interrupt enable flag  }
  procedure OpADCimm; { opcode $69 - add imm to accumulator with carry }
  procedure OpBVS;    { opcode $70 - branch if overflow set }
  procedure OpSEI;    { opcode $78 - set interrupt enable flag }
  procedure OpBCC;    { opcode $90 - branch if carry clear }
  procedure OpLDAimm; { opcode $A9 - load accumulator with immediate }
  procedure OpBCS;    { opcode $B0 - branch if carry set }
  procedure OpCLV;    { opcode $B8 - clear overflow flag }
  procedure OpBNE;    { opcode $D0 - branch if not equal }
  procedure OpCLD;    { opcode $D8 - clear decimal flag }
  procedure OpNOP;    { opcode $EA - no operation }
  procedure OpBEQ;    { opcode $F0 - branch if equal }
  procedure OpSED;    { opcode $F8 - set decimal flag }

  procedure DumpRegs;
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
  opcode := LoadIncPC;
  OpTbl[opcode]();
end;

procedure TCpu6502.ExecuteToWithDump(pcBreak: word);
var
  opcode: byte;
begin
  while PC <> pcBreak do begin
    opcode := LoadIncPC;
    OpTbl[opcode]();
    DumpRegs;
  end;
end;

{--- PC and memory access functions -------------------------------------------}

{ load memory from PC abs, PC <- PC + 1 }
function TCpu6502.LoadIncPC : byte;
begin
  LoadIncPC := LoadByte(PC);
  inc(PC);
end;

{ add signed byte to PC }
procedure TCpu6502.PCAddRel(const addr: byte);
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

procedure TCpu6502.AluEOR(const m: byte); inline;
begin
  A := A xor m;
  AluUpdateNZ(A);
end;

procedure TCpu6502.AluUpdateFlags(const op1: byte; const op2: byte; const res: word); inline;
begin
  FlagC := ((res and BIT_8) <> 0);
  FlagZ := ((res and $ff) = 0);
  FlagN := ((res and BIT_7) <> 0);
  FlagV := (((op1 and BIT_7) = (op2 and BIT_7))) and ((op1 and BIT_7) <> (res and BIT_7));
end;

procedure TCpu6502.AluUpdateNZ(const op: byte); inline;
begin
  FlagZ := ((op and $ff) = 0);
  FlagN := ((op and BIT_7) <> 0);
end;


{--- opcode implementation ---------------------------------------------------}

procedure TCpu6502.OpBPL;    { opcode $10 - branch on PLus }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if not FlagN then PCAddRel(rel);
end;

procedure TCpu6502.OpCLC;    { opcode $18 - clear carry flag }
begin
  FlagC := false;
end;

procedure TCpu6502.OpBMI;    { opcode $30 - branch on MInus }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if FlagN then PCAddRel(rel);
end;

procedure TCpu6502.OpSEC;    { opcode $38 - set carry flag }
begin
  FlagC := true;
end;

procedure TCpu6502.OpJMPabs; { opcode $4C - jump absolute }
var
  tmpPC: word;
begin
  tmpPC := LoadIncPC;
  tmpPC := tmpPC or (word(LoadIncPC()) shl 8);
  PC := tmpPC;
end;

procedure TCpu6502.OpBVC;    { opcode $50 - branch if overflow clear }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if not FlagV then PCAddRel(rel);
end;

procedure TCpu6502.OpCLI;    { opcode $58 - clear interrupt enable flag  }
begin
  FlagI := false;
end;

procedure TCpu6502.OpADCimm; { opcode $69 - add immediate to accumulator with carry }
var
  m: byte;
begin
  m := LoadIncPC;
  AluADC(m);
end;

procedure TCpu6502.OpBVS;    { opcode $70 - branch if overflow set }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if FlagV then PCAddRel(rel);
end;

procedure TCpu6502.OpSEI;    { opcode $78 - set interrupt enable flag  }
begin
  FlagI := true;
end;

procedure TCpu6502.OpBCC;    { opcode $90 - branch if carry clear }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if not FlagC then PCAddRel(rel);
end;

procedure TCpu6502.OpLDAimm; { opcode $A9 - load accumulator with immediate }
begin
  A := LoadIncPC;
  AluUpdateNZ(A);
end;

procedure TCpu6502.OpBCS;    { opcode $B0 - branch if carry set }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if FlagC then PCAddRel(rel);
end;

procedure TCpu6502.OpCLV;    { opcode $B8 - clear overflow flag }
begin
  FlagV := false;
end;

procedure TCpu6502.OpBNE;    { opcode $D0 - branch if not equal }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if not FlagZ then PCAddRel(rel);
end;

procedure TCpu6502.OpCLD;    { opcode $D8 - clear decimal flag }
begin
  FlagD := false;
end;

procedure TCpu6502.OpNOP;    { opcode $EA - no operation }
begin
end;

procedure TCpu6502.OpBEQ;    { opcode $D0 - branch if equal }
var
  rel: byte;
begin
  rel := LoadIncPC;
  if FlagZ then PCAddRel(rel);
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
  OpTbl[$90] := @OpBCC;      { branch if carry clear }
  OpTbl[$A9] := @OpLDAimm;   { load accumulator with immediate }
  OpTbl[$B0] := @OpBCS;      { branch if carry set }
  OpTbl[$B8] := @OpCLV;      { clear overflow flag }
  OpTbl[$D0] := @OpBNE;      { branch if not equal }
  OpTbl[$D8] := @OpCLD;      { clear decimal flag }
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
