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

  function LoadIncPC : byte; inline;

  procedure AluADC(const m: byte); inline;
  procedure AluSBC(const m: byte); inline;
  procedure AluAND(const m: byte); inline;
  procedure AluORA(const m: byte); inline;
  procedure AluEOR(const m: byte); inline;
  procedure AluUpdateFlags(const op1: byte; const op2: byte; const res: word); inline;
  procedure AluUpdateNZ(const op: byte); inline;

  procedure InitOpTbl;
  procedure OpSEC;
  procedure OpCLC;
  procedure OpSEI;
  procedure OpCLI;
  procedure OpSED;
  procedure OpCLD;
  procedure OpCLV;

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


{--- memory access functions -------------------------------------------------}

function TCpu6502.LoadIncPC : byte;
begin
  LoadIncPC := LoadByte(PC);
  inc(PC);
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

procedure TCpu6502.OpSEC;
begin
  FlagC := true;
end;

procedure TCpu6502.OpCLC;
begin
  FlagC := false;
end;

procedure TCpu6502.OpSEI;
begin
  FlagI := true;
end;

procedure TCpu6502.OpCLI;
begin
  FlagI := false;
end;

procedure TCpu6502.OpSED;
begin
  FlagD := true;
end;

procedure TCpu6502.OpCLD;
begin
  FlagD := false;
end;

procedure TCpu6502.OpCLV;
begin
  FlagV := false;
end;

procedure TCpu6502.InitOpTbl;
begin
  OpTbl[$18] := @OpCLC;  { clear carry flag }
  OpTbl[$38] := @OpSEC;  { set carry flag }
  OpTbl[$58] := @OpCLI;  { clear interrupt flag }
  OpTbl[$78] := @OpSEI;  { set interrupt flag }
  OpTbl[$B8] := @OpCLV;  { clear overflow flag }
  OpTbl[$D8] := @OpCLD;  { clear decimal flag }
  OpTbl[$F8] := @OpSED;	{ set decimal flag }
end;

procedure TCpu6502.DumpRegs;
begin
  Write(Format('A=%2.2x X=%2.2x Y=%2.2x  ', [A, X, Y]));
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
