{ SYS6502 - A MOS Technology MCS6502 emulator
  Copyright (C) 2020-2021 Bernd Böckmann

  Distributed under MIT license. Please see LICENSE file.
}

unit cpu6502;

{$MODE objfpc}{$H+}{$J-}

{$OVERFLOWCHECKS off}   // some code depends on value wrap around

interface

type

PCpu6502 = ^TCpu6502;

TLoadFunc = function(const addr: word) : byte;
TStoreProc = procedure(const addr: word; const m: byte);
TBuiltinProc = procedure(cpu: PCpu6502);

TCpu6502 = object
  { cpu registers }
  { }
  PC: word;		//< 16-bit program counter PC

  A: byte;		//< 8-bit accumulator register A
  X: byte;		//< 8-bit register X
  Y: byte;		//< 8-bit register Y
  S: byte;              //< 8-bit stack register

  FlagC: boolean;	//< carry flag
  FlagZ: boolean;	//< zero flag
  FlagI: boolean;	//< interrupt disable flag
  FlagD: boolean;	//< decimal flag
  FlagV: boolean;	//< overflow flag
  FlagN: boolean;	//< negative (sign) flag

  { function pointers to external memory interface }
  { }
  LoadByte: TLoadFunc;		//< load byte from external memory
  StoreByte: TStoreProc;	//< store byte to external memory

  { opcode function dispatch table }
  OpTbl: array[0..$FF] of procedure of object;
  
  { function call table - determines what happens if CPU executes mem[PC]   }
  {   if FuncTbl[PC] is nil simulator continues executing 6502 instructions }
  {   if FuncTbl[PC] <> nil then the given function is called               }
  FuncTbl: array[0..$FFFF] of TBuiltinProc;

  constructor Init(const load: TLoadFunc; const store: TStoreProc);
  procedure ResetCPU;
  procedure InstallBuiltinProc(const addr: word; func: TBuiltinProc);

  
  { fetch and execute next instruction }
  procedure ExecuteNext;
  { fetch and execute next instruction end dexplay cpu status }
  procedure ExecuteTo(pcBreak: word);
  procedure ExecuteToWithDump(pcBreak: word);
  
  { flag functions }
  { }
  function FlagsToByte : byte;
  procedure ByteToFlags(const b: byte);


  { fetch next byte from PC address and increment PC by 1 }
  function LoadByteIncPC : byte;
  { fetch next word from PC address and increment PC by 2 }
  function LoadWord(const addr: word) : word;
  function LoadWordIncPC : word;

  { add a signed byte to current PC }
  procedure PCAddByteSigned(const addr: byte);

  { addressing functions }
  { }
  function LoadImm : byte;
  function LoadZp : byte;
  function LoadZpWithAddr(out addr: word) : byte;
  procedure StoreZp(m: byte);
  function LoadZpX : byte;
  function LoadZpXWithAddr(out addr: word) : byte;
  procedure StoreZpX(const m: byte);
  function LoadZpY : byte;
  procedure StoreZpY(const m: byte);
  function LoadAbs : byte;
  function LoadAbsWithAddr(out addr: word) : byte;
  procedure StoreAbs(const m: byte);
  function LoadAbsX : byte;
  function LoadAbsXWithAddr(out addr: word) : byte;
  procedure StoreAbsX(const m: byte);
  function LoadAbsY : byte;
  procedure StoreAbsY(const m: byte);
  function LoadIndX : byte;
  procedure StoreIndX(const m: byte);
  function LoadIndY : byte;
  procedure StoreIndY(const m: byte);

  { ALU routines }
  { }
  procedure AluADC(const m: byte);
  procedure AluSBC(const m: byte);
  procedure AluAND(const m: byte);
  procedure AluORA(const m: byte);
  procedure AluEOR(const m: byte);
  procedure AluCMP(const m: byte);
  procedure AluCPX(const m: byte);
  procedure AluCPY(const m: byte);
  function AluINC(const m: byte) : byte;
  function AluDEC(const m: byte) : byte;
  function AluASL(const m: byte) : byte;
  function AluLSR(const m: byte) : byte;
  function AluROL(const m: byte) : byte;
  function AluROR(const m: byte) : byte;

  procedure AluUpdateFlags(const op1: byte; const op2: byte; const res: word);
  procedure AluUpdateNZ(const op: byte);
  procedure AluUpdateNZC(const op: byte);

  { stack routines }
  { }
  procedure PushStack(v: byte);
  function PullStack : byte;

  procedure InitFuncTbl;

  { opcode execution routines }
  { }
  procedure InitOpTbl;
  procedure OpBRK;    //< opcode $00 - break
  procedure OpORAindX;//< opcode $01 - bitwise or A with (ind,X)
  procedure OpORAzp;  //< opcode $05 - bitwise or A with zp
  procedure OpASLzp;  //< opcode $06 - arithmetic shift left zp
  procedure OpPHP;    //< opcode $08 - push flags
  procedure OpORAimm; //< opcode $09 - bitwise or A with imm
  procedure OpASL;    //< opcode $0A - arithmetic shift left accumulator
  procedure OpORAabs; //< opcode $0D - bitwise or A with abs
  procedure OpASLabs; //< opcode $0E - arithmetic shift left abs
  procedure OpBPL;    //< opcode $10 - branch on PLus
  procedure OpORAindY;//< opcode $11 - bitwise or A with (ind),Y
  procedure OpORAzpX; //< opcode $15 - bitwise or A with zp,X
  procedure OpASLzpX; //< opcode $16 - arithmetic shift left zp,X
  procedure OpCLC;    //< opcode $18 - clear carry
  procedure OpORAabsY;//< opcode $19 - bitwise or A with abs,Y
  procedure OpORAabsX;//< opcode $1D - bitwise or A with abs,X
  procedure OpASLabsX;//< opcode $1E - arithmetic shift left abs,X
  procedure OpJSR;    //< opcode $20 - jump service routine
  procedure OpANDindX;//< opcode $21 - and A with (ind,X)
  procedure OpBITzp;  //< opcode $24 - test zp bits ans set flags
  procedure OpANDzp;  //< opcode $25 - and A with zp
  procedure OpROLzp;  //< opcode $26 - rotate right zp
  procedure OpPLP;    //< opcode $28 - pull flags
  procedure OpANDimm; //< opcode $29 - and A with imm
  procedure OpROL;    //< opcode $2A - rotate right A
  procedure OpBITabs; //< opcode $2C - test abs bits ans set flags
  procedure OpANDabs; //< opcode $2D - and A with abs
  procedure OpROLabs; //< opcode $2E - rotate right abs
  procedure OpBMI;    //< opcode $30 - branch on MInus
  procedure OpANDindY;//< opcode $31 - and A with (ind),Y
  procedure OpANDzpX; //< opcode $35 - and A with zp,X
  procedure OpROLzpX; //< opcode $36 - rotate right zp,X
  procedure OpSEC;    //< opcode $38 - set carry flag
  procedure OpANDabsY;//< opcode $39 - and A with abs,Y
  procedure OpANDabsX;//< opcode $3D - and A with abs,X
  procedure OpROLabsX;//< opcode $3E - rotate right abs,X
  procedure OpRTI;    //< opcode $40 - return from interrupt
  procedure OpEORindX;//< opcode $41 - eor A with (ind,X)
  procedure OpEORzp;  //< opcode $45 - eor A with zp
  procedure OpLSRzp;  //< opcode $46 - logical shift right zp
  procedure OpPHA;    //< opcode $48 - push A
  procedure OpEORimm; //< opcode $49 - eor A with imm
  procedure OpLSR;    //< opcode $4A - logical shift right A
  procedure OpJMPabs; //< opcode $4C - jump absolute
  procedure OpEORabs; //< opcode $4D - eor A with abs
  procedure OpLSRabs; //< opcode $4E - logical shift right abs
  procedure OpBVC;    //< opcode $50 - branch if overflow clear
  procedure OpEORindY;//< opcode $51 - eor A with (ind),Y
  procedure OpEORzpX; //< opcode $55 - eor A with zp,X
  procedure OpLSRzpX; //< opcode $56 - logical shift right zp,X
  procedure OpCLI;    //< opcode $58 - clear interrupt enable flag
  procedure OpEORabsY;//< opcode $59 - eor A with abs,Y
  procedure OpEORabsX;//< opcode $5D - eor A with abs,X
  procedure OpLSRabsX;//< opcode $5E - logical shift right abs,X
  procedure OpRTS;    //< opcode $60 - return from subroutine
  procedure OpADCindX;//< opcode $61 - add (ind,X) to accumulator with carry
  procedure OpADCzp;  //< opcode $65 - add zp to accumulator with carry
  procedure OpRORzp;  //< opcode $66 - rotate right zp
  procedure OpPLA;    //< opcode $68 - pull A
  procedure OpADCimm; //< opcode $69 - add imm to accumulator with carry
  procedure OpROR;    //< opcode $6A - rotate right A
  procedure OpJMPind; //< opcode $6C - jump indirect
  procedure OpADCabs; //< opcode $6D - add abs to accumulator with carry
  procedure OpRORabs; //< opcode $6E - rotate right abs
  procedure OpBVS;    //< opcode $70 - branch if overflow set
  procedure OpADCindY;//< opcode $71 - add (ind),Y to accumulator with carry
  procedure OpADCzpX; //< opcode $75 - add zp,X to accumulator with carry
  procedure OpRORzpX; //< opcode $76 - rotate right zp,X
  procedure OpSEI;    //< opcode $78 - set interrupt enable flag
  procedure OpADCabsY;//< opcode $79 - add abs,Y to accumulator with carry
  procedure OpADCabsX;//< opcode $7D - add abs,X to accumulator with carry
  procedure OpRORabsX;//< opcode $7E - rotate right abs
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
  procedure OpTXS;    //< opcode $9A - store X in S
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
  procedure OpTSX;    //< opcode $BA - store S in X
  procedure OpLDYabsX;//< opcode $BC - load Y with abs,X
  procedure OpLDAabsX;//< opcode $BD - load A with abs,X
  procedure OpLDXabsY;//< opcode $BE - load X with abs,Y
  procedure OpCPYimm; //< opcode $C0 - compare Y with imm
  procedure OpCMPindX;//< opcode $C1 - compare A with (ind,X)
  procedure OpCPYzp;  //< opcode $C4 - compare Y with zp
  procedure OpCMPzp;  //< opcode $C5 - compare A with zp
  procedure OpDECzp;  //< opcode $C6 - decrement zp
  procedure OpINY;    //< opcode $C8 - increment Y
  procedure OpCMPimm; //< opcode $C9 - compare accumulator with immediate
  procedure OpDEX;    //< opcode $CA - decrement X
  procedure OpCPYabs; //< opcode $CC - compare Y with abs
  procedure OpCMPabs; //< opcode $CD - compare A with abs
  procedure OpDECabs; //< opcode $CE - decrement abs
  procedure OpBNE;    //< opcode $D0 - branch if not equal
  procedure OpCMPindY;//< opcode $D1 - compare A with (ind),Y
  procedure OpCMPzpX; //< opcode $D5 - compare A with zp,X
  procedure OpDECzpX; //< opcode $D6 - decrement zp,X
  procedure OpCLD;    //< opcode $D8 - clear decimal flag
  procedure OpCMPabsY;//< opcode $D9 - compare A with abs,Y
  procedure OpCMPabsX;//< opcode $DD - compare A with abs,X
  procedure OpDECabsX;//< opcode $DE - decrement abs,X
  procedure OpCPXimm; //< opcode $E0 - compare X with imm
  procedure OpSBCindX;//< opcode $E1 - subtract (ind,X) from A with carry
  procedure OpCPXzp;  //< opcode $E4 - compare X with zp
  procedure OpSBCzp;  //< opcode $E5 - subtract zp from A with carry
  procedure OpINCzp;  //< opcode $E6 - increment zp
  procedure OpINX;    //< opcode $E8 - increment X
  procedure OpSBCimm; //< opcode $E9 - subtract imm from A with carry
  procedure OpNOP;    //< opcode $EA - no operation
  procedure OpCPXabs; //< opcode $EC - compare X with abs
  procedure OpSBCabs; //< opcode $ED - subtract abs from A with carry
  procedure OpINCabs; //< opcode $EE - increment abs
  procedure OpBEQ;    //< opcode $F0 - branch if equal
  procedure OpSBCindY;//< opcode $F1 - subtract (ind),Y from A with carry
  procedure OpSBCzpX; //< opcode $F5 - subtract zp,X from A with carry
  procedure OpINCzpX; //< opcode $F6 - increment zp,X
  procedure OpSED;    //< opcode $F8 - set decimal flag
  procedure OpSBCabsY;//< opcode $F9 - subtract abs,Y from A with carry
  procedure OpSBCabsX;//< opcode $FD - subtract abs,X from A with carry
  procedure OpINCabsX;//< opcode $FE - increment abs,X

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


function BtoD(const b: boolean; const v: byte) : byte;
begin
  if b then BtoD := v else BtoD := 0;
end;

function DtoB(const v: byte) : boolean;
begin
  DToB := v <> 0;
end;

constructor TCpu6502.Init(const load: TLoadFunc; const store: TStoreProc);
begin
  LoadByte := load;
  StoreByte := store;

  InitOpTbl;
  InitFuncTbl;
  ResetCPU;
end;

procedure TCpu6502.ResetCPU;
begin
  PC := LoadWord($FFFC);
  S  := $FD;

  FlagI := true;
end;

procedure TCpu6502.InstallBuiltinProc(const addr: word; func: TBuiltinProc);
begin
  FuncTbl[addr] := func;
end;


{--- high level execution routines  ------------------------------------------}

procedure TCpu6502.ExecuteNext;
var
  opcode: byte;
begin
  if FuncTbl[PC] = nil then begin
    opcode := LoadByteIncPC;
    OpTbl[opcode]();
  end else
    FuncTbl[PC](@self);
end;

procedure TCpu6502.ExecuteTo(pcBreak: word);
begin
  while PC <> pcBreak do begin
    ExecuteNext;
  end;
end;

procedure TCpu6502.ExecuteToWithDump(pcBreak: word);
begin
  while PC <> pcBreak do begin
    ExecuteNext;
    DumpRegs;
  end;
end;


{--- Flag functions -----------------------------------------------------------}

function TCpu6502.FlagsToByte : byte;
begin
  FlagsToByte := BtoD(FlagC, $01) or BtoD(FlagZ, $02) or BtoD(FlagI, $04) or
                 BtoD(FlagD, $08) or BtoD(FlagV, $40) or BtoD(FlagN, $80);
end;

procedure TCpu6502.ByteToFlags(const b: byte);
begin
  FlagC := DtoB(b and $01);
  FlagZ := DtoB(b and $02);
  FlagI := DtoB(b and $04);
  FlagD := DtoB(b and $08);
  FlagV := DtoB(b and $40);
  FlagN := DtoB(b and $80);
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
begin
  LoadWordIncPC := LoadWord(PC);
  PC := PC + 2;
end;

{ addressing modes implementation }

function TCpu6502.LoadImm : byte;
begin
  LoadImm := LoadByteIncPC;
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

procedure TCpu6502.StoreZp(m: byte);
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


{--- stack functions ---------------------------------------------------------}

procedure TCpu6502.PushStack(v: byte);
begin
  StoreByte($100 + S, v);
  dec(S);
end;

function TCpu6502.PullStack : byte;
begin
  inc(S);
  PullStack := LoadByte($100 + S);
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

procedure TCpu6502.AluSBC(const m: byte);
begin
  AluADC(m xor $ff);
end;

procedure TCpu6502.AluAND(const m: byte);
begin
  A := A and m;
  AluUpdateNZ(A);
end;

procedure TCpu6502.AluORA(const m: byte);
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

procedure TCpu6502.AluCPX(const m: byte);
var
  tmp: byte;
begin
  tmp := X - m;
  AluUpdateNZC(tmp);
end;

procedure TCpu6502.AluCPY(const m: byte);
var
  tmp: byte;
begin
  tmp := Y - m;
  AluUpdateNZC(tmp);
end;

function TCpu6502.AluINC(const m: byte) : byte;
begin
  AluINC := m + 1;
  AluUpdateNZ(AluINC);
end;

function TCpu6502.AluDEC(const m: byte) : byte;
begin
  AluDEC := m - 1;
  AluUpdateNZ(AluDEC);
end;

function TCpu6502.AluASL(const m: byte) : byte;
begin
  FlagC := (m and $80) <> 0;
  AluASL := m shl 1;
  AluUpdateNZ(AluASL);
end;

function TCpu6502.AluLSR(const m: byte) : byte;
begin
  FlagC := (m and $1) <> 0;
  AluLSR := m shr 1;
  AluUpdateNZ(AluLSR);
end;

function TCpu6502.AluROL(const m: byte) : byte;
var
  tmpC: boolean;
begin
  tmpC := FlagC;
  FlagC := (m and BIT_7) <> 0;
  AluROL := (m shl 1);
  if tmpC then AluROL := AluROL or BIT_0;
  AluUpdateNZ(AluROL);
end;

function TCpu6502.AluROR(const m: byte) : byte;
var
  tmpC: boolean;
begin
  tmpC := FlagC;
  FlagC := (m and BIT_0) <> 0;
  AluROR := (m shr 1);
  if tmpC then AluROR := AluROR or BIT_7;
  AluUpdateNZ(AluROR);
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

procedure TCpu6502.OpBRK; { opcode $00 }
begin
  PC := PC + 1;
  PushStack(PC shr 8);
  PushStack(PC and $FF);
  PushStack(FlagsToByte or BIT_4 or BIT_5);
  PC := LoadWord($FFFE);
end;

procedure TCpu6502.OpORAindX; { opcode $01 }
begin
  AluORA(LoadIndX);
end;

procedure TCpu6502.OpORAzp; { opcode $05 }
begin
  AluORA(LoadZp);
end;

procedure TCpu6502.OpASLzp; {opcode $06 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluASL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpPHP; { opcode $08 }
begin
  PushStack(FlagsToByte or BIT_4 or BIT_5);
end;

procedure TCpu6502.OpORAimm; { opcode $09 }
begin
  AluORA(LoadImm);
end;

procedure TCpu6502.OpASL; { opcode $0A }
begin
  A := AluASL(A);
end;

procedure TCpu6502.OpORAabs; { opcode $0D }
begin
  AluORA(LoadAbs);
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

procedure TCpu6502.OpBPL;    { opcode $10 - branch on PLus }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagN then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpORAindY; { opcode $11 }
begin
  AluORA(LoadIndY);
end;

procedure TCpu6502.OpORAzpX; { opcode $15 }
begin
  AluORA(LoadZpX);
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

procedure TCpu6502.OpCLC;    { opcode $18 - clear carry flag }
begin
  FlagC := false;
end;

procedure TCpu6502.OpORAabsY; { opcode $19 }
begin
  AluORA(LoadAbsY);
end;

procedure TCpu6502.OpORAabsX; { opcode $1D }
begin
  AluORA(LoadAbsX);
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

procedure TCpu6502.OpJSR; { opcode $20 }
var
  addr: word;
begin
  addr := LoadWord(PC);
  PC := PC + 1;
  PushStack(PC shr 8);
  PushStack(PC and $FF);
  PC := addr;
end;

procedure TCpu6502.OpANDindX; { opcode $21 }
begin
  AluAND(LoadIndX);
end;

procedure TCpu6502.OpBITzp; { opcode $24 }
var
  m: byte;
begin
  m := LoadZp;
  FlagZ := m = 0;
  FlagV := (m and BIT_6) <> 0;
  FlagN := (m and BIT_7) <> 0;
end;

procedure TCpu6502.OpANDzp; { opcode $25 }
begin
  AluAND(LoadZp);
end;

procedure TCpu6502.OpROLzp; {opcode $26 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluROL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpPLP; { opcode $28 }
begin
  ByteToFlags(PullStack);
end;

procedure TCpu6502.OpANDimm; { opcode $29 }
begin
  AluAND(LoadImm);
end;

procedure TCpu6502.OpROL; { opcode $2A }
begin
  A := AluROL(A);
end;

procedure TCpu6502.OpBITabs; { opcode $2C }
var
  m: byte;
begin
  m := LoadAbs;
  FlagZ := m = 0;
  FlagV := (m and BIT_6) <> 0;
  FlagN := (m and BIT_7) <> 0;
end;

procedure TCpu6502.OpANDabs; { opcode $2D }
begin
  AluAND(LoadAbs);
end;

procedure TCpu6502.OpROLabs; {opcode $2E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluROL(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpROLzpX; {opcode $36 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluROL(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpROLabsX; {opcode $3E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluROL(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpRTI; { opcode $40 }
begin
  ByteToFlags(PullStack);
  PC := PullStack;
  PC := PC or (PullStack shl 8);
end;

procedure TCpu6502.OpEORindX; { opcode $41 }
begin
  AluEOR(LoadIndX);
end;

procedure TCpu6502.OpEORzp; { opcode $45 }
begin
  AluEOR(LoadZp);
end;

procedure TCpu6502.OpLSRzp; {opcode $46 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluLSR(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpPHA; { opcode $48 }
begin
  PushStack(A);
end;

procedure TCpu6502.OpEORimm; { opcode $49 }
begin
  AluEOR(LoadImm);
end;

procedure TCpu6502.OpLSR; { opcode $4A }
begin
  A := AluLSR(A);
end;

procedure TCpu6502.OpJMPabs; { opcode $4C - jump absolute }
begin
  PC := LoadWordIncPC;
end;

procedure TCpu6502.OpEORabs; { opcode $4D }
begin
  AluEOR(LoadAbs);
end;

procedure TCpu6502.OpLSRabs; {opcode $4E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluLSR(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpBVC;    { opcode $50 - branch if overflow clear }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if not FlagV then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpEORindY; { opcode $51 }
begin
  AluEOR(LoadIndY);
end;

procedure TCpu6502.OpEORzpX; { opcode $55 }
begin
  AluEOR(LoadZpX);
end;

procedure TCpu6502.OpLSRzpX; {opcode $56 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluLSR(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpCLI;    { opcode $58 - clear interrupt enable flag  }
begin
  FlagI := false;
end;

procedure TCpu6502.OpEORabsY; { opcode $59 }
begin
  AluEOR(LoadAbsY);
end;

procedure TCpu6502.OpEORabsX; { opcode $5D }
begin
  AluEOR(LoadAbsX);
end;

procedure TCpu6502.OpLSRabsX; {opcode $5E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluLSR(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpRTS; { opcode $60 }
var
  addr: word;
begin
  addr := PullStack;
  addr := addr or (PullStack shl 8);
  inc(addr);
  PC := addr;
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

procedure TCpu6502.OpRORzp; {opcode $66 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluROR(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpPLA; { opcode $68 }
begin
  A := PullStack;
end;

procedure TCpu6502.OpADCimm; { opcode $69 - add immediate to accumulator with carry }
var
  m: byte;
begin
  m := LoadImm;
  AluADC(m);
end;

procedure TCpu6502.OpROR; { opcode $6A }
begin
  A := AluROR(A);
end;

procedure TCpu6502.OpJMPind; { opcode $6C - jump indirect }
var
  addr: word;
begin
  addr := LoadWord(PC);
  PC := LoadByte(addr);

  if (addr and $FF) = $FF then
    addr := addr - $FF	 { emulate wrap around }
  else
    addr := addr + 1;

  PC := PC or (LoadByte(addr) shl 8);
end;

procedure TCpu6502.OpADCabs; { opcode $6D }
var
  m: byte;
begin
  m := LoadAbs;
  AluADC(m);
end;

procedure TCpu6502.OpRORabs; {opcode $6E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluROR(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpRORzpX; {opcode $76 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluROR(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpRORabsX; {opcode $7E }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluROR(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpTXS; { opcode $9A }
begin
  S := X;
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

procedure TCpu6502.OpTSX; { opcode $BA }
begin
  X := S;
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

procedure TCpu6502.OpCPYimm; { opcode $C0 }
begin
  AluCPY(LoadImm);
end;

procedure TCpu6502.OpCMPindX; { opcode $C1 }
begin
  AluCMP(LoadIndX);
end;

procedure TCpu6502.OpCPYzp; { opcode $C4 }
begin
  AluCPY(LoadZp);
end;

procedure TCpu6502.OpCMPzp; { opcode $C5 }
begin
  AluCMP(LoadZp);
end;

procedure TCpu6502.OpDECzp; { opcode $C6 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluDEC(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpCPYabs; { opcode $CC }
begin
  AluCPY(LoadAbs);
end;

procedure TCpu6502.OpDECabs; { opcode $CE }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluDEC(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpDECzpX; { opcode $D6 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluDEC(tmp);
  StoreByte(addr, tmp);
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

procedure TCpu6502.OpDECabsX; { opcode $DE }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluDEC(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpCPXimm; { opcode $E0 }
begin
  AluCPX(LoadImm);
end;

procedure TCpu6502.OpSBCindX; { opcode $E1 }
var
  m: byte;
begin
  m := LoadindX;
  AluSBC(m);
end;

procedure TCpu6502.OpCPXzp; { opcode $E4 }
begin
  AluCPX(LoadZp);
end;

procedure TCpu6502.OpSBCzp; { opcode $E5 }
var
  m: byte;
begin
  m := LoadZp;
  AluSBC(m);
end;

procedure TCpu6502.OpINCzp; { opcode $E6 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpWithAddr(addr);
  tmp := AluINC(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpINX;    { opcode $E8 }
begin
  inc(X);
  AluUpdateNZ(X);
end;

procedure TCpu6502.OpSBCimm; { opcode $E9 }
var
  m: byte;
begin
  m := LoadImm;
  AluSBC(m);
end;

procedure TCpu6502.OpNOP;    { opcode $EA - no operation }
begin
end;

procedure TCpu6502.OpCPXabs; { opcode $EC }
begin
  AluCPX(LoadAbs);
end;

procedure TCpu6502.OpSBCabs; { opcode $ED }
var
  m: byte;
begin
  m := LoadAbs;
  AluSBC(m);
end;

procedure TCpu6502.OpINCabs; { opcode $EE }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsWithAddr(addr);
  tmp := AluINC(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpBEQ;    { opcode $F0 - branch if equal }
var
  rel: byte;
begin
  rel := LoadByteIncPC;
  if FlagZ then PCAddByteSigned(rel);
end;

procedure TCpu6502.OpSBCindY; { opcode $F1 }
var
  m: byte;
begin
  m := LoadindY;
  AluSBC(m);
end;


procedure TCpu6502.OpSBCzpX; { opcode $F5 }
var
  m: byte;
begin
  m := LoadZpX;
  AluSBC(m);
end;

procedure TCpu6502.OpINCzpX; { opcode $F6 }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadZpXWithAddr(addr);
  tmp := AluINC(tmp);
  StoreByte(addr, tmp);
end;

procedure TCpu6502.OpSED;    { opcode $F8 - set decimal flag }
begin
  FlagD := true;
end;

procedure TCpu6502.OpSBCabsY; { opcode $F9 }
var
  m: byte;
begin
  m := LoadAbsY;
  AluSBC(m);
end;

procedure TCpu6502.OpSBCabsX; { opcode $FD }
var
  m: byte;
begin
  m := LoadAbsX;
  AluSBC(m);
end;

procedure TCpu6502.OpINCabsX; { opcode $FE }
var
  addr: word;
  tmp: byte;
begin
  tmp := LoadAbsXWithAddr(addr);
  tmp := AluINC(tmp);
  StoreByte(addr, tmp);
end;


procedure TCpu6502.InitFuncTbl;
var
  i: integer;
begin
  for i := low(FuncTbl) to high(FuncTbl) do
    FuncTbl[i] := nil;
end;

procedure TCpu6502.InitOpTbl;
var
  i: integer;
begin
  for i := low(OpTbl) to high(OpTbl) do begin
    OpTbl[i] := @OpNOP;
  end;

  OpTbl[$00] := @OpBRK;
  OpTbl[$01] := @OpORAindX;
  OpTbl[$05] := @OpORAzp;
  OpTbl[$06] := @OpASLzp;
  OpTbl[$08] := @OpPHP;
  OpTbl[$09] := @OpORAimm;
  OpTbl[$0A] := @OpASL;
  OpTbl[$0D] := @OpORAabs;
  OpTbl[$0E] := @OpASLabs;
  OpTbl[$10] := @OpBPL;      { branch if plus }
  OpTbl[$11] := @OpORAindY;
  OpTbl[$15] := @OpORAzpX;
  OpTbl[$16] := @OpASLzpX;
  OpTbl[$18] := @OpCLC;      { clear carry flag }
  OpTbl[$19] := @OpORAabsY;
  OpTbl[$1D] := @OpORAabsX;
  OpTbl[$1E] := @OpASLabsX;
  OpTbl[$20] := @OpJSR;
  OpTbl[$21] := @OpANDindX;
  OpTbl[$24] := @OpBITzp;
  OpTbl[$25] := @OpANDzp;
  OpTbl[$26] := @OpROLzp;
  OpTbl[$28] := @OpPLP;
  OpTbl[$29] := @OpANDimm;
  OpTbl[$2A] := @OpROL;
  OpTbl[$2C] := @OpBITabs;
  OpTbl[$2D] := @OpANDabs;
  OpTbl[$2E] := @OpROLabs;
  OpTbl[$30] := @OpBMI;      { branch if minus }
  OpTbl[$31] := @OpANDindY;
  OpTbl[$35] := @OpANDzpX;
  OpTbl[$36] := @OpROLzpX;
  OpTbl[$38] := @OpSEC;
  OpTbl[$39] := @OpANDabsY;
  OpTbl[$3D] := @OpANDabsX;
  OpTbl[$3E] := @OpROLabsX;
  OpTbl[$40] := @OpRTI;
  OpTbl[$41] := @OpEORindX;
  OpTbl[$45] := @OpEORzp;
  OpTbl[$46] := @OpLSRzp;
  OpTbl[$48] := @OpPHA;
  OpTbl[$49] := @OpEORimm;
  OpTbl[$4A] := @OpLSR;
  OpTbl[$4C] := @OpJMPabs;   { jump absolute }
  OpTbl[$4D] := @OpEORabs;
  OpTbl[$4E] := @OpLSRabs;
  OpTbl[$50] := @OpBVC;      { branch if overflow clear }
  OpTbl[$51] := @OpEORindY;
  OpTbl[$55] := @OpEORzpX;
  OpTbl[$56] := @OpLSRzpX;
  OpTbl[$58] := @OpCLI;      { clear interrupt flag }
  OpTbl[$5D] := @OpEORabsX;
  OpTbl[$5E] := @OpLSRabsX;
  OpTbl[$60] := @OpRTS;
  OpTbl[$61] := @OpADCindX;
  OpTbl[$65] := @OpADCzp;
  OpTbl[$66] := @OpRORzp;
  OpTbl[$68] := @OpPLA;
  OpTbl[$69] := @OpADCimm;   { add immediate to accumulator with carry }
  OpTbl[$6A] := @OpROR;
  OpTbl[$6C] := @OpJMPind;
  OpTbl[$6D] := @OpADCabs;
  OpTbl[$6E] := @OpRORabs;
  OpTbl[$70] := @OpBVS;      { branch if overflow set }
  OpTbl[$71] := @OpADCindY;
  OpTbl[$75] := @OpADCzpX;
  OpTbl[$76] := @OpRORzpX;
  OpTbl[$78] := @OpSEI;      { set interrupt flag }
  OpTbl[$79] := @OpADCabsY;
  OpTbl[$7D] := @OpADCabsX;
  OpTbl[$7E] := @OpRORabsX;
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
  OpTbl[$9A] := @OpTXS;
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
  OpTbl[$BA] := @OpTSX;
  OpTbl[$BC] := @OpLDYabsX;
  OpTbl[$BD] := @OpLDAabsX;
  OpTbl[$BE] := @OpLDXabsY;
  OpTbl[$C0] := @OpCPYimm;
  OpTbl[$C1] := @OpCMPindX;
  OpTbl[$C4] := @OpCPYzp;
  OpTbl[$C5] := @OpCMPzp;
  OpTbl[$C6] := @OpDECzp;
  OpTbl[$C8] := @OpINY;
  OpTbl[$C9] := @OpCMPimm;
  OpTbl[$CA] := @OpDEX;
  OpTbl[$CC] := @OpCPYabs;
  OpTbl[$CD] := @OpCMPabs;
  OpTbl[$CE] := @OpDECabs;
  OpTbl[$D0] := @OpBNE;      { branch if not equal }
  OpTbl[$D1] := @OpCMPindY;
  OpTbl[$D5] := @OpCMPzpX;
  OpTbl[$D6] := @OpDECzpX;
  OpTbl[$D8] := @OpCLD;      { clear decimal flag }
  OpTbl[$D9] := @OpCMPabsY;
  OpTbl[$DD] := @OpCMPabsX;
  OpTbl[$DE] := @OpDECabsX;
  OpTbl[$E0] := @OpCPXimm;
  OpTbl[$E1] := @OpSBCindX;
  OpTbl[$E4] := @OpCPXzp;
  OpTbl[$E5] := @OpSBCzp;
  OpTbl[$E6] := @OpINCzp;
  OpTbl[$E8] := @OpINX;
  OpTbl[$E9] := @OpSBCimm;
  OpTbl[$EC] := @OpCPXabs;
  OpTbl[$ED] := @OpSBCabs;
  OpTbl[$EA] := @OpNOP;      { no operation }
  OpTbl[$EE] := @OpINCabs;
  OpTbl[$F0] := @OpBEQ;      { branch if equal }
  OpTbl[$F1] := @OpSBCindY;
  OpTbl[$F5] := @OpSBCzpX;
  OpTbl[$F6] := @OpINCzpX;
  OpTbl[$F8] := @OpSED;	     { set decimal flag }
  OpTbl[$F9] := @OpSBCabsY;
  OpTbl[$FD] := @OpSBCabsX;
  OpTbl[$FE] := @OpINCabsX;
end;

procedure TCpu6502.DumpRegs;
begin
  Write(Format('PC=%4.4x  A=%2.2x X=%2.2x Y=%2.2x S=%2.2x ', [PC, A, X, Y, S]));
  if FlagN then Write('N') else Write('.');
  if FlagV then Write('V') else Write('.');
  if FlagD then Write('D') else Write('.');
  if FlagI then Write('I') else Write('.');
  if FlagZ then Write('Z') else Write('.');
  if FlagC then Write('C') else Write('.');
  WriteLn;
end;

end.
