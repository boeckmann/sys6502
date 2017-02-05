program sys6502;

uses
  cpu6502;

var
  cpu: TCpu6502;

begin
  cpu.A := $20;
  cpu.FlagC := true;
  cpu.AluSBC($1f);
  cpu.DumpRegs;
end.

