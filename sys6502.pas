program sys6502;

uses
  sysutils, cpu6502;

var
  cpu: TCpu6502;
  mem: array[0..$3FF] of byte;

  flagTest: array[0..14] of byte = ($38, $78, $f8, $18, $58, $d8, $a9, $f9, $69, $01, $d0, $fc, $4c, $fd, $03);
  zpTest: array[0..6] of byte = ($69, $23, $85, $02, $4c, $fd, $03);

procedure StoreMem(const addr: word; const m: byte);
begin
  mem[addr] := m;
end;

function LoadMem(const addr: word) : byte;
begin
  LoadMem := mem[addr];
end;

procedure DumpMem(const addr: word);
var
  i: integer;
begin
  Write(format('%4.4x  ', [addr]));
  for i := 0 to 15 do begin
    Write(format('%2.2x ', [mem[addr+i]]));
  end;
  WriteLn;
end;

procedure LoadPrgAt(const addr: word; var prg: array of byte; const len: integer);
var
  i: integer;
begin
  WriteLn(format('loading %d bytes at %4.4x', [len, addr]));
  for i := 0 to len - 1 do mem[addr+i] := prg[i];
end;

begin
  cpu.InitOpTbl;
  cpu.LoadByte := @LoadMem;
  cpu.StoreByte := @StoreMem;

//  mem[$000] := $4C; mem[$001] := $00; mem[$002] := $01;  { JMP #$100    }
//  mem[$100] := $38;                                      { SEC          }
//  mem[$101] := $B0; mem[$102] := $10;                    { BCS  #$10    }

  LoadPrgAt($0200, zpTest, length(zpTest));
  cpu.PC := $0200;

  cpu.DumpRegs;
  cpu.ExecuteToWithDump($03fd);
  DumpMem(0);

end.

