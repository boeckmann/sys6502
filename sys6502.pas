program sys6502;

{$MODE objfpc}{$H+}

uses
  SysUtils, math, cpu6502;

const
  MAX_MEM = $ffff;
  PC_HALT = $FFFC;	// no further execution if PC = PC_HALT
var
  cpu: TCpu6502;
  mem: array[0..MAX_MEM] of byte;

  command: string;
  terminating: boolean;


procedure StoreMem(const addr: word; const m: byte);
begin
  mem[addr] := m;
end;

function LoadMem(const addr: word) : byte;
begin
  LoadMem := mem[addr];
end;

function ParseNumber(const s: string; var n: integer) : boolean;
var
  code: word;
begin
  Val(s, n, code);
  ParseNumber := code = 0;
end;

procedure ShowVerbose(const s: string);
begin
  WriteLn(s);
end;

procedure ShowError(const err: string);
begin
  Write('ERROR: '); WriteLn(err);
end;

procedure DoLoad(const fn: string; const addr: word);
var
  f: file of byte;
  len: word;
  rlen: word;
begin
  {$I-}
  Assign(f, fn);
  Reset(f);
  if IOResult <> 0 then begin
    ShowError(fn + ' not found');
    Exit;
  end;
  len := FileSize(f);
  BlockRead(f, mem[addr], min(addr+len, MAX_MEM), rlen);
  if IOResult <> 0 then begin
    ShowError('can not read from ' + fn);
    Close(f);
    Exit;
  end;
  Close(f);
  {$I+}
  ShowVerbose(format('%d bytes loaded at $%4.4x from %s', [rlen, addr, fn]));
end;

procedure DoCpuStatus;
begin
  cpu.DumpRegs;
end;

procedure DoHelp;
begin
  WriteLn('  command help');
  WriteLn('  ------------');
  WriteLn('   c : show cpu status (pc, registers, flags)');
  WriteLn('   l <file> [addr] : load <file> to addr if given, else to $0200');
  WriteLn('   m <num>: dump page <num>');
  WriteLn('   p : dump page pc points to');
  WriteLn('   q : quit');
  WriteLn('   r : run until pc=$FFFC');
  WriteLn('   s : execute next instruction');
end;

procedure DoMemdump(const start: word; const len: word);
var
  f, t, i: word;
begin
  f := start;
  while (f <= start + len - 1) and (f >= start) do begin
    t := min(f+15, start+len-1);

    Write(IntToHex(f, 4));
    Write(': ');
    for i := f to t do begin
      Write(IntToHex(mem[i], 2));
      Write(' ');
    end;
    WriteLn;

    f := f + 16;
  end;
end;

procedure DoMemdumpPage(const addr: word);
var
  page: word;
begin
  page := addr shr 8;
  DoMemdump(page shl 8, $100);
end;

procedure DoRun;
begin
  cpu.ExecuteTo($FFFC);
end;

procedure DoStep;
begin
  if cpu.PC <> PC_HALT then begin
    cpu.ExecuteNext;
  end;
  cpu.DumpRegs;
end;

function ReadCommand : string;
begin
  Write(format('PC=%4.4X> ', [cpu.PC]));
  ReadLn(ReadCommand);
end;

procedure InterpretCommand(const cmd : string);

  function IsSpace(c: char) : boolean;
  begin
    IsSpace := (c = ' ') or (c = #10) or (c = #13);
  end;

  function NextToken(const cmd: string; var pos: integer): string;
  var
    start: integer;
  begin
    while (pos <= Length(cmd)) and (IsSpace(cmd[pos])) do inc(pos);
    start := pos;
    while (pos <= Length(cmd)) and (not IsSpace(cmd[pos])) do inc(pos);

    NextToken := Copy(cmd,start,pos-start);
  end;

var
  token: string;
  pos: integer;
  arg1, arg2: string;
  narg1, narg2: integer;

begin
  pos := 1;
  token := NextToken(cmd, pos);

  if token = 'c' then begin   { show cpu status }
    DoCpuStatus;
  end
  
  else if token = 'h' then begin
    DoHelp;
  end
  
  else if token = 'l' then begin   { load data (program) }
    arg1 := NextToken(cmd, pos);
    if arg1 <> '' then begin
      arg2 := NextToken(cmd, pos);
      if arg2 <> '' then begin
        if ParseNumber(arg2, narg2) then
          DoLoad(arg1, narg2)
        else
          ShowError('number expected');
      end else
        DoLoad(arg1, $0200);
    end else ShowError('no file given');
  end
  
  else if token = 'm' then begin
    arg1 := NextToken(cmd, pos);
    if arg1 <> '' then begin
      if ParseNumber(arg1,narg1) then
        DoMemdumpPage(narg1 * $100)
      else
        ShowError('page number expected');
    end else
      ShowError('page number expected');
  end

  else if token = 'p' then begin  { show page memory containing PC }
    DoMemdumpPage(cpu.PC);
  end

  else if token = 'q' then begin   { quit simulator }
    terminating := true;
  end
  
  else if token = 'r' then begin   { run simulator until PC=$FFFC }
    DoRun;
  end

  else if token = 's' then begin
    DoStep;
  end

  else begin
    ShowError('illegal command');
  end;
end;

procedure InitSystem;
begin
  FillChar(mem, Length(mem), 0);    // init memory
  mem[$FFFC] := $00;                // set 6502 reset vector to $0200
  mem[$FFFD] := $02;
  mem[$0200] := $4C;                // 0200: JMP $FFFC
  mem[$0201] := $FC;                // absolute jump to $FFFC terminates sim loop
  mem[$0202] := $FF;

  cpu.Init(@LoadMem, @StoreMem);
  cpu.ResetCPU;
end;

begin
  InitSystem;

  terminating := false;
  while not terminating do begin
    command := ReadCommand;
    InterpretCommand(command);
  end;

  // LoadPrgAt($0200, zpTest, length(zpTest));
  //cpu.PC := $0200;

  //cpu.DumpRegs;
  //cpu.ExecuteToWithDump($03fd);
  //DumpMem(0);
end.

