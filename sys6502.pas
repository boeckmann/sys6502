{ SYS6502 - A MOS Technology MCS6502 emulator
  Copyright (C) 2020-2021 Bernd Böckmann

  Distributed under MIT license. Please see LICENSE file.
}

program sys6502;

{$MODE objfpc}{$H+}

uses
  SysUtils, math, cpu6502;

const
  MAX_MEM = $FFFF;
  PC_HALT = $FFFC;	// no further execution if PC = PC_HALT
var
  cpu: TCpu6502;
  mem: array[0..MAX_MEM] of byte;

  command: string;
  terminating: boolean;

  keyBuf: string[88] = '';
  keyPtr: integer = 1;
  verbose: boolean = false;

procedure SysChrin(cpu: PCpu6502);
begin
  if keyPtr > Length(keyBuf) then begin
    ReadLn(keyBuf);
    SetLength(keyBuf, Length(keyBuf) + 1);
    keyBuf[Length(keyBuf)] := chr($0A);
    keyPtr := 1;
  end;

  with cpu^ do begin
    A := Ord(keyBuf[keyPtr]);
    OpRTS;
  end;

  inc(keyPtr);
end;

procedure SysChrout(cpu: PCpu6502);
begin
  with cpu^ do begin
    Write(Chr(A)); Flush(Stdout);
    OpRTS;
  end;
end;

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
  if Verbose then WriteLn(s);
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

function MinWord(const a, b: word) : word;
begin
  if b < a then MinWord := b else MinWord := a;
end;

procedure DoMemdump(const start: word; const len: word);
var
  f, t, i: word;
begin
  f := start;
  while (f <= start + len - 1) and (f >= start) do begin
    t := MinWord(f+15, start+len-1);

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
  mem[$FFFE] := $FC;                // set interrupt vector to terminate addr
  mem[$FFFF] := $FF;

  mem[$01FE] := $FC;                // init end of  stk end with terminate addr
  mem[$01FF] := $FF;

  cpu.Init(@LoadMem, @StoreMem);
  cpu.InstallBuiltinProc($FFCF, @SysChrin);
  cpu.InstallBuiltinProc($FFD2, @SysChrout);
end;

begin
  InitSystem;

  if ParamCount = 0 then begin
    verbose := true;
    terminating := false;
    while not terminating do begin
      command := ReadCommand;
      InterpretCommand(command);
    end;
  end
  else begin
    DoLoad(ParamStr(1), $200);
    DoRun;
  end
end.

