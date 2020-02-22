from enum import Enum
from typing import Callable, Generator, Iterable, List, Optional, Union

class Instr(Enum):
    ADD = 1
    MUL = 2
    INP = 3
    OUT = 4
    JPT = 5
    JPF = 6
    LT = 7
    EQ = 8
    RB = 9
    HALT = 99

class IntMachine:
    """Interpreter for Intcode programs."""
    def __init__(
            self,
            program: Iterable[int],
            noun: Optional[int] = None,
            verb: Optional[int] = None,
            fn_get_input: Optional[Callable[[], int]] = None,
            fn_set_output: Optional[Callable[[int], None]] = None
            ):
        """Creates a new IntMachine running given input Intcode progam."""
        self._program: List[int] = program
        self._mem: _Memory = _Memory(self._program)
        self._pc: int = 0
        self._rb: int = 0

        if noun != None and verb != None:
            self._mem[1] = noun
            self._mem[2] = verb

        self._fn_get_input = fn_get_input or\
            IntMachine.get_raw_input
        self._fn_set_output = fn_set_output or\
            IntMachine.set_raw_output

    @staticmethod
    def get_raw_input() -> int:
        """Standard input method if none provided."""
        return int(input("> "))

    @staticmethod
    def set_raw_output(val: int) -> None:
        """Standard output method if none provided."""
        print(val)

    def run(self) -> int:
        instr = self._step()
        while instr != Instr.HALT.value:
            instr = self._step()
        return self._mem[0]

    def get_addr(self, mode, c):
        val = 0
        if mode == 0:
            val = self._mem[c]
        elif mode == 1:
            val = c
        elif mode == 2:
            val = self._rb + self._mem[c]
        return val

    def get_val(self, mode, c):
        return self._mem[self.get_addr(mode, c)]

    def _step(self) -> Instr:
        instr = self._mem[self._pc]
        mode = instr // 100
        instr = instr % 100
        if instr == Instr.ADD.value:
            t1 = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t2 = self.get_val(mode%10, self._pc+2)
            mode //= 10
            addr = self.get_addr(mode, self._pc+3)
            self._mem[addr] = t1 + t2
            self._pc += 4
        elif instr == Instr.MUL.value:
            t1 = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t2 = self.get_val(mode%10, self._pc+2)
            mode //= 10
            addr = self.get_addr(mode, self._pc+3)
            self._mem[addr] = t1 * t2
            self._pc += 4
        elif instr == Instr.INP.value:
            addr = self.get_addr(mode, self._pc+1)
            self._mem[addr] = self._fn_get_input()
            self._pc += 2
        elif instr == Instr.OUT.value:
            s = self.get_val(mode, self._pc+1)
            self._pc += 2
            self._fn_set_output(s)
        elif instr == Instr.JPT.value:
            s = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t = self.get_val(mode, self._pc+2)
            self._pc = t if s else (self._pc + 3)
        elif instr == Instr.JPF.value:
            s = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t = self.get_val(mode, self._pc+2)
            self._pc = t if s == 0 else (self._pc + 3)
        elif instr == Instr.LT.value:
            s = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t = self.get_val(mode%10, self._pc+2)
            mode //= 10
            addr = self.get_addr(mode, self._pc+3)
            self._mem[addr] = s < t
            self._pc += 4
        elif instr == Instr.EQ.value:
            s = self.get_val(mode%10, self._pc+1)
            mode //= 10
            t = self.get_val(mode%10, self._pc+2)
            mode //= 10
            addr = self.get_addr(mode, self._pc+3)
            self._mem[addr] = s == t
            self._pc += 4
        elif instr == Instr.RB.value:
            self._rb += self.get_val(mode, self._pc+1)
            self._pc += 2
        return instr

# ---> Shameless steal... look I want to finish the problem ok?
class _Memory(list):
    """List[int] wrapper that extends and fills with zeros as neccessary."""
    def __init__(self, program: Iterable[int]):
        super().__init__(program)

    def _extend(self, newlen: int) -> None:
        """Entend memory to be at least this long by padding with zeros."""
        diff = newlen - len(self)
        if diff > 0:
            self.extend([0] * diff)

    def __getitem__(self, address: Union[int, slice]) -> int:
        if isinstance(address, int):
            assert address >= 0
            if address >= len(self):
                self._extend(address + 1)
        elif isinstance(address, slice):
            # (I'll implement the general case if/when the time comes...)
            assert address.step is None or address.step > 0

            if address.stop - 1 >= len(self):
                self._extend(address.stop)
        return super().__getitem__(address)

    def __setitem__(self, address: int, value: int) -> None:
        assert address >= 0
        if address >= len(self):
            self._extend(address + 1)
        super().__setitem__(address, value)
