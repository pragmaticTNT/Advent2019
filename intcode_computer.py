from typing import Callable, List, Iterable

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
            fn_get_in: Optional[Callable[[], int]] = None,
            fn_set_out: Optional[Callable[[int], None]] = None
            ):
        """Creates a new IntMachine running given input Intcode progam."""
        self._program: List[int] = program
        self._mem: _Memory = _Memory(self._program)
        self._pc: int = 0
        self._rb: int = 0

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

    def run(self, to_completion:bool = True) -> int:
        instr = self._step()
        while instr != Instr.HALT:
            if not to_completion and instr == INSTR.OUTPUT:
                yield
            instr = self._step()
        return self._mem[0]

    def get_addr(self, mode, c):
        val = 0
        if mode == 0:
            val = tape[c]
        elif mode == 1:
            val = c
        elif mode == 2:
            val = self._rb + tape[c]
        return val

    def get_val(self, mode, c):
        return self._mem[get_addr(mode, c)]

    def _step(self) -> Instr:
        instr = tape[c]
        mode    = instr // 100
        instr   = instr % 100
        if instr == 1:
            t1 = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t2 = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = t1 + t2
            c += 4
        if instr == 2:
            t1 = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t2 = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = t1 * t2
            c += 4
        if instr == 3:
            addr = getAddr(tape, mode, rel, c+1)
            tape[addr] = fnGetInput()
            c += 2
        if instr == 4:
            s = getVal(tape, mode, rel, c+1)
            c += 2
            fnSetOutput(s)
        if instr == 5:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode, rel, c+2)
            c = t if s else (c + 3)
        if instr == 6:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode, rel, c+2)
            c = t if s == 0 else (c + 3)
        if instr == 7:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = s < t
            c += 4
        if instr == 8:
            s = getVal(tape, mode%10, rel, c+1)
            mode //= 10
            t = getVal(tape, mode%10, rel, c+2)
            mode //= 10
            addr = getAddr(tape, mode, rel, c+3)
            tape[addr] = s == t
            c += 4
        if instr == 9:
            rel += getVal(tape, mode, rel, c+1)
            c += 2

class Instr:

def read_program(fileName):
    tape = []
    with fileinput.input(fileName) as f:
        code = f.readline()
        tape = [int(c) for c in code.split(",")]
    tape += [0 for i in range(len(tape)*10)]
    return tape
