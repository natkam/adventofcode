from typing import List, Set, Tuple


class ProgramEndException(Exception):
    """Raised to indicate that GameBoy program has ended."""

    pass


class InfiniteLoopError(Exception):
    """Raised when GameBoy program reaches the same instruction a second time.

    Reaching the same instruction again indicates that the program has entered
    an infinite loop."""

    def __init__(self, operation: str):
        super().__init__()
        self.operation = operation


class GameBoy:
    def __init__(self, instructions: List[Tuple[str, int]]):
        self._instructions = instructions
        self._accumulator = 0
        self._i = 0
        self._executed_instructions: Set[int] = set()

    def get_accumulator_value(self) -> int:
        return self._accumulator

    def get_executed_instructions(self) -> Set[int]:
        return self._executed_instructions

    def execute_instruction_at(self, i: int) -> int:
        """Executes instruction number `i`, return the number of the next instruction.

        Available instructions (each has one integer argument):
         - acc: increase `self.accumulator` by argument value, go to the next
            instruction;
         - jmp: jump to another instruction - argument is the jump length from
            the current instruction;
         - nop: do nothing, go to the next instruction.

        Raises:
            ProgramEndException if `i` exceeds the length of the instruction list.
        """
        if i >= len(self._instructions):
            raise ProgramEndException("Program reached the end.")

        op, arg = self._instructions[i]

        if i in self._executed_instructions:
            raise InfiniteLoopError(operation=op)

        incr = 1

        if op == "acc":
            self._accumulator += arg
        elif op == "jmp":
            incr = arg
        elif op == "nop":
            pass

        self._executed_instructions.add(i)
        self._i += incr

        return self._i


def solve_part_one() -> int:
    """Executes the instructions in the `INSTRUCTIONS` list.

    There's an infinite loop in the instructions list; we're breaking out of it
    as soon as an instruction is reached for a second time.

    Returns:
        Int, the value of `accumulator` just before the program executes an
        instruction a second time.
    """
    game = GameBoy(INSTRUCTIONS)
    i = 0

    while True:
        try:
            i = game.execute_instruction_at(i)
        except InfiniteLoopError:
            return game.get_accumulator_value()


def solve_part_two() -> int:
    """Executes the instructions in the `INSTRUCTIONS` list.

    Available instructions (each has one integer argument):
     - acc: increase `accumulator` by argument value, go to the next instruction;
     - jmp: jump to another instruction - argument is the jump length from
        the current instruction;
     - nop: do nothing, go to the next instruction.

    The program enters an infinite loop at one point. If exactly one of the "jmp"
    instructions is changed to "nop", or one of the "nop" to "jmp", it breaks the
    loop - the program then reaches the end (the last line) and halts.

    Returns:
        Int, the value of `accumulator` after the program execution ends.
    """
    accumulator = 0
    i = 0
    executed_instructions = set()

    while i < len(INSTRUCTIONS):
        op, arg = INSTRUCTIONS[i]

        # if i == 345:
        if i in executed_instructions:
            if op == "jmp":
                op = "nop"
            elif op == "nop":
                op = "jmp"

        executed_instructions.add(i)
        incr = 1

        if op == "acc":
            accumulator += arg
        elif op == "jmp":
            incr = arg

        i += incr

    return accumulator


if __name__ == "__main__":
    with open("08_input") as f:
        instructions = [line.split() for line in f.read().splitlines()]

    INSTRUCTIONS: List[Tuple[str, int]] = [
        (operation, int(argument)) for operation, argument in instructions
    ]

    print(solve_part_one())
    print(solve_part_two())
