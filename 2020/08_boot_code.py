from copy import deepcopy
from typing import List, Set, Tuple


class ProgramEndException(Exception):
    """Raised to indicate that GameBoy program has ended."""

    pass


class InfiniteLoopError(Exception):
    """Raised when GameBoy program reaches the same instruction a second time.

    Reaching the same instruction again indicates that the program has entered
    an infinite loop."""

    pass


class GameBoy:
    def __init__(self, instructions: List[Tuple[str, int]]):
        self._instructions = instructions
        self._i = 0
        self._accumulator = 0
        self._executed_instructions: Set[int] = set()

    def get_accumulator_value(self) -> int:
        return self._accumulator

    def get_executed_instructions(self) -> Set[int]:
        return self._executed_instructions

    def boot(self) -> None:
        """Starts running the instructions from `self.instructions`.

        Raises:
            ProgramEndException if `i` exceeds the length of the instruction list.
            InfiniteLoopError if the same instruction is reached for a second time.
            (Exceptions are raised in the `self._execute_instruction` method.)
        """
        while True:
            self._execute_instruction()

    def _execute_instruction(self) -> None:
        """Executes instruction at `self._i`, sets `_i` to the next instruction number.

        Available instructions (each has one integer argument):
         - acc: increase `self._accumulator` by argument value, go to the next
            instruction;
         - jmp: jump to another instruction - argument is the jump length from
            the current instruction;
         - nop: do nothing, go to the next instruction.

        Raises:
            ProgramEndException if `i` exceeds the length of the instruction list.
            InfiniteLoopError if the same instruction is reached for a second time,
                i.e. when `i` is already present in the executed instructions set.
        """
        if self._i >= len(self._instructions):
            raise ProgramEndException("Program reached the end.")

        if self._i in self._executed_instructions:
            raise InfiniteLoopError()

        op, arg = self._instructions[self._i]
        incr = 1

        if op == "acc":
            self._accumulator += arg
        elif op == "jmp":
            incr = arg
        elif op == "nop":
            pass

        self._executed_instructions.add(self._i)
        self._i += incr


def solve_part_one() -> int:
    """Executes the instructions in the `INSTRUCTIONS` list.

    There's an infinite loop in the instructions list; we're breaking out of it
    as soon as an instruction is reached for a second time.

    Returns:
        Int, the value of the game accumulator just before the program executes
        an instruction a second time.
    """
    game = GameBoy(INSTRUCTIONS)
    try:
        game.boot()
    except InfiniteLoopError:
        return game.get_accumulator_value()

    raise RuntimeError("Expected an InfiniteLoopError, something went wrong!")


def get_loop_indexes() -> Set[int]:
    """Returns the indexes of instructions executed before the loop was detected."""
    test_game = GameBoy(INSTRUCTIONS)
    try:
        test_game.boot()
    except InfiniteLoopError:
        return test_game.get_executed_instructions()

    raise RuntimeError("Expected an InfiniteLoopError, something went wrong!")


def create_updated_instructions(i_swap: int) -> List[Tuple[str, int]]:
    """Returns a copy of `INSTRUCTIONS` with the operation swapped at `i_swap` index."""
    new_instructions: List[Tuple[str, int]] = deepcopy(INSTRUCTIONS)
    old_op, arg = INSTRUCTIONS[i_swap]
    if old_op == "jmp":
        new_instructions[i_swap] = ("nop", arg)
    elif old_op == "nop":
        new_instructions[i_swap] = ("jmp", arg)

    return new_instructions


def solve_part_two() -> int:
    """Executes the instructions in the `INSTRUCTIONS` list.

    The program enters an infinite loop at one point. If exactly one of the "jmp"
    instructions is changed to "nop", or one of the "nop" to "jmp", it breaks the
    loop - the program then reaches the end (the last line) and halts. The point is
    to find which instruction to change, and then run the program until a halt.

    Returns:
        Int, the value of the game's accumulator after the program ends.
    """
    indexes_in_loop = get_loop_indexes()

    for i_swap in indexes_in_loop:
        old_op, _ = INSTRUCTIONS[i_swap]
        if old_op == "acc":
            continue

        new_instructions = create_updated_instructions(i_swap)
        game = GameBoy(new_instructions)
        try:
            game.boot()
        except InfiniteLoopError:
            continue
        except ProgramEndException:
            return game.get_accumulator_value()

    raise RuntimeError("Expected a ProgramEndException, something went wrong!")


if __name__ == "__main__":
    with open("08_input") as f:
        instructions = [line.split() for line in f.read().splitlines()]

    INSTRUCTIONS: List[Tuple[str, int]] = [
        (operation, int(argument)) for operation, argument in instructions
    ]

    print(solve_part_one())
    print(solve_part_two())
