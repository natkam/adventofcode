from typing import List, Tuple


def solve_part_one() -> int:
    """Executes the instructions in the `INSTRUCTIONS` list.

    Available instructions (each has one integer argument):
     - acc: increase `accumulator` by argument value, go to the next instruction;
     - jmp: jump to another instruction - argument is the jump length from
        the current instruction;
     - nop: do nothing, go to the next instruction.

    Returns:
        Int, the value of `accumulator` just before the program executes an
        instruction a second time.
    """
    codes: List[Tuple[str, int]] = [
        (operation, int(argument)) for operation, argument in INSTRUCTIONS
    ]
    accumulator = 0
    i = 0
    executed_instructions = set()

    while True:
        if i in executed_instructions:
            return accumulator

        executed_instructions.add(i)
        op, arg = codes[i]
        incr = 1

        if op == "acc":
            accumulator += arg
        elif op == "jmp":
            incr = arg
        elif op == "nop":
            pass

        i += incr


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
    codes: List[Tuple[str, int]] = [
        (operation, int(argument)) for operation, argument in INSTRUCTIONS
    ]
    accumulator = 0
    i = 0
    executed_instructions = set()

    while i < len(codes):
        op, arg = codes[i]

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
        INSTRUCTIONS = [line.split() for line in f.read().splitlines()]

    print(solve_part_one())
    print(solve_part_two())
