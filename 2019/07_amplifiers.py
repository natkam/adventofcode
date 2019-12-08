from itertools import permutations

from computer import Computer


def solve_part_one():
    max_signal = 0

    for input_set in permutations([0, 1, 2, 3, 4]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        amp_A = Computer(opcodes.copy(), input_set[0], 0)
        amp_A.solve()
        amp_B = Computer(opcodes.copy(), input_set[1], amp_A.next_input)
        amp_B.solve()
        amp_C = Computer(opcodes.copy(), input_set[2], amp_B.next_input)
        amp_C.solve()
        amp_D = Computer(opcodes.copy(), input_set[3], amp_C.next_input)
        amp_D.solve()
        amp_E = Computer(opcodes.copy(), input_set[4], amp_D.next_input)
        amp_E.solve()

        if amp_E.next_input > max_signal:
            max_signal = amp_E.next_input

    return max_signal


def solve_part_two():
    max_signal = 0

    for input_set in permutations([5, 6, 7, 8, 9]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        # function that takes the previous instance and gets its output?
        # or subprocesses/multiprocessing, or what?

        amp_A = Computer(opcodes.copy(), input_set[0], 0)
        amp_A.solve()
        amp_B = Computer(opcodes.copy(), input_set[1], amp_A.next_input)
        amp_B.solve()
        amp_C = Computer(opcodes.copy(), input_set[2], amp_B.next_input)
        amp_C.solve()
        amp_D = Computer(opcodes.copy(), input_set[3], amp_C.next_input)
        amp_D.solve()
        amp_E = Computer(opcodes.copy(), input_set[4], amp_D.next_input)
        amp_E.solve()

        if amp_E.next_input > max_signal:
            max_signal = amp_E.next_input

    return max_signal


if __name__ == "__main__":
    # with open("07_input") as f:
    # opcodes = [int(code) for code in f.read().split(",")]
    # print(solve_part_one())
    print(solve_part_two())
