from itertools import permutations

from computer import Computer


def solve_part_one():
    max_signal = 0

    for input_set in permutations([0, 1, 2, 3, 4]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        amp_A = Computer(opcodes.copy(), inputs=[input_set[0], 0])
        amp_A.solve()
        amp_B = Computer(opcodes.copy(), inputs=[input_set[1], amp_A.inputs[-1]])
        amp_B.solve()
        amp_C = Computer(opcodes.copy(), inputs=[input_set[2], amp_B.inputs[-1]])
        amp_C.solve()
        amp_D = Computer(opcodes.copy(), inputs=[input_set[3], amp_C.inputs[-1]])
        amp_D.solve()
        amp_E = Computer(opcodes.copy(), inputs=[input_set[4], amp_D.inputs[-1]])
        amp_E.solve()

        if amp_E.inputs[-1] > max_signal:
            max_signal = amp_E.inputs[-1]

    return max_signal


def solve_part_two():
    max_signal = 0

    for input_set in permutations([5, 6, 7, 8, 9]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        amp_A = Computer(opcodes.copy(), inputs=[input_set[0], 0])
        amp_B = Computer(opcodes.copy(), inputs=[input_set[1]], previous=amp_A)
        amp_C = Computer(opcodes.copy(), inputs=[input_set[2]], previous=amp_B)
        amp_D = Computer(opcodes.copy(), inputs=[input_set[3]], previous=amp_C)
        amp_E = Computer(opcodes.copy(), inputs=[input_set[4]], previous=amp_D)
        amp_A.previous = amp_E

        amp_E.is_paused = True

        while amp_E.is_paused:
            amp_A.solve_in_loop()
            amp_B.solve_in_loop()
            amp_C.solve_in_loop()
            amp_D.solve_in_loop()
            amp_E.solve_in_loop()

        if amp_E.output_for_next_computer > max_signal:
            max_signal = amp_E.output_for_next_computer

        Computer.instance_counter = 0

    return max_signal


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
