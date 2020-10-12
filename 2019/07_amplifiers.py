from itertools import permutations

from computer import Computer


def solve_part_one():
    max_signal = 0

    for input_set in permutations([0, 1, 2, 3, 4]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        amp_A = Computer(opcodes.copy(), initial_input=input_set[0], next_input=0)
        amp_A.solve()
        amp_B = Computer(
            opcodes.copy(), initial_input=input_set[1], next_input=amp_A.next_input
        )
        amp_B.solve()
        amp_C = Computer(
            opcodes.copy(), initial_input=input_set[2], next_input=amp_B.next_input
        )
        amp_C.solve()
        amp_D = Computer(
            opcodes.copy(), initial_input=input_set[3], next_input=amp_C.next_input
        )
        amp_D.solve()
        amp_E = Computer(
            opcodes.copy(), initial_input=input_set[4], next_input=amp_D.next_input
        )
        amp_E.solve()

        if amp_E.next_input > max_signal:
            max_signal = amp_E.next_input

    return max_signal


def solve_part_two():
    max_signal = 0

    for input_set in permutations([5, 6, 7, 8, 9]):
        with open("07_input") as f:
            opcodes = [int(code) for code in f.read().split(",")]

        amp_A = Computer(opcodes.copy(), initial_input=input_set[0], next_input=0)
        amp_B = Computer(opcodes.copy(), initial_input=input_set[1], previous=amp_A)
        amp_C = Computer(opcodes.copy(), initial_input=input_set[2], previous=amp_B)
        amp_D = Computer(opcodes.copy(), initial_input=input_set[3], previous=amp_C)
        amp_E = Computer(opcodes.copy(), initial_input=input_set[4], previous=amp_D)
        amp_A.previous = amp_E

        amp_A.solve()

        amp_B.resume()
        amp_C.resume()
        amp_D.resume()
        amp_E.resume()

        while amp_E.is_paused:
            amp_A.resume()
            amp_B.resume()
            amp_C.resume()
            amp_D.resume()
            amp_E.resume()

        if amp_E.output_for_next_computer > max_signal:
            max_signal = amp_E.output_for_next_computer

        Computer.instance_counter = 0

    return max_signal


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
