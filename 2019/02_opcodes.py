def solve_part_one(first, second):
    with open("02_input", "r") as f:
        opcodes = [int(code) for code in f.read().split(",")]

    opcodes[1] = first
    opcodes[2] = second

    for index in range(0, len(opcodes), 4):
        code = opcodes[index]
        if code == 99:
            break

        i_1, i_2, i_result = (
            opcodes[index + 1],
            opcodes[index + 2],
            opcodes[index + 3],
        )

        if code == 1:
            result = opcodes[i_1] + opcodes[i_2]
        elif code == 2:
            result = opcodes[i_1] * opcodes[i_2]
        else:
            raise ValueError(f"Unknown opcode! {type(code)}, {index}, {opcodes[index]}")

        opcodes[i_result] = result

    return opcodes[0]


def solve_part_two():
    for first in range(100):
        for second in range(100):
            result = solve_part_one(first, second)
            if result == 19690720:
                return 100 * first + second


if __name__ == "__main__":
    print(solve_part_one(12, 2))
    print(solve_part_two())
