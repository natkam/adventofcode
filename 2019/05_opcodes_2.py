def solve_part_one(opcodes):
    first_input = 1

    for index in range(0, len(opcodes), 2):
        code = int(str(opcodes[index])[-2:])
        modes = str(opcodes[index])[:-2]

        if code == 99:
            break

        if code == 3:
            param = opcodes[index + 1]
            opcodes[param] = first_input
            continue

        if code == 4:
            param = opcodes[index + 1]
            mode = modes[-1]
            if not mode or mode == "0":
                print(f"[opcode 4] {opcodes[param]}")
            elif mode == "1":
                print(f"[opcode 4] {param}")
            continue

        # TODO: modes for instructions 1 & 2; and advance the loop by 4!
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


def test_case_1():
    opcodes = [int(code) for code in "3,0,4,0,99".split(",")]
    solve_part_one(opcodes)  # prints "1"




if __name__ == "__main__":
    # with open("02_input", "r") as f:
    #     opcodes = [int(code) for code in f.read().split(",")]
    #
    # print(solve_part_one(opcodes))
    test_case_1()
