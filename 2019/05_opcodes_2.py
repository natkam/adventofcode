def solve_part_one(opcodes):
    first_input = 1
    advance_pointer = False

    for index in range(0, len(opcodes), 2):
        if advance_pointer:
            advance_pointer = False
            continue

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
            if not modes or modes[-1] == "0":
                print(f"[opcode 4] {opcodes[param]}")
            elif modes[-1] == "1":
                print(f"[opcode 4] {param}")
            else:
                raise ValueError(f"Unknkown mode! {modes[-1]}")
            continue

        params = []
        for i in range(1, 3):
            if not modes or modes[-1] == "0":
                params.append(opcodes[opcodes[index + i]])
            elif modes[-1] == "1":
                params.append(opcodes[index + i])
            else:
                raise ValueError(f"Unknkown mode! {modes[-1]}")
            modes = modes[:-1]

        if code == 1:
            result = params[0] + params[1]
        elif code == 2:
            result = params[0] * params[1]
        else:
            raise ValueError(f"Unknown opcode! {type(code)}, {index}, {opcodes[index]}")

        opcodes[opcodes[index + 3]] = result
        advance_pointer = True


def test_case_1():
    opcodes = [int(code) for code in "3,0,4,0,99".split(",")]
    solve_part_one(opcodes)  # prints "1"


if __name__ == "__main__":
    # test_case_1()

    with open("05_input", "r") as f:
        opcodes = [int(code) for code in f.read().split(",")]

    solve_part_one(opcodes)
