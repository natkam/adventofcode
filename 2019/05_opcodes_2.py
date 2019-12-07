with open("05_input", "r") as f:
    opcodes = [int(code) for code in f.read().split(",")]


def apply_modes(index, params_number):
    modes = str(opcodes[index])[:-2]
    params = []
    for i in range(1, params_number + 1):
        if not modes or modes[-1] == "0":
            params.append(opcodes[opcodes[index + i]])
        elif modes[-1] == "1":
            params.append(opcodes[index + i])
        else:
            raise ValueError(f"Unknkown mode! {modes[-1]}")
        modes = modes[:-1]

    return params


def process_code_3(index):
    """ Take the input ("1") and write it to the specified position. """
    result_index = opcodes[index + 1]
    value_to_write = 1

    return result_index, value_to_write


def process_code_4(index):
    """ Take the value specified by the param, and output it to sdtout. """
    params = apply_modes(index, 1)
    print(f"[opcode 4] {params[0]}")

    return None


def process_code_1(index):
    """ Sum together two numbers and write the result to a specified position.

    The numbers to sum are specified by the first two params.
    The third param specifies the position to write the result.
    """
    params = apply_modes(index, 2)
    result_index = opcodes[index + 3]
    value_to_write = params[0] + params[1]

    return result_index, value_to_write


def process_code_2(index):
    """ Multiply two numbers and write the result to a specified position.

    The numbers to multiply are specified by the first two params.
    The third param specifies the position to write the result.
    """
    params =  apply_modes(index, 2)
    result_index = opcodes[index + 3]
    value_to_write = params[0] * params[1]

    return result_index, value_to_write


def solve_part_one():
    global opcodes
    advance_pointer = False

    for index in range(0, len(opcodes), 2):
        if advance_pointer:
            advance_pointer = False
            continue

        code = int(str(opcodes[index])[-2:])

        if code == 99:
            break

        if code == 3:
            result_index, value_to_write = process_code_3(index)
            opcodes[result_index] = value_to_write
            continue

        if code == 4:
            process_code_4(index)
            continue

        if code == 1:
            result_index, value_to_write = process_code_1(index)
            opcodes[result_index] = value_to_write
            advance_pointer = True
        elif code == 2:
            result_index, value_to_write = process_code_2(index)
            opcodes[result_index] = value_to_write
            advance_pointer = True
        else:
            raise ValueError(f"Unknown opcode! {type(code)}, {index}, {opcodes[index]}")


def test_case_1():
    global opcodes
    opcodes = [int(code) for code in "3,0,4,0,99".split(",")]
    solve_part_one()  # prints "1"


if __name__ == "__main__":
    # test_case_1()
    solve_part_one()
