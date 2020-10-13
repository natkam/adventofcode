from computer import Computer


def solve_part_one():
    x_min = 0
    x_max = 50  # excluding x_max
    y_min = 0
    y_max = 50  # excluding y_max

    affected_field_count = 0

    for y in range(y_min, y_max):
        for x in range(x_min, x_max):
            with open('19_input', 'r') as f:
                program = [int(code) for code in f.read().split(',')]
            drone = Computer(opcodes=program, inputs=[x, y])
            drone.solve()
            # drone.inputs will contain the output of the program;
            # 0 - no effect, 1 - affected field
            affected_field_count += drone.inputs[-1]

    return affected_field_count


if __name__ == '__main__':
    print(solve_part_one())
