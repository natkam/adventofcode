from typing import Literal, cast, List

from computer import Computer


def solve_part_one() -> int:
    """Uncomment the `print` statements to see how the beam looks on a plane."""

    with open("19_input", "r") as f:
        program: List[int] = [int(code) for code in f.read().split(",")]

    x_min = 0
    x_max = 50  # excluding x_max
    y_min = 0
    y_max = 50  # excluding y_max

    affected_field_count = 0

    for y in range(y_min, y_max):
        # print(f"{y}: ", end="")
        for x in range(x_min, x_max):
            drone = Computer(opcodes=program.copy(), inputs=[x, y])
            drone.solve()
            # drone.inputs will contain the output of the program;
            # 0 - no effect, 1 - affected field
            affected_field_count += drone.inputs[-1]

            # print("." if drone.inputs[-1] == 0 else "#", end="")
        # print()

    return affected_field_count


def solve_part_two():
    """By checking the coordinates of the beam edges, I figured out the equations
    of the two straight lines that (approximately) delimit the beam. Given the straight
    line equation y = a * x + b, their coefficients are:
    (a2 = 1.694915254237288, b2 = -0.8474576271186436) for min x within the beam,
    and
    (a1 = 1.3157894736842106, b1 = 0) for the max x within the beam.

    Given the fact that a1 and a2 are the respective tangents of the lines, and
    the fact that the size of the square that represents the Santa's ship is 100x100,
    I was able to calculate the approximate x-width of the beam into which the ship
    should fit: 159 to 176 (59 more than the ship's width in the row closest to you,
    and 76 more than the ship's width in the row furthers from you). In other words:
     Y <= upper_beam_edge(X - 59)  <==>  Y >= lower_beam_edge(X + 99)
     Y + 99 <= upper_beam_edge(X)  <==>  Y + 99 >= lower_beam_edge(X + 99 + 76)

    Hence, in particular:
    X >= 609.588...
    Y >= 932.35...
    I used these numbers to estimate the range of points to check.
    """

    # def lower_beam_edge(x: int) -> int:
    #     a1 = 1.3157894736842106
    #     b1 = 0
    #     return round(a1 * x + b1)
    #
    # def upper_beam_edge(x: int) -> int:
    #     a2 = 1.694915254237288
    #     b2 = -0.8474576271186436
    #     return round(a2 * x + b2)
    #
    # We have to find minimal integers X, Y such that:
    #  1. Y >= lower_beam_edge(X + 99)
    # and
    #  2. Y + 99 <= upper_beam_edge(X)

    with open("19_input", "r") as f:
        program: List[int] = [int(code) for code in f.read().split(",")]

    x_min = 600
    x_max = 670
    y_min = 930
    y_max = 1000

    def check_beam(x: int, y: int) -> Literal[0, 1]:
        drone = Computer(opcodes=program.copy(), inputs=[x, y])
        drone.solve()
        return cast(Literal[0, 1], drone.inputs[-1])

    for y in range(y_min, y_max):
        for x in range(x_min, x_max):
            beam = check_beam(x, y)

            if beam == 1:
                x_plus_99 = check_beam(x + 99, y)
                if x_plus_99 == 0:
                    break
                if check_beam(x, y + 99) == 1:
                    return x * 10_000 + y


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
