def part_one() -> int:
    alias = {
        "A": 1,  # "Rock"
        "B": 2,  # "Paper"
        "C": 3,  # "Scissors"
        "X": 1,  # "Rock"
        "Y": 2,  # "Paper"
        "Z": 3,  # "Scissors"
    }
    result_points = {"loss": 0, "draw": 3, "win": 6}

    with open("02_input") as f:
        data = f.read().splitlines()

    total = 0

    for line in data:
        elf, you = [alias[shape] for shape in line.split()]
        if elf == you:
            # draw => 3 points
            total += result_points["draw"] + you
        elif elf % 3 + 1 == you:
            # win => 6 points
            total += result_points["win"] + you
        else:
            # loss => 0 points
            total += result_points["loss"] + you

    return total


def part_two():
    alias = {
        "A": 1,  # "Rock"
        "B": 2,  # "Paper"
        "C": 3,  # "Scissors"
        "X": 0,  # loss
        "Y": 3,  # draw
        "Z": 6,  # win
    }
    winning_shapes = {1: "B", 2: "C", 3: "A"}
    losing_shapes = {1: "C", 2: "A", 3: "B"}

    with open("02_input") as f:
        data = f.read().splitlines()

    total = 0

    for line in data:
        elf, result = [alias[shape] for shape in line.split()]
        if result == 3:
            # draw => choose the same shape as the opponent does
            you = elf
        elif result == 6:
            # win => choose a shape with a +1 greater value
            you = alias[winning_shapes[elf]]
        else:
            # loss
            you = alias[losing_shapes[elf]]

        total += you + result

    return total


if __name__ == "__main__":
    print(part_one())
    print(part_two())
