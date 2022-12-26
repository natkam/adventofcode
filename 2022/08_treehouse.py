def part_one():
    with open("08_input") as f:
        data = f.read().splitlines()

    data = [[int(tree) for tree in row] for row in data]
    result = []
    for y in range(len(data)):
        row = data[y]
        result.append([])
        for x in range(len(row)):
            if (
                all(tree < row[x] for tree in row[:x])
                # Visible from the west
                or all(tree < row[x] for tree in row[x + 1 :])
                # Visible from the east
                or all(data[i][x] < row[x] for i in range(y))
                # Visible from the north
                or all(data[i][x] < row[x] for i in range(y + 1, len(data)))
                # Visible from the south
            ):
                result[y].append(True)
            else:
                result[y].append(False)

    return sum(sum(line) for line in result)


def part_two():
    with open("08_input") as f:
        data = f.read().splitlines()

    data = [[int(tree) for tree in row] for row in data]
    return


if __name__ == "__main__":
    print(part_one())
    print(part_two())
