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
    scenic_score = 0
    for y in range(len(data)):
        row = data[y]
        for x in range(len(row)):
            west = 0
            while True:
                if x == west:
                    # Edge of the forest
                    break
                west += 1
                if row[x - west] >= row[x]:
                    break
            east = 0
            while True:
                if x + east == len(row) - 1:
                    # Edge of the forest
                    break
                east += 1
                if row[x + east] >= row[x]:
                    break
            north = 0
            while True:
                if y == north:
                    # Edge of the forest
                    break
                north += 1
                if data[y - north][x] >= row[x]:
                    break
            south = 0
            while True:
                if y + south == len(row) - 1:
                    # Edge of the forest
                    break
                south += 1
                if data[y + south][x] >= row[x]:
                    break
            scenic_score = max(scenic_score, west * east * north * south)

    return scenic_score


if __name__ == "__main__":
    print(part_one())
    print(part_two())
