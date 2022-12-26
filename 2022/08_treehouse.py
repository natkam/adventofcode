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
            west = 1
            while True:
                if (west_i := x - west) < 0:
                    west -= 1
                    break
                if row[west_i] >= row[x]:
                    break
                west += 1
            east = 1
            while True:
                if (east_i := x + east) >= len(row):
                    east -= 1
                    break
                if row[east_i] >= row[x]:
                    break
                east += 1
            north = 1
            while True:
                if (north_i := y - north) < 0:
                    north -= 1
                    break
                if data[north_i][x] >= row[x]:
                    break
                north += 1
            south = 1
            while True:
                if (south_i := y + south) >= len(row):
                    south -= 1
                    break
                if data[south_i][x] >= row[x]:
                    break
                south += 1
            scenic_score = max(scenic_score, west * east * north * south)
            if scenic_score == 199272:
                breakpoint()

    return scenic_score


if __name__ == "__main__":
    print(part_one())
    print(part_two())
