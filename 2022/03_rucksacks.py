from functools import reduce


def _get_priority(char: str) -> int:
    if char.islower():
        return ord(char) - 96
    else:
        return ord(char) - 38


def part_one():
    with open("03_input") as f:
        data = f.read().splitlines()

    value = 0

    for line in data:
        half = int(len(line) / 2)
        first, second = set(line[:half]), set(line[half:])
        common = first.intersection(second).pop()
        value += _get_priority(common)

    return value


def part_two():
    with open("03_input") as f:
        data = f.read().splitlines()

    value = 0
    group = []

    for i, line in enumerate(data, start=1):
        group.append(set(line))
        if not i % 3:
            common = reduce(lambda x, y: x.intersection(y), group).pop()
            value += _get_priority(common)
            group = []

    return value


if __name__ == "__main__":
    print(part_one())
    print(part_two())
