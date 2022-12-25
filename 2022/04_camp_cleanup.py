from typing import List


def _parse_input(data: List[str]) -> List[List[List[str]]]:
    return [[rang.split("-") for rang in line.split(",")] for line in data]


def _overlaps(first: List[str], second: List[str]) -> bool:
    return (int(first[0]) <= int(second[0]) and int(first[-1]) >= int(second[0])) or (
        int(first[0]) > int(second[0]) and int(first[0]) <= int(second[-1])
    )


def _contains(first: List[str], second: List[str]) -> bool:
    return int(first[0]) <= int(second[0]) and int(first[-1]) >= int(second[-1])


def part_one():
    with open("04_input") as f:
        data = f.read().splitlines()

    data = _parse_input(data)
    full_contain_count = 0

    for first, second in data:
        if _contains(first, second) or _contains(second, first):
            full_contain_count += 1

    return full_contain_count


def part_two():
    with open("04_input") as f:
        data = f.read().splitlines()

    data = _parse_input(data)
    full_contain_count = 0

    for first, second in data:
        if _overlaps(first, second):
            full_contain_count += 1

    return full_contain_count


if __name__ == "__main__":
    print(part_one())
    print(part_two())
