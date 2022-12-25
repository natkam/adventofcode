from itertools import pairwise


def _fourwise(iterable):
    for ((a, b), (b, c)), ((b, c), (c, d)) in pairwise(pairwise(pairwise(iterable))):
        yield a, b, c, d


def part_one_easier():
    data = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    for i, batch in enumerate(_fourwise(data), start=4):
        if len(set(batch)) == len(batch):
            return i


def _solve(step: int) -> int:
    with open("06_input") as f:
        data = f.read()

    for i in range(len(data)):
        batch = data[i : i + step]
        if len(set(batch)) == len(batch):
            return i + step


def part_one():
    return _solve(step=4)


def part_two():
    return _solve(step=14)


if __name__ == "__main__":
    print(part_one())
    print(part_two())
