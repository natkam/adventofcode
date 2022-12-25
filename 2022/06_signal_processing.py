from itertools import pairwise
from typing import Iterable, Tuple


def _fourwise(iterable: Iterable) -> Iterable[Tuple[str, str, str, str]]:
    for ((a, b), (b, c)), ((b, c), (c, d)) in pairwise(pairwise(pairwise(iterable))):
        yield a, b, c, d


def part_one_easier() -> int:
    data = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    for i, batch in enumerate(_fourwise(data), start=4):
        if len(set(batch)) == len(batch):
            return i
    return 0


def _solve(step: int) -> int:
    with open("06_input") as f:
        data = f.read()

    for i in range(len(data)):
        batch = data[i : i + step]
        if len(set(batch)) == len(batch):
            return i + step
    return 0


def part_one() -> int:
    return _solve(step=4)


def part_two() -> int:
    return _solve(step=14)


if __name__ == "__main__":
    print(part_one())
    print(part_two())
