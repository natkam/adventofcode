from collections import Counter
from functools import reduce
from itertools import combinations
from typing import List


def solve_part_one() -> int:
    counts = Counter(higher - lower for lower, higher in zip(joltages, joltages[1:]))

    return counts[1] * counts[3]


def solve_part_two() -> int:
    all_series: List[List[int]] = []
    i = 1
    while True:
        series: List[int] = []
        try:
            while joltages[i + 1] - joltages[i - 1] <= 3:
                # Find series of >=3 consecutive numbers; the middle one can be skipped.
                series.append(joltages[i])
                i += 1
        except IndexError:
            break
        finally:
            if series:
                all_series.append(series)
            i += 1

    # print("max series len:", max(len(s) for s in all_series))  # 3

    all_possible_skips: List[int] = []
    for series in all_series:
        # There are no series longer than 3, so this will work:
        # count the ways in which 0, 1 or 2 adapters can be skipped in a series
        # (0 or 1 adapter for series shorter than 3).
        # With longer series the formula below would have to be more complicated.
        possible_skips = sum(
            len(list(combinations(series, k))) for k in range(min(len(series) + 1, 3))
        )
        all_possible_skips.append(possible_skips)

    return reduce(lambda x, y: x * y, all_possible_skips)


if __name__ == "__main__":
    with open("10_input") as f:
        joltages = [0] + sorted([int(joltage) for joltage in f.read().splitlines()])
    joltages.append(joltages[-1] + 3)

    print(solve_part_one())
    print(solve_part_two())
