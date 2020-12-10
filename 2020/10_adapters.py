from collections import Counter
from functools import reduce
from itertools import combinations


def solve_part_one() -> int:
    diffs = []
    for lower, higher in zip(joltages, joltages[1:]):
        diffs.append(higher - lower)
    counts = Counter(diffs)

    return counts[1] * counts[3]


def solve_part_two() -> int:
    all_series = []
    i = 0
    while i < len(joltages) - 2:
        series = []
        while joltages[i + 2] - joltages[i] <= 3:
            # Find series of >=3 consecutive numbers; the middle one can be skipped
            series.append(joltages[i + 1])
            i += 1
        if series:
            all_series.append(series)
        i += 1

    # print("max series len:", max(len(s) for s in all_series))  # 3

    all_possible_skips = []
    for series in all_series:
        possible_skips = 0
        for k in range(min(len(series) + 1, 3)):
            # There are no series longer than 3, so this will work.
            # Otherwise the formula below would have to be more complicated.
            possible_skips += len(list(combinations(series, k)))
        all_possible_skips.append(possible_skips)

    return reduce(lambda x, y: x * y, all_possible_skips)


if __name__ == "__main__":
    with open("10_input") as f:
        joltages = [0] + sorted([int(joltage) for joltage in f.read().splitlines()])
    joltages.append(joltages[-1] + 3)

    print(solve_part_one())
    print(solve_part_two())
