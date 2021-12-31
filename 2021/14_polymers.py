# Template:     NNCB
# After step 1: NCNBCHB
# After step 2: NBCCNBBBCBHCB
# After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
# After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

# After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times,
# C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most
# common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces
# 1749 - 161 = 1588.

# Apply 10 steps of pair insertion to the polymer template and find the most and least common elements
# in the result. What do you get if you take the quantity of the most common element and subtract
# the quantity of the least common element?
from collections import Counter
from typing import Dict


def _grow_polymer(template: str) -> str:
    result = ""
    for elem, next_elem in zip(template, template[1:]):
        result += elem
        result += rules[elem + next_elem]

    result += next_elem

    return result


def solve_part_one() -> int:
    sequence = template
    for _ in range(10):
        sequence = _grow_polymer(sequence)
        c = Counter(sequence)
        # print(len(sequence), c)
        # print(sequence)

    # print(len(sequence))
    return max(c.values()) - min(c.values())


if __name__ == "__main__":
    with open("14_input") as f:
        data = f.read().splitlines()

    template: str = data[0]
    rules: Dict[str, str] = dict([line.split(" -> ") for line in data[2:]])

    print(solve_part_one())  # 3009
