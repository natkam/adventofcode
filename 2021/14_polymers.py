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


pair_replacements = {}


def _grow_polymer(sequence: str) -> str:
    result = ""
    for elem, next_elem in zip(sequence, sequence[1:]):
        result += elem
        result += rules[elem + next_elem]

    result += next_elem

    return result


def _get_pair_replacement_after_10_rounds(pair: str) -> str:
    replacement = pair_replacements.get(pair)
    if replacement is not None:
        print(f"Using memoisation for pair {pair}")
        # Don't count the letter common to two pairs twice:
        return replacement

    sequence = pair
    for _ in range(10):
        sequence = _grow_polymer(sequence)

    pair_replacements[pair] = sequence
    return sequence


def solve(insertions_number: int) -> int:
    main_counter = Counter()

    for elem, next_elem in zip(template, template[1:]):
        pair = elem + next_elem
        sequence = _get_pair_replacement_after_10_rounds(pair)

        # Don't count the letter common to two pairs twice:
        c = Counter(sequence[:-1])
        main_counter += c

    # Also count the last element of the entire sequence:
    main_counter[next_elem] += 1

    print(main_counter)
    return max(main_counter.values()) - min(main_counter.values())


if __name__ == "__main__":
    with open("14_input") as f:
        data = f.read().splitlines()

    template: str = data[0]
    rules: Dict[str, str] = dict([line.split(" -> ") for line in data[2:]])

    # Part 1:
    print(solve(10))  # 3009

    # Part 2:
    # print(solve(40))  # That's way too much at the moment :P
    # print(solve(20))  # 20: 3260359
