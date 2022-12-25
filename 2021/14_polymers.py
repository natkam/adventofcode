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


PAIR_REPLACEMENTS = {}


def _grow_polymer(sequence: str) -> str:
    result = ""
    for elem, next_elem in zip(sequence, sequence[1:]):
        result += elem
        result += rules[elem + next_elem]

    result += next_elem

    return result


def _get_pair_replacement_after_rounds(pair: str) -> str:
    ROUNDS = 10
    replacement = PAIR_REPLACEMENTS.get(pair)
    if replacement is not None:
        # print(f"Using memoization for pair {pair}")
        return replacement

    sequence = pair
    for _ in range(ROUNDS):
        sequence = _grow_polymer(sequence)

    PAIR_REPLACEMENTS[pair] = sequence
    return sequence


def solve(insertions_number: int) -> int:
    main_counter = Counter()

    for elem, next_elem in zip(template, template[1:]):
        pair = elem + next_elem
        sequence = _get_pair_replacement_after_rounds(pair)

        for elem_1, next_elem_1 in zip(sequence, sequence[1:]):
            pair_1 = elem_1 + next_elem_1
            sequence_1 = _get_pair_replacement_after_rounds(pair_1)

            for elem_2, next_elem_2 in zip(sequence_1, sequence_1[1:]):
                pair_2 = elem_2 + next_elem_2
                sequence_2 = _get_pair_replacement_after_rounds(pair_2)

                # Don't count the letter common to two pairs twice:
                c = Counter(sequence_2[:-1])
                main_counter += c

    # Also count the last element of the entire sequence:
    main_counter[template[-1]] += 1

    # print(main_counter)
    return max(main_counter.values()) - min(main_counter.values())


def _get_char_counts(pair: str, level: int) -> Counter:
    try:
        counter = PAIR_COUNTERS[(pair, level)]
    except KeyError:
        inserted_char = rules[pair]
        left = pair[0] + inserted_char
        right = inserted_char + pair[1]
        c_left = _get_char_counts(left, level - 1)
        c_right = _get_char_counts(right, level - 1)
        c = c_left + c_right
        PAIR_COUNTERS[(pair, level)] = c
        return c
    else:
        return counter


def solve_with_memo_2(rounds_number: int) -> int:
    main_counter = Counter()

    for elem, next_elem in zip(template, template[1:]):
        pair = elem + next_elem
        print(pair)
        c = _get_char_counts(pair, rounds_number)
        main_counter += c

    print(main_counter)
    return max(main_counter.values()) - min(main_counter.values())


if __name__ == "__main__":
    with open("14_test_input") as f:
        data = f.read().splitlines()

    template: str = data[0]
    rules: Dict[str, str] = dict([line.split(" -> ") for line in data[2:]])
    PAIR_COUNTERS = {(pair, 1): Counter(pair + val) for pair, val in rules.items()}

    print(solve_with_memo_2(10))

    # Part 1:
    # print(solve(10))  # 3009
    # print(f"{pair_replacements=}")
    # Part 2:
    # print(solve(40))  # That's way too much at the moment :P

    # import time
    # start = time.perf_counter()
    # print(solve(20))  # 20: 3260359 (~0.5 s), 30: 3374833625 (730s)
    # print(time.perf_counter() - start, "sec")  # ~0.5 s!

    for key, val in PAIR_COUNTERS.items():
        print(key, dict(val))
