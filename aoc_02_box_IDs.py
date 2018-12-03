from collections import Counter

with open('./02_input.txt', 'r') as f:
    data = f.read()


def solve_part_one(boxes_data):
    twice_counter = 0
    thrice_counter = 0

    for box in boxes_data.splitlines():
        box_counter = Counter(box)

        if 2 in box_counter.values():
            twice_counter += 1
        if 3 in box_counter.values():
            thrice_counter += 1

    return twice_counter * thrice_counter


def solve_part_two(boxes_data):
    sorted_ids = sorted(boxes_data.splitlines())

    for prev_id, next_id in zip(sorted_ids, sorted_ids[1:]):
        diff_count = 0
        chars_in_common = ''

        for char_in_prev, char_in_next in zip(prev_id, next_id):
            if diff_count > 1:
                break

            if char_in_prev != char_in_next:
                diff_count += 1
            else:
                chars_in_common += char_in_prev

        if diff_count == 1:
            return chars_in_common


print(solve_part_one(data))
print(solve_part_two(data))
