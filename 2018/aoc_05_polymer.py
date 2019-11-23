from string import ascii_lowercase

with open('./05_input', 'r') as f:
    data = f.read()


def reduce_polymer(data, unit_to_remove=None):
    letters_i = iter(data)
    reduced_polymer = []

    for letter in letters_i:
        if letter.lower() == unit_to_remove:
            continue

        if not reduced_polymer:
            reduced_polymer.append(letter)
            continue

        if abs(ord(reduced_polymer[-1]) - ord(letter)) == 32:
            reduced_polymer.pop()
            continue

        reduced_polymer.append(letter)

    return reduced_polymer


def solve_part_one(data):
    reduced_polymer = reduce_polymer(data)

    return len(reduced_polymer)


def solve_part_two(data):
    min_length = len(data) + 1

    for unit_to_remove in ascii_lowercase:
        reduced_polymer = reduce_polymer(data, unit_to_remove)

        if len(reduced_polymer) < min_length:
            min_length = len(reduced_polymer)

    return min_length


print(solve_part_one(data))
print(solve_part_two(data))
