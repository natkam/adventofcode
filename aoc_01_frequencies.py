import functools
import itertools

with open('./01_input.txt', 'r') as f:
    frequencies = f.read()


def solve_part_one(frequencies):
    return functools.reduce(lambda x, y: x + y, [int(freq) for freq in frequencies.splitlines()])


def solve_part_two(frequencies):
    frequencies = [int(freq) for freq in frequencies.splitlines()]
    unique_frequencies = {0}  # using a list turns out to be ridiculously slow!
    last_frequency = 0

    for freq in itertools.cycle(frequencies):
        last_frequency += int(freq)

        if last_frequency in unique_frequencies:
            return last_frequency

        unique_frequencies.add(last_frequency)


print(solve_part_two(frequencies))
