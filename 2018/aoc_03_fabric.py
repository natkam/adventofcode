import re

import numpy as np

with open('./03_input.txt', 'r') as f:
    data = f.read()

MAX_WIDTH = 1000
MAX_LENGTH = 1000

LINE_PATTERN = re.compile(r'#(?P<id>\d+) @ (?P<x_0>\d+),(?P<y_0>\d+): (?P<x_size>\d+)x(?P<y_size>\d+)')


def parse_input_line(line):
    if LINE_PATTERN.match(line):
        numbers_as_strings = LINE_PATTERN.match(line).groups()

        return map(int, numbers_as_strings)


def create_claimed_fabric(data):
    fabric = np.zeros((MAX_WIDTH, MAX_LENGTH), np.int8)

    for line in data.splitlines():
        claim_id, x_0, y_0, x_size, y_size = parse_input_line(line)
        fabric[x_0:x_0 + x_size, y_0:y_0 + y_size] += 1

    return fabric


def solve_part_one(data):
    claimed_fabric = create_claimed_fabric(data)

    return (claimed_fabric > 1).sum()


def solve_part_two(data):
    claimed_fabric = create_claimed_fabric(data)

    for line in data.splitlines():
        claim_id, x_0, y_0, x_size, y_size = parse_input_line(line)

        if (claimed_fabric[x_0:x_0 + x_size, y_0:y_0 + y_size] == 1).all():
            return claim_id


print(solve_part_two(data))
