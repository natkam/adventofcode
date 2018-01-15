"""
http://adventofcode.com/2017/day/3

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

input: 325489
correct answer: 552

"""
from math import ceil, sqrt

def find_max_odd_root(number):
    max_root = int(sqrt(number))
    max_odd_root = max_root if (max_root % 2) else max_root - 1  # python ternary operator o_O
    return max_odd_root

def distance(x, y):
    return abs(x) + abs(y)

def set_final_coords(min_distance, tail_len):
    side = ceil(tail_len/2/min_distance)
    tail_rem = tail_len % (2 * min_distance)
    return {
        1: (min_distance, -(min_distance) + tail_rem),
        2: (min_distance - tail_rem, min_distance),
        3: (-(min_distance), min_distance - tail_rem),
        4: (-(min_distance) + tail_rem, -(min_distance)),
    }.get(side, None)

def calculate_distance(number):
    max_odd_root = find_max_odd_root(number)
    sq_coordinate = max_odd_root//2
    min_distance = sq_coordinate + 1
    tail_len = number - max_odd_root**2
    if tail_len == 0:
        x = sq_coordinate
        y = -(sq_coordinate)
    else:
        try:
            x, y = set_final_coords(min_distance, tail_len)
        except TypeError:
            print('Error! \nProbably one of these has an incorrect value:'
            + '\n(tail_len, min_distance) == (%s, %s)' % (tail_len, min_distance))
            raise
        # is this an appropriate way to handle it?
    return distance(x, y)

input_number = 325489
print('calculated distance: ', calculate_distance(input_number))
