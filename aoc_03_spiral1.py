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
import math

def find_max_odd_root(number):
    max_root = int(math.sqrt(number))
    # print(max_root)
    max_odd_root = max_root if (max_root % 2) else max_root - 1  # python ternary operator o_O
    return max_odd_root

def calculate_distance(number):
    max_odd_root = find_max_odd_root(number)
    min_distance = int(max_odd_root/2)
    x, y = min_distance, -min_distance
    print('the odd square x, y: %s' % x, y)
    # x, y = find_odd_square_coords(number)
    tail = number - max_odd_root**2
    # print('tail: %s' % tail)
    if tail == 0:
        print('tail == 0')
        return abs(x) + abs(y)
    elif tail <= 2 * min_distance + 1 + 1: # 2 * (min_distance + 1)
        x += 1
        y += tail - 1
        print('0th side of the square, x, y = %s' % x, y)
        return abs(x) + abs(y)
    elif tail <= 4 * (min_distance + 1):
        y = min_distance + 1
        x = min_distance + 1 - tail % (2 * (min_distance + 1))
        print('1st side of the square, x, y = %s' % x, y)
        return abs(x) + abs(y)
    elif tail <= 6 * (min_distance + 1):
        x = -(min_distance + 1)
        y = min_distance + 1 - tail % (2 * (min_distance + 1))
        print('2nd side of the square, x, y = %s' % x, y)
        return abs(x) + abs(y)
    elif tail <= 8 * (min_distance + 1):
        y = -(min_distance + 1)
        x = -(min_distance + 1) + tail % (2 * (min_distance + 1))
        print('3rd side of the square, x, y = %s' % x, y)
        return abs(x) + abs(y)
    # (side, remainder) = divmod(tail, max_odd_root+1)
    # print(side, remainder)
    # return 123

input_number = 325489
# input_number = 26
# print(find_max_odd_root(input_number))
print('calculated distance: ', calculate_distance(input_number))
