"""
(see 03_spiral1.py)

As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

    Square 1 starts with the value 1.
    Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
    Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
    Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
    Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?

Your puzzle input is still 325489.
correct answer: 330785

"""
from functools import reduce

puzzle_input = 325489


class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return 'Point(%s, %s)' % (self.x, self.y)

    def __str__(self):
        return '(%s, %s)' % (self.x, self.y)


class PtArray(list):
    # The conjecture is that a matrix of squares (Points) of the following size will be big enough.
    min_x = -5
    max_x = 5
    min_y = -5
    max_y = 5

    def __new__(cls, data):
        obj = super().__new__(cls, data)
        return obj

    def __str__(self):
        output = ''
        for row in self[::-1]:
            for p in row:
                output += str(p[1]).center(9)
            output += '\n'
        return output

    def set_val(self, x, y, value):
        self[y - self.min_y][x - self.min_x][1] = value

    def get_val(self, x, y):
        return self[y - self.min_y][x - self.min_x][1]

    def add_neighbours(self, x, y):
        init_val = self.get_val(x, y)
        adjacent_coords = [
            (x + 1, y), (x+1, y+1), (x, y+1), (x-1, y+1), (x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1)
        ]
        adjacent_vals = [self.get_val(x_n, y_n) for (x_n, y_n) in adjacent_coords]
        adjacent_sum = reduce(lambda a, b: a + b, adjacent_vals)
        return adjacent_sum

    def fill_one_field(self, x, y):
        value = self.add_neighbours(x, y)
        self.set_val(x, y, value)
        return value

    def fill_store(self, puzzle_input):
        x = y = 0
        value = 1
        radius = 1
        count = 0
        self.set_val(x, y, value)
        x = x + 1
        while radius <= 4:
            while y < radius:
                value = self.fill_one_field(x, y)
                if value > puzzle_input:
                    return value
                y = y + 1
            while x > -(radius):
                value = self.fill_one_field(x, y)
                if value > puzzle_input:
                    return value
                x = x - 1
            while y > -(radius):
                value = self.fill_one_field(x, y)
                if value > puzzle_input:
                    return value
                y = y - 1
            while x <= radius:
                value = self.fill_one_field(x, y)
                if value > puzzle_input:
                    return value
                x = x + 1
            radius += 1


st1 = PtArray([
    [[Point(x, y), 0] for x in range(PtArray.min_x, PtArray.max_x + 1)] for y in range(PtArray.min_y, PtArray.max_y + 1)
])

try:
    print('The first number bigger than the puzzle input: %s' % st1.fill_store(puzzle_input))
    # print(st1)
except IndexError:
    print("You're approaching the edge of the store!")
    print("The add_neighbours() function needs more wiggle room.")
    print("Expand the store by changing max_x, min_x, max_y, min_y, and try again.")
    raise
