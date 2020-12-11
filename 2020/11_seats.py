from copy import deepcopy
from typing import List, Tuple


class StableStateException(Exception):
    pass


class Grid:
    def __init__(self, seats: List[List[str]]):
        self.seats = seats
        self.size = (len(seats[0]), len(seats))  # x, y
        self.previous_seats: List[List[str]] = []
        self.age = 0

    @property
    def occupied_seats_count(self) -> int:
        return sum(row.count("#") for row in self.seats)

    def _count_occupied_adjacent_seats(self, x: int, y: int) -> int:
        occupied_adjacent_seats = 0
        neighbours: List[Tuple[int, int]] = [
            (x - 1, y),
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x + 1, y),
            (x + 1, y + 1),
            (x, y + 1),
            (x - 1, y + 1),
        ]
        for x_n, y_n in neighbours:
            if x_n < 0 or y_n < 0:
                continue
            try:
                occupied_adjacent_seats += 1 if self.previous_seats[y_n][x_n] == "#" else 0
            except IndexError:
                pass
        # if self.age == 2:
        #     breakpoint()
        return occupied_adjacent_seats

    def _position_is_floor(self, x: int, y: int) -> bool:
        return self.seats[y][x] == "."

    # def _seat_is_empty(self, x: int, y: int) -> bool:
    #     state = self.seats[y][x]
    #     if state == ".":
    #         raise ValueError(f"Position ({x}, {y}) is not a seat, it's floor!")
    #     return state == "L"

    def iterate(self):
        self.previous_seats = deepcopy(self.seats)

        for y in range(self.size[1]):
            for x in range(self.size[0]):
                if self._position_is_floor(x, y):
                    continue
                occupied_neighbours = self._count_occupied_adjacent_seats(x, y)
                if self.previous_seats[y][x] == "L" and not occupied_neighbours:
                    self.seats[y][x] = "#"
                elif self.previous_seats[y][x] == "#" and occupied_neighbours >= 4:
                    self.seats[y][x] = "L"

        if self.seats == self.previous_seats:
            raise StableStateException("No seats change state any more.")

        self.age += 1
        # print(self, "\n")

    def __str__(self):
        return "\n".join("".join(self.seats[y]) for y in range(self.size[1]))


def solve_part_one() -> int:
    grid = Grid(deepcopy(seats))
    # print(grid, "\n")
    while True:
        try:
            grid.iterate()
        except StableStateException:
            break

    print(grid.age)
    return grid.occupied_seats_count


if __name__ == "__main__":
    with open("11_input") as f:
        seats = [[seat for seat in row] for row in f.read().splitlines()]

    print(solve_part_one())
