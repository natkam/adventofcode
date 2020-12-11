from copy import deepcopy
from typing import Callable, List, Tuple


class StableStateException(Exception):
    pass


class Grid:
    def __init__(self, seats: List[List[str]], tolerance_level: int):
        """Tolerance level means how many neighbours can a person stand before they go away."""
        self.seats = seats
        self.size = (len(seats[0]), len(seats))  # x, y
        self.previous_state: List[List[str]] = []
        self.tolerance = tolerance_level
        self.age = 0

    @property
    def occupied_seats_count(self) -> int:
        return sum(row.count("#") for row in self.seats)

    def _count_occupied_adjacent_seats(self, x: int, y: int) -> int:
        occupied_adjacent_seats = 0
        nearest_seat_azimuths: List[Callable[[int], Tuple[int, int]]] = [
            lambda d: (x - d, y),
            lambda d: (x - d, y - d),
            lambda d: (x, y - d),
            lambda d: (x + d, y - d),
            lambda d: (x + d, y),
            lambda d: (x + d, y + d),
            lambda d: (x, y + d),
            lambda d: (x - d, y + d),
        ]
        for direction in nearest_seat_azimuths:
            dist = 1  # distance from the next seat
            x_n, y_n = direction(dist)
            if x_n < 0 or y_n < 0:  # Cannot rely on IndexError here. Python <3
                continue
            try:
                while self.previous_state[y_n][x_n] == ".":
                    dist += 1
                    x_n, y_n = direction(dist)
                    if x_n < 0 or y_n < 0:  # Cannot rely on IndexError here.
                        raise IndexError
            except IndexError:
                continue

            if self.previous_state[y_n][x_n] == "#":
                occupied_adjacent_seats += 1

        return occupied_adjacent_seats

    def _place_is_floor(self, x: int, y: int) -> bool:
        return self.seats[y][x] == "."

    def iterate(self):
        self.previous_state = deepcopy(self.seats)

        for y in range(self.size[1]):
            for x in range(self.size[0]):
                if self._place_is_floor(x, y):
                    continue
                occupied_neighbours = self._count_occupied_adjacent_seats(x, y)
                if self.previous_state[y][x] == "L" and not occupied_neighbours:
                    self.seats[y][x] = "#"
                elif (
                    self.previous_state[y][x] == "#"
                    and occupied_neighbours >= self.tolerance
                ):
                    self.seats[y][x] = "L"

        if self.seats == self.previous_state:
            raise StableStateException("No seats change their state any more.")

        self.age += 1
        # print(self, "\n")

    def __str__(self):
        return "\n".join("".join(self.seats[y]) for y in range(self.size[1]))


def solve_part_one() -> int:
    grid = Grid(deepcopy(seats), tolerance_level=4)
    while True:
        try:
            grid.iterate()
        except StableStateException:
            break

    return grid.occupied_seats_count


def solve_part_two() -> int:
    grid = Grid(deepcopy(seats), tolerance_level=5)
    # print(grid, "\n")
    while True:
        try:
            grid.iterate()
        except StableStateException:
            break

    # print(grid.age)
    return grid.occupied_seats_count


if __name__ == "__main__":
    with open("11_input") as f:
        seats = [[seat for seat in row] for row in f.read().splitlines()]

    print(solve_part_two())
