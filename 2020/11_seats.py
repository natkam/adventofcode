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

    def _index_outside_gird(self, x: int, y: int) -> bool:
        if x < 0 or y < 0:  # -1 won't raise an IndexError in Python...
            return True
        try:
            _ = self.previous_state[y][x]
        except IndexError:
            return True

        return False

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
        for get_nearest_seat in nearest_seat_azimuths:
            for dist in range(1, self.size[0] + self.size[1]):
                x_n, y_n = get_nearest_seat(dist)
                if self._index_outside_gird(x_n, y_n):
                    break
                if self.previous_state[y_n][x_n] != ".":
                    if self.previous_state[y_n][x_n] == "#":
                        occupied_adjacent_seats += 1
                    break

        return occupied_adjacent_seats

    def _update_seat(self, x: int, y: int):
        if self.seats[y][x] == ".":
            return

        occupied_neighbours = self._count_occupied_adjacent_seats(x, y)
        if self.previous_state[y][x] == "L" and not occupied_neighbours:
            self.seats[y][x] = "#"
        elif (
                self.previous_state[y][x] == "#"
                and occupied_neighbours >= self.tolerance
        ):
            self.seats[y][x] = "L"

    def iterate(self):
        self.previous_state = deepcopy(self.seats)

        for y in range(self.size[1]):
            for x in range(self.size[0]):
                self._update_seat(x, y)

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

    # print(solve_part_one())
    print(solve_part_two())
