from copy import deepcopy
from typing import Callable, List, Optional, Tuple


class StableStateException(Exception):
    pass


class Grid:
    def __init__(
        self,
        seats: List[List[str]],
        tolerance_level: int,
        max_dist: Optional[int] = None,
    ):
        """Tolerance level is how many neighbours makes a person leave their seat;
        max_dist is how far can be the nearest seat to be relevant.
        """
        self.seats = seats
        self.size = (len(seats[0]), len(seats))  # x, y
        self.previous_state: List[List[str]] = []
        self.tolerance = tolerance_level
        self.max_dist = max_dist or self.size[0] + self.size[1]
        self.age = 0

    @property
    def occupied_seats_count(self) -> int:
        return sum(row.count("#") for row in self.seats)

    def _index_outside_gird(self, x: int, y: int) -> bool:
        if x < 0 or x >= self.size[0] or y < 0 or y >= self.size[1]:
            return True
        return False

    # ### With these methods the code is perhaps somewhat more readable,
    # ### but it also turns out to be considerably slower (ca. 25%).
    # def _is_floor(self, seat: str) -> bool:
    #     return seat == "."
    #
    # def _is_occupied(self, seat: str) -> bool:
    #     if self._is_floor(seat):
    #         raise ValueError(f"Floor cannot be occupied")
    #     return seat == "#"

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
            for dist in range(1, self.max_dist + 1):
                x_n, y_n = get_nearest_seat(dist)
                if self._index_outside_gird(x_n, y_n):
                    break
                seat = self.previous_state[y_n][x_n]
                if seat != ".":
                    if seat == "#":
                        occupied_adjacent_seats += 1

                        # Optimisation - albeit slight, ca. 1 s less on my machine
                        if occupied_adjacent_seats >= self.tolerance:
                            return occupied_adjacent_seats
                    break

        return occupied_adjacent_seats

    def _update_seat(self, x: int, y: int):
        seat = self.previous_state[y][x]
        if seat == ".":
            return

        occupied_neighbours = self._count_occupied_adjacent_seats(x, y)
        if seat == "L" and not occupied_neighbours:
            self.seats[y][x] = "#"
        elif seat == "#" and occupied_neighbours >= self.tolerance:
            self.seats[y][x] = "L"

    def iterate(self):
        self.previous_state = deepcopy(self.seats)

        for y in range(self.size[1]):
            for x in range(self.size[0]):
                self._update_seat(x, y)

        if self.seats == self.previous_state:
            raise StableStateException("No seats change their state any more.")

        self.age += 1

    def __str__(self):
        return "\n".join("".join(self.seats[y]) for y in range(self.size[1]))


def solve_part_one() -> int:
    grid = Grid(deepcopy(seats), tolerance_level=4, max_dist=1)

    while True:
        try:
            grid.iterate()
        except StableStateException:
            break

    return grid.occupied_seats_count


def solve_part_two() -> int:
    grid = Grid(deepcopy(seats), tolerance_level=5)

    while True:
        try:
            grid.iterate()
        except StableStateException:
            break

    return grid.occupied_seats_count


if __name__ == "__main__":
    with open("11_input") as f:
        seats = [[seat for seat in row] for row in f.read().splitlines()]

    print(solve_part_one())
    print(solve_part_two())
