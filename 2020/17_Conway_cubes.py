from itertools import product
from typing import List, Tuple, cast

import numpy as np

Point3D = Tuple[int, int, int]
Point4D = Tuple[int, int, int, int]
CYCLES_COUNT = 6


def init_space() -> np.ndarray:
    with open("17_input") as f:
        starter = np.array([[[c for c in cubes] for cubes in f.read().splitlines()]])

    # Add some wiggle room on the edges to avoid IndexError when checking neighbours.
    space_shape = tuple(dim + 2 * (CYCLES_COUNT + 1) for dim in starter.shape)
    space_shape = cast(Point3D, space_shape)
    space = np.full(space_shape, ".")
    z_0, z_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[0]
    y_0, y_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[1]
    x_0, x_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[2]

    space[z_0:z_1, y_0:y_1, x_0:x_1] = starter

    return space


def get_neighbours_indexes(elem: Point3D) -> List[Point3D]:
    z, y, x = elem
    neighbours = [
        p
        for p in product(range(z - 1, z + 2), range(y - 1, y + 2), range(x - 1, x + 2))
        if not p == (z, y, x)
    ]
    return neighbours


def get_neighbours(space: np.ndarray, elem: Point3D) -> Tuple[List[Point3D], int]:
    neighbours = get_neighbours_indexes(elem)
    active_count = len([n for n in neighbours if space[n] == "#"])
    return neighbours, active_count


def solve_part_one() -> int:
    space = init_space()

    for cycle in range(1, CYCLES_COUNT + 1):
        new_space = np.full(space.shape, ".")
        active_elems = [(z, y, x) for z, y, x in np.argwhere(space == "#")]

        for elem in active_elems:
            neighbours, active_count = get_neighbours(space, elem)
            if active_count in [2, 3]:
                new_space[elem] = "#"
            for neighbour in neighbours:
                _, active_neighbours_count = get_neighbours(space, neighbour)
                if space[neighbour] == "." and active_neighbours_count == 3:
                    new_space[neighbour] = "#"

        space = new_space

    return (space == "#").sum()


def init_4d_space() -> np.ndarray:
    with open("17_input") as f:
        starter = np.array([[[[c for c in cubes] for cubes in f.read().splitlines()]]])

    # Add some wiggle room on the edges to avoid IndexError when checking neighbours.
    space_shape = tuple(dim + 2 * (CYCLES_COUNT + 1) for dim in starter.shape)
    space_shape = cast(Point4D, space_shape)
    space = np.full(space_shape, ".")
    z_0, z_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[0]
    y_0, y_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[1]
    x_0, x_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[2]
    w_0, w_1 = CYCLES_COUNT + 1, CYCLES_COUNT + 1 + starter.shape[3]

    space[z_0:z_1, y_0:y_1, x_0:x_1, w_0:w_1] = starter

    return space


def get_4d_neighbours_indexes(elem: Point4D) -> List[Point4D]:
    z, y, x, w = elem
    neighbours = [
        p
        for p in product(
            range(z - 1, z + 2),
            range(y - 1, y + 2),
            range(x - 1, x + 2),
            range(w - 1, w + 2),
        )
        if not p == (z, y, x, w)
    ]
    return neighbours


def get_4d_neighbours(space: np.ndarray, elem: Point4D) -> Tuple[List[Point4D], int]:
    neighbours = get_4d_neighbours_indexes(elem)
    active_count = len([n for n in neighbours if space[n] == "#"])
    return neighbours, active_count


def solve_part_two() -> int:
    space = init_4d_space()

    for cycle in range(1, CYCLES_COUNT + 1):
        new_space = np.full(space.shape, ".")
        active_elems = [(z, y, x, w) for z, y, x, w in np.argwhere(space == "#")]
        for elem in active_elems:
            neighbours, active_count = get_4d_neighbours(space, elem)
            if active_count in [2, 3]:
                new_space[elem] = "#"
            for neighbour in neighbours:
                _, active_neighbours_count = get_4d_neighbours(space, neighbour)
                if space[neighbour] == "." and active_neighbours_count == 3:
                    new_space[neighbour] = "#"

        space = new_space

    return (space == "#").sum()


if __name__ == "__main__":
    # print(solve_part_one())
    print(solve_part_two())
