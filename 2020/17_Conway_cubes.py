from typing import List, Tuple, cast

import numpy as np


CYCLES_COUNT = 6


def create_empty_space(starter_shape: Tuple[int, int, int]) -> np.ndarray:
    # space_shape = tuple(dim + 1 for dim in starter_shape)
    return np.full(starter_shape, '.')


def init_space() -> np.ndarray:
    with open("17_input")as f:
        starter = np.array([[[c for c in cubes] for cubes in f.read().splitlines()]])

    space_shape = tuple(dim + 2 * CYCLES_COUNT + 4 for dim in starter.shape)
    space_shape = cast(Tuple[int, int, int], space_shape)
    space = create_empty_space(space_shape)
    z_0, z_1 = CYCLES_COUNT + 2, CYCLES_COUNT + 2 + starter.shape[0]
    y_0, y_1 = CYCLES_COUNT + 2, CYCLES_COUNT + 2 + starter.shape[1]
    x_0, x_1 = CYCLES_COUNT + 2, CYCLES_COUNT + 2 + starter.shape[2]

    space[z_0:z_1, y_0:y_1, x_0:x_1] = starter

    return space


def get_neighbours_indexes(elem: Tuple[int, int, int]) -> List[Tuple[int, int, int]]:
    z, y, x = elem
    neighbours = []
    for i in range(z - 1, z + 2):
        for j in range(y - 1, y + 2):
            for k in range(x - 1, x + 2):
                if i == z and j == y and k == x:
                    continue
                neighbours.append((i, j, k))
    return neighbours


def count_active_neighbours(space: np.ndarray, elem: Tuple[int, int, int]) -> Tuple[List[Tuple[int, int, int]], int]:
    active_count = 0
    neighbours = get_neighbours_indexes(elem)
    for i, j, k in neighbours:
        if space[i, j, k] == "#":  # TODO: make sure there's no IndexErrors!
            active_count += 1
    return neighbours, active_count


def solve_part_one() -> int:
    space = init_space()

    for cycle in range(1, CYCLES_COUNT + 1):
        new_space = create_empty_space(space.shape)
        active_elems = [(z, y, x) for z, y, x in zip(*np.where(space == "#"))]

        for elem in active_elems:
            neighbours, active_count = count_active_neighbours(space, elem)
            if active_count in [2, 3]:
                new_space[elem] = "#"
            for neighbour in neighbours:
                _, active_neighbours_count = count_active_neighbours(space, neighbour)
                if space[neighbour] == "." and active_neighbours_count == 3:
                    new_space[neighbour] = "#"

        space = new_space

    return (space == "#").sum()


if __name__ == '__main__':
    print(solve_part_one())

