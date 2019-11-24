import typing
from collections import Counter

import numpy as np

with open("06_input", "r") as f:
    data = f.read().splitlines()

COORDS = {
    str(number): coords
    for number, coords in enumerate(
        map(lambda coord_pair: [int(x) for x in coord_pair.split(",")], data)
    )
}


def calculate_extrema():
    x_coords = set(pair[0] for pair in COORDS.values())
    y_coords = set(pair[1] for pair in COORDS.values())
    x_min, x_max = min(x_coords), max(x_coords)
    y_min, y_max = min(y_coords), max(y_coords)

    return x_min, x_max, y_min, y_max


def calculate_distance(point_1, point_2):
    return sum((abs(coord_1 - coord_2) for coord_1, coord_2 in zip(point_1, point_2)))


def get_distances_list(x: int, y: int) -> typing.List:
    # TODO: these get calculated for each part of the task; store them somewhere?
    return [(pt, calculate_distance((x, y), COORDS[pt])) for pt in COORDS]


def get_closest_point(x: int, y: int) -> str:
    distances = get_distances_list(x, y)

    # have to find the minimum manually, because ties don't count!
    closest_points = [distances[0][0]]
    d_min = distances[0][1]

    for d in distances[1:]:
        if d[1] < d_min:
            d_min = d[1]
            closest_points = [d[0]]
        elif d[1] == d_min:
            closest_points.append(d[0])

    if len(closest_points) > 1:  # i.e. location is eqidistant from >=2 points
        return "x"

    return closest_points[0]


def get_grid_of_closest_points() -> np.ndarray:
    x_min, x_max, y_min, y_max = calculate_extrema()
    values = [
        [get_closest_point(x + x_min, y + y_min) for x in range(x_max + 1 - x_min)]
        for y in range(y_max + 1 - y_min)
    ]

    return np.array(values)


def get_total_distance(x: int, y: int):
    return sum(d[1] for d in get_distances_list(x, y))


def get_grid_of_total_distances():
    x_min, x_max, y_min, y_max = calculate_extrema()
    values = [
        [get_total_distance(x + x_min, y + y_min) for x in range(x_max + 1 - x_min)]
        for y in range(y_max + 1 - y_min)
    ]

    return np.array(values)


def solve_first():
    grid = get_grid_of_closest_points()
    values_on_edges = set(
        np.concatenate((np.unique(grid[[0, -1]]), np.unique(grid[:, [0, -1]])))
    )

    c = Counter(grid.flatten())
    del c["x"]  # get rid of the ties
    for edge in values_on_edges:
        del c[edge]
    biggest_area: typing.Tuple[str, int] = c.most_common(1)[0]

    return biggest_area[1]


def solve_second():
    grid = get_grid_of_total_distances()
    result = grid[grid < 10000].shape[0]

    return result


if __name__ == "__main__":
    import time

    start = time.perf_counter()
    print(solve_first())  # ~ 5 s; the calculated area could be smaller, in fact
    print(solve_second())  # ~ 5 s
    print(time.perf_counter() - start)  # ~ 10 s
