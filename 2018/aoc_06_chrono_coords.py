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


def calculate_distance(point_1, point_2):
    return sum((abs(coord_1 - coord_2) for coord_1, coord_2 in zip(point_1, point_2)))


def get_closest_point(x: int, y: int) -> str:
    distances = [(pt, calculate_distance((x, y), COORDS[pt])) for pt in COORDS]

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


def solve_first():
    x_coords = set(pair[0] for pair in COORDS.values())
    y_coords = set(pair[1] for pair in COORDS.values())
    x_min, x_max = min(x_coords), max(x_coords)
    y_min, y_max = min(y_coords), max(y_coords)

    values = [
        [get_closest_point(x + x_min, y + y_min) for x in range(y_max + 1 - y_min)]
        for y in range(y_max + 1 - y_min)
    ]

    ar = np.array(values)
    values_on_edges = set(
        np.concatenate((np.unique(ar[[0, -1]]), np.unique(ar[:, [0, -1]])))
    )

    c = Counter(ar.flatten())
    del c["x"]  # get rid of the ties
    for edge in values_on_edges:
        del c[edge]
    result = c.most_common(1)

    return result

if __name__ == "__main__":
    import time
    start = time.perf_counter()
    print(solve_first())
    print(time.perf_counter() - start)  # ~ 4.5 s

