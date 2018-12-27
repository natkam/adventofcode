import operator
from functools import reduce

with open('25_input', 'r') as f:
    data = f.read()

points = sorted(data.splitlines())
MAX_DISTANCE = 3


def calculate_distance(point_1, point_2):
    return sum((abs(coord_1 - coord_2) for coord_1, coord_2 in zip(point_1, point_2)))


def belongs_to_constellation(point, constellation):
    return any((calculate_distance(point, star) <= MAX_DISTANCE for star in constellation))


def solve_part_one(points):
    # TODO: refactor to make more readable
    constellations = []

    for point in points:
        point = [int(coord) for coord in point.split(',')]

        constellations_to_merge = []
        point_belongs_somewhere = False

        for constellation in constellations:
            if belongs_to_constellation(point, constellation):
                point_belongs_somewhere = True
                if not constellations_to_merge:
                    constellation.append(point)

                constellations_to_merge.append(constellation)

        if constellations_to_merge:
            constellations = [c for c in constellations if c not in constellations_to_merge]
            constellations.append(reduce(operator.add, constellations_to_merge))

        if not point_belongs_somewhere:
            constellations.append([point])

    return len(constellations)


if __name__ == '__main__':
    print(solve_part_one(points))
