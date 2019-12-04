import typing

Coord = typing.Tuple[int, int]
ORIGIN = (0, 0)


def get_wire_coords(wire_path: typing.List[str]) -> typing.Set:
    coords = set()
    x_now, y_now = ORIGIN

    for path in wire_path:
        direction = path[0]
        steps_count = int(path[1:])
        if direction == "R":
            coords.update([(x_now + x, y_now) for x in range(1, steps_count + 1)])
            x_now += steps_count
        elif direction == "L":
            coords.update([(x_now - x, y_now) for x in range(1, steps_count + 1)])
            x_now -= steps_count
        elif direction == "U":
            coords.update([(x_now, y_now + y) for y in range(1, steps_count + 1)])
            y_now += steps_count
        elif direction == "D":
            coords.update([(x_now, y_now - y) for y in range(1, steps_count + 1)])
            y_now -= steps_count

    return coords


def get_taxi_distance(point_1: Coord, point_2: Coord):
    return sum((abs(coord_1 - coord_2) for coord_1, coord_2 in zip(point_1, point_2)))


def solve_part_one(data):
    first_wire, second_wire = [wire.split(",") for wire in data.splitlines()]
    coords_1 = get_wire_coords(first_wire)
    coords_2 = get_wire_coords(second_wire)

    cross_points = coords_1.intersection(coords_2)
    cross_distances = [get_taxi_distance(ORIGIN, point) for point in cross_points]

    return min(cross_distances)


def get_wire_path(wire_path: typing.List[str]) -> typing.List[Coord]:
    coords = []
    x_now, y_now = ORIGIN

    for path in wire_path:
        direction = path[0]
        steps_count = int(path[1:])
        if direction == "R":
            coords.extend([(x_now + x, y_now) for x in range(1, steps_count + 1)])
            x_now += steps_count
        elif direction == "L":
            coords.extend([(x_now - x, y_now) for x in range(1, steps_count + 1)])
            x_now -= steps_count
        elif direction == "U":
            coords.extend([(x_now, y_now + y) for y in range(1, steps_count + 1)])
            y_now += steps_count
        elif direction == "D":
            coords.extend([(x_now, y_now - y) for y in range(1, steps_count + 1)])
            y_now -= steps_count

    return coords


def walk_the_wire(coords: typing.List[Coord], end: Coord):
    distance_walked = 0

    for point in coords:
        distance_walked += 1
        if point == end:
            return distance_walked


def solve_part_two(data):
    first_wire, second_wire = [wire.split(",") for wire in data.splitlines()]
    coords_1: list = get_wire_path(first_wire)
    coords_2 = get_wire_path(second_wire)
    cross_points = set(coords_1).intersection(set(coords_2))

    distances = []
    for intersection in cross_points:
        dist_1 = walk_the_wire(coords_1, intersection)
        dist_2 = walk_the_wire(coords_2, intersection)
        distances.append(dist_1 + dist_2)

    return min(distances)


if __name__ == "__main__":
    with open("03_input", "r") as f:
        data = f.read()

    print(solve_part_one(data))
    print(solve_part_two(data))
