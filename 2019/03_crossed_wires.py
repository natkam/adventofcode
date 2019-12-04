import typing


def get_wire_coords(wire_path: typing.List[str]) -> typing.Set:
    coords = set()

    x_now, y_now = 0, 0
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


def calculate_distance(point_1, point_2):
    return sum((abs(coord_1 - coord_2) for coord_1, coord_2 in zip(point_1, point_2)))


def solve_part_one(data):
    first_wire, second_wire = [wire.split(",") for wire in data.splitlines()]
    coords_1 = get_wire_coords(first_wire)
    coords_2 = get_wire_coords(second_wire)

    cross_points = coords_1.intersection(coords_2)
    cross_distances = [calculate_distance((0, 0), point) for point in cross_points]

    return min(cross_distances)


if __name__ == '__main__':
    with open("03_input", "r") as f:
        data = f.read()
        
    print(solve_part_one(data))