from __future__ import annotations  # Python 3.8
import typing

with open("06_test_input", "r") as f:
    test_data = f.read().splitlines()

with open("06_test_input_2", "r") as f:
    test_data_2 = f.read().splitlines()

with open("06_input", "r") as f:
    data = f.read().splitlines()


class Node:
    def __init__(
        self, name: str, parent: typing.Optional[Node] = None,
    ):
        self.name = name
        self.parent = parent

    def __repr__(self):
        return f"<Node {self.name}>"

    def __eq__(self, other: Node):
        return self.name == other.name

    def get_all_ancestors(self):
        ancestors = []
        par = self.parent
        while True:
            if par is None:
                break
            ancestors.append(par)
            par = par.parent

        return ancestors


def create_tree(data):
    com = Node("COM")
    existing_planets = {"COM": com}

    child_parent_pairs = dict(line.split(")")[::-1] for line in data)

    for child_name, parent_name in child_parent_pairs.items():
        if parent_name in existing_planets:
            parent = existing_planets[parent_name]
        else:
            parent = Node(name=parent_name)
            existing_planets[parent_name] = parent
        if child_name in existing_planets:
            child = existing_planets[child_name]
            child.parent = parent
        else:
            child = Node(name=child_name, parent=parent)
        existing_planets[child_name] = child

    orphans = {
        name: planet
        for name, planet in existing_planets.items()
        if planet.parent is None and planet.name != "COM"
    }

    if orphans:
        for child_name, parent_name in child_parent_pairs.items():
            if child_name in orphans:
                existing_planets[child_name].parent = existing_planets[parent_name]

    return existing_planets


def solve_first_part(data):
    existing_planets = create_tree(data)
    orbits = [node.get_all_ancestors() for node in existing_planets.values()]
    orbit_count = sum(len(ancestors) for ancestors in orbits)
    return orbit_count


def solve_part_two(data):
    existing_planets = create_tree(data)
    your_ancestors = existing_planets["YOU"].get_all_ancestors()
    santa_ancestors = existing_planets["SAN"].get_all_ancestors()

    common_part = 0
    # start from COM; works because `__eq__` is overwritten
    for i, planet in enumerate(your_ancestors[::-1]):
        if planet in santa_ancestors[::-1]:
            common_part += 1
        else:
            break

    return (len(your_ancestors) - common_part) + (len(santa_ancestors) - common_part)


if __name__ == "__main__":
    # test_result = solve_first_part(test_data)
    # assert test_result == 42, f"[PART 1] wrong result: {test_result}"
    print(solve_first_part(data))

    # test_result_2 = print(solve_part_two(test_data_2))
    # assert test_result_2 == 4, f"[PART 2] wrong result: {test_result_2}"
    print(solve_part_two(data))
