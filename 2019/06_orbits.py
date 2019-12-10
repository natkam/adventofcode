from __future__ import annotations  # Python 3.8
import typing

with open("06_test_input", "r") as f:
    test_data = f.read().splitlines()

with open("06_input", "r") as f:
    data = f.read().splitlines()


class Node:
    def __init__(
        self,
        name: str,
        parent: typing.Optional[Node] = None,
        # children: typing.Optional[typing.List[Node]] = None,
    ):
        self.name = name
        self.parent = parent
        # self.children = children or []

    def __repr__(self):
        return f"<Node {self.name}>"

    def get_all_ancestors(self):
        ancestors = []
        par = self.parent
        while True:
            if par is None:
                break
            ancestors.append(par)
            par = par.parent

        return ancestors

    # def get_direct_children(self, nodes: typing.List[Node]):
    #     children = [node for node in nodes if node.parent == self]
    #     return children


def solve_first_part(data):
    com = Node("COM")
    existing_planets = {"COM": com}

    child_parent_pairs = dict(line.split(")")[::-1] for line in data)
    all_children = set(child_parent_pairs)  # 1805 planets + COM = 1806 objects

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

    orphans = {name: planet for name, planet in existing_planets.items() if planet.parent is None and planet.name != "COM"}

    if orphans:
        for child_name, parent_name in child_parent_pairs.items():
            if child_name in orphans:
                existing_planets[child_name].parent = existing_planets[parent_name]

    orbits = [node.get_all_ancestors() for node in existing_planets.values()]
    orbit_count = sum(len(ancestors) for ancestors in orbits)
    return orbit_count


if __name__ == "__main__":
    # test_result = solve_first_part(test_data)
    # print(test_result)
    # assert test_result == 42, f"wrong result: {test_result}"

    print(solve_first_part(data))
