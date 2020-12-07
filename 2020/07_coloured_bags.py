import re
from collections import defaultdict
from typing import Set

from anytree import Node, findall_by_attr


def create_tree() -> Set[Node]:
    roots = set()
    tree_dict = dict()

    for rule in rules:
        parent_name = " ".join(rule.split()[:2])
        children_names = re.findall(r"(?<= \d )\w+ \w+(?= bags*[,.])", rule)

        # If parent node exists in the tree, it should not be created anew.
        parent_nodes = tree_dict.setdefault(parent_name, [Node(parent_name)])

        for child_name in children_names:
            # Nodes with the same names can appear in multiple places in the tree.
            tree_dict.setdefault(child_name, []).extend(
                Node(child_name, parent=parent_node) for parent_node in parent_nodes
            )
            # If a node is a child of another, it should be in the `roots`.
            roots = roots.difference(tree_dict[child_name])

        roots.update(node.root for node in parent_nodes)

    return roots


def get_containing_bags() -> defaultdict:
    containing_bags = defaultdict(set)

    for rule in rules:
        parent_name = " ".join(rule.split()[:2])
        children_names = re.findall(r"(?<= \d )\w+ \w+(?= bags*[,.])", rule)

        for child in children_names:
            containing_bags[child].add(parent_name)

    return containing_bags


def find_outer_bags_of(colour: str, containing_bags: defaultdict) -> Set[str]:
    parents = containing_bags[colour]
    outer_bags = set(parents)
    
    for parent in parents:
        outer_bags |= find_outer_bags_of(parent, containing_bags)

    return outer_bags


def solve_part_one() -> int:
    containing_bags = get_containing_bags()
    bag_colour = "shiny gold"
    outer_bags = find_outer_bags_of(bag_colour, containing_bags)

    return len(outer_bags)



if __name__ == "__main__":
    with open("07_input") as f:
        rules = f.read().splitlines()

    print(solve_part_one())
