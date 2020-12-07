import re
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


def solve_part_one():
    roots = create_tree()
    bag_colour = "shiny gold"
    outer_bags = set()
    for root in roots:
        gold_nodes = findall_by_attr(root, bag_colour)
        for node in gold_nodes:
            outer_bags.update(node.ancestors)
    return len(outer_bags)


if __name__ == "__main__":
    with open("07_input") as f:
        rules = f.read().splitlines()

    print(solve_part_one())
