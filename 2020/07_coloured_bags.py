import re
from collections import defaultdict
from typing import Dict, List, Set


def get_containing_bags() -> defaultdict:
    containing_bags = defaultdict(set)

    for rule in RULES:
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
    outer_bags = find_outer_bags_of(BAG_COLOUR, containing_bags)

    return len(outer_bags)


def get_contained_bags() -> Dict[str, List[List[str]]]:
    contained_bags = dict()

    for rule in RULES:
        parent = " ".join(rule.split()[:2])
        children = [
            child.split(maxsplit=1)
            for child in re.findall(r"\d \w+ \w+(?= bags*[,.])", rule)
        ]
        contained_bags[parent] = children

    return contained_bags


def count_inner_bags_of(colour: str, contained_bags: Dict[str, List[List[str]]]) -> int:
    bags_inside = contained_bags[colour]

    inner_bags_count = 0
    for number, bag in bags_inside:
        inner_bags_count += int(number)
        inner_bags_count += int(number) * count_inner_bags_of(bag, contained_bags)

    return inner_bags_count


def solve_part_two() -> int:
    contained_bags = get_contained_bags()
    return count_inner_bags_of(BAG_COLOUR, contained_bags)


if __name__ == "__main__":
    with open("07_input") as f:
        RULES = f.read().splitlines()

    BAG_COLOUR = "shiny gold"

    print(solve_part_one())
    print(solve_part_two())
