import operator
from collections import defaultdict
from functools import reduce
from typing import Dict, List, Set, Tuple


FoodsInputList = List[Tuple[Set[str], List[str]]]


def parse_food_list(foods: List[str]) -> FoodsInputList:
    foods_parsed: FoodsInputList = []
    for line in foods:
        ings_str, allerg_str = line.rstrip(")").split(" (contains ")
        ings = set(ings_str.split())
        allerg = allerg_str.split(", ")
        foods_parsed.append((ings, allerg))
    return foods_parsed


def sort_ingredients_by_allergens(foods_parsed: FoodsInputList) -> defaultdict:
    """For each allergen, collects all the ingredient sets that contain it."""
    allergens = defaultdict(list)
    for ings, allerg in foods_parsed:
        for a in allerg:
            allergens[a].append(ings)
    return allergens


def simplify_ingredients(allergens: defaultdict) -> Dict[str, Set[str]]:
    """For each allergen, finds ingredients common to all its ingredient sets."""
    return {a: reduce(operator.and_, ing_sets) for a, ing_sets in allergens.items()}


def get_allergen_ingredients(foods_parsed: FoodsInputList) -> Dict[str, str]:
    """Identifies which allergen is found in which ingredient."""
    allergens = sort_ingredients_by_allergens(foods_parsed)
    simplified = simplify_ingredients(allergens)
    identified = {a: min(ings) for a, ings in simplified.items() if len(ings) == 1}

    while len(simplified) != len(identified):
        for a, ings in simplified.items():
            if a in identified:
                continue
            ings -= set(identified.values())
            if len(ings) == 1:
                identified[a] = min(ings)

    return identified


def solve_part_one(foods: List[str]) -> int:
    foods_parsed = parse_food_list(foods)
    allergens = get_allergen_ingredients(foods_parsed)

    safe_ings_cout = 0
    for ings, _ in foods_parsed:
        safe_ings_cout += len(ings - set(allergens.values()))

    return safe_ings_cout


def solve_part_two(foods: List[str]) -> str:
    foods_parsed = parse_food_list(foods)
    allergens = get_allergen_ingredients(foods_parsed)

    return ",".join(ing for a, ing in sorted(allergens.items()))


if __name__ == "__main__":
    with open("21_input") as f:
        foods = f.read().splitlines()

    print(solve_part_one(foods))
    print(solve_part_two(foods))
