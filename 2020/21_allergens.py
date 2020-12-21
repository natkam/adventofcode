import operator
from collections import defaultdict
from functools import reduce
from typing import Dict, List, Set, Tuple


def parse_food_list(foods: List[str]) -> List[Tuple[Set[str], List[str]]]:
    foods_parsed: List[Tuple[Set[str], List[str]]] = []
    for line in foods:
        ing_str, allerg_str = line.rstrip(")").split(" (contains ")
        ing = set(ing_str.split())
        allerg = allerg_str.split(", ")
        foods_parsed.append((ing, allerg))
    return foods_parsed


def sort_ingredients_by_allergens(
    foods_parsed: List[Tuple[Set[str], List[str]]]
) -> defaultdict:
    allergens = defaultdict(list)
    for ings, allerg in foods_parsed:
        for a in allerg:
            allergens[a].append(ings)
    return allergens


def simplify_ingredients(allergens: defaultdict) -> Dict[str, List[str]]:
    """For each allergen, find ingredients common to all ingredient sets."""
    return {a: reduce(operator.and_, ing_sets) for a, ing_sets in allergens.items()}


def get_allergen_ingredients(allergens: defaultdict) -> Dict[str, List[str]]:
    simplified = simplify_ingredients(allergens)
    identified = {min(ings) for a, ings in simplified.items() if len(ings) == 1}
    identified_alls = {a: min(ings) for a, ings in simplified.items() if len(ings) == 1}

    while len(simplified) != len(identified):
        for a, ings in simplified.items():
            if a in identified_alls:
                continue
            ings -= identified
            simplified[a] = ings
            if len(ings) == 1:
                identified.update(ings)
                identified_alls[a] = min(ings)

    return identified_alls


def solve_part_one(foods: List[str]) -> int:
    foods_parsed = parse_food_list(foods)
    allergens = sort_ingredients_by_allergens(foods_parsed)

    identified = get_allergen_ingredients(allergens)
    dangerous_ingredients = set(identified.values())

    safe_ings_cout = 0
    for ings, _ in foods_parsed:
        safe_ings_cout += len(ings - dangerous_ingredients)

    return safe_ings_cout


def solve_part_two(foods: List[str]) -> str:
    foods_parsed = parse_food_list(foods)
    allergens = sort_ingredients_by_allergens(foods_parsed)

    identified = get_allergen_ingredients(allergens)

    return ",".join(ing for a, ing in sorted(identified.items()))


if __name__ == "__main__":
    with open("21_input") as f:
        foods = f.read().splitlines()

    print(solve_part_one(foods))
    print(solve_part_two(foods))
