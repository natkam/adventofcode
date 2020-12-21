import operator
from collections import defaultdict
from functools import reduce
from typing import Dict, List, Set, Tuple


def simplify_ingredients(allergens: defaultdict) -> Dict[str, List[str]]:
    simplified = dict()
    for a, ing_sets in allergens.items():
        common_ing = reduce(operator.and_, ing_sets)
        if len(common_ing) == 0:
            raise ValueError(f"Oops! No common elements found for {a}!")
        simplified[a] = common_ing

    return simplified


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
    allergens = defaultdict(list)
    foods_parsed: List[Tuple[Set[str], List[str]]] = []

    for line in foods:
        ing_str, allerg_str = line.rstrip(")").split(" (contains ")
        ing = set(ing_str.split())
        allerg = allerg_str.split(", ")
        foods_parsed.append((ing, allerg))
        for a in allerg:
            allergens[a].append(ing)

    identified = get_allergen_ingredients(allergens)
    dangerous_ingredients = set(identified.values())

    safe_ings_cout = 0
    for ings, _ in foods_parsed:
        safe_ings_cout += len(ings - dangerous_ingredients)

    return safe_ings_cout


if __name__ == "__main__":
    with open("21_input") as f:
        foods = f.read().splitlines()

    print(solve_part_one(foods))
