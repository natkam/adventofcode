from typing import List


def _get_calories_per_elf(data: List[str]) -> List[int]:
    calories = []
    one_elf = 0

    for i, food_item in enumerate(data):
        if food_item == "":
            calories.append(one_elf)
            one_elf = 0
            continue
        one_elf += int(food_item)

    calories.append(one_elf)

    return calories


def part_one():
    with open("01_input") as f:
        data = f.read().splitlines()

    calories = _get_calories_per_elf(data)

    return max(calories)

def part_two():
    with open("01_input") as f:
        data = f.read().splitlines()

    calories = _get_calories_per_elf(data)

    return sum(sorted(calories)[-3:])



if __name__ == "__main__":
    print(part_one())
    print(part_two())
