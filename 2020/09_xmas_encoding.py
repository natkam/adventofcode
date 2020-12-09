from itertools import combinations


def solve_part_one() -> int:
    proper_data = numbers[25:]

    for i, number in enumerate(proper_data):
        possible_sums = set(a + b for a, b in combinations(numbers[i : i + 25], 2))
        if number not in possible_sums:
            return number

    raise RuntimeError("Haven't found any invalid numbers, something must be wrong!")


def solve_part_two(goal: int) -> int:
    for start_index, start_number in enumerate(numbers):
        contiguous_sum = start_number
        end_index = start_index
        while contiguous_sum < goal:
            end_index += 1
            contiguous_sum += numbers[end_index]
            if contiguous_sum == goal:
                return start_number + max(numbers[start_index : end_index + 1])

    raise RuntimeError(
        "Haven't found the right set of numbers, something must be wrong!"
    )


if __name__ == "__main__":
    with open("09_input") as f:
        numbers = [int(n) for n in f.read().splitlines()]

    invalid_number = solve_part_one()
    print(invalid_number)
    print(solve_part_two(invalid_number))
