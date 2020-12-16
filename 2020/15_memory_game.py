from typing import List


def test_solve_part_one():
    starting_numbers = [int(n) for n in "0,3,6".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 436, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "1,3,2".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 1, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "2,1,3".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 10, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "1,2,3".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 27, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "2,3,1".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 78, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "3,2,1".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 438, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "3,1,2".split(",")]
    solution = solve(starting_numbers, 2020)
    assert solution == 1836, f"Solution: {solution}"


def test_solve_part_two():
    starting_numbers = [int(n) for n in "0,3,6".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 175594, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "1,3,2".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 2578, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "2,1,3".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 3544142, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "1,2,3".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 261214, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "2,3,1".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 6895259, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "3,2,1".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 18, f"Solution: {solution}"

    starting_numbers = [int(n) for n in "3,1,2".split(",")]
    solution = solve(starting_numbers, 30_000_000)
    assert solution == 362, f"Solution: {solution}"


def solve(starting_numbers: List[int], stop_index: int) -> int:
    counter = dict()

    for i, number in enumerate(starting_numbers, start=1):
        counter[number] = i

    prev_number = number
    for i in range(i + 1, stop_index + 1):
        if prev_number not in counter:
            number = 0
        else:
            number = (i - 1) - counter[prev_number]
        counter[prev_number] = i - 1
        prev_number = number

    return number


if __name__ == "__main__":
    starting_numbers = [int(n) for n in "0,14,1,3,7,9".split(",")]

    # test_solve_part_one()
    # test_solve_part_two()
    print(solve(starting_numbers, stop_index=2020))  # Part 1
    print(solve(starting_numbers, stop_index=30_000_000))  # Part 2
