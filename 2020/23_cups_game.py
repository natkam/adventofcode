from collections import deque
from typing import List, Tuple


def test_solve_part_one():
    input_ = "32415"
    result = solve_part_one(input_, moves=10)
    assert result == "92658374", f"Expected 92658374, got {result}."
    result = solve_part_one(input_)
    assert result == "67384529", f"Expected 67384529, got {result}."


def make_a_move(labels: List[int], current_cup: int) -> Tuple[List[int], int]:
    ...


def solve_part_one(input_: str, moves: int = 100) -> str:
    labels = deque([int(i) for i in input_], maxlen=len(input_))
    current_cup = deque[0]  # maybe this is not needed, just rotate the deque?

    for _ in range(moves):
        print(f"-- move {_} --")
        labels, current_cup = make_a_move(labels, current_cup)

    labels.rotate(-labels.index(1))
    labels.popleft()

    return "".join(str(i) for i in labels)


if __name__ == '__main__':
    print(solve_part_one("871369452"))
