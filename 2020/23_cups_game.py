from collections import deque
from typing import List, Tuple


def test_solve_part_one():
    input_ = "389125467"
    result = solve_part_one(input_, moves=10)
    assert result == "92658374", f"Expected 92658374, got {result}."
    result = solve_part_one(input_)
    assert result == "67384529", f"Expected 67384529, got {result}."


def make_a_move(labels: deque) -> deque:
    current_cup = labels.popleft()
    # print(f"cups: {labels} (current: {current_cup})")
    pick_up = [labels.popleft() for _ in range(3)]
    # print(f"pick up: {pick_up}")
    destination = max([n for n in labels if n < current_cup] or labels)
    # print(f"destination: {destination}")
    labels.appendleft(current_cup)
    labels.rotate(-(labels.index(destination) + 1))  # Destination is at the end now.
    labels.extend(pick_up)

    labels.rotate(-(labels.index(current_cup) + 1))

    return labels


def solve_part_one(input_: str, moves: int = 100) -> str:
    labels = deque([int(i) for i in input_], maxlen=len(input_))

    for _ in range(1, moves + 1):
        # print(f"\n-- move {_} --")
        labels = make_a_move(labels)

    labels.rotate(-labels.index(1))
    labels.popleft()

    return "".join(str(i) for i in labels)


# def solve_part_two(input_: str, moves: int = 100) -> str:
#     labels = deque([int(i) for i in input_], maxlen=len(input_))


if __name__ == '__main__':
    test_solve_part_one()
    print(solve_part_one("871369452"))
