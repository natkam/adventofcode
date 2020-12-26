from collections import deque
from typing import List


def test_solve_part_one():
    input_ = "389125467"
    result = solve_part_one(input_, moves=10)
    assert result == "92658374", f"Expected 92658374, got {result}."
    result = solve_part_one(input_)
    assert result == "67384529", f"Expected 67384529, got {result}."


def test_solve_part_two():
    input_ = "389125467"
    result = solve_part_two(input_, moves=1000)
    assert result == 149245887792, f"Expected 149245887792, got {result}."


def get_destination(current_cup: int, pick_up: List[int], labels_max: int) -> int:
    pick_up_set = set(pick_up)
    for i in range(1, 4):
        if current_cup - i <= 0:
            break
        if current_cup - i not in pick_up_set:
            return current_cup - i

    max_in_labels = max({labels_max - i for i in range(4)} - pick_up_set)
    return max_in_labels


def make_a_move(labels: deque) -> deque:
    current_cup = labels.popleft()
    pick_up = [labels.popleft() for _ in range(3)]

    # `deque.index` calls (one for each move) take up >99% of execution time.
    destination_index = labels.index(
        get_destination(current_cup, pick_up, labels.maxlen)
    )

    labels.appendleft(current_cup)
    labels.rotate(-(destination_index + 2))  # Destination is at the end now.
    labels.extend(pick_up)

    # labels.rotate(-(labels.index(current_cup) + 1))
    labels.rotate(destination_index + 4)  # Doesn't make execution any faster. How come?

    return labels


def solve_part_one(input_: str, moves: int = 100) -> str:
    labels = deque([int(i) for i in input_], maxlen=len(input_))

    for _ in range(1, moves + 1):
        labels = make_a_move(labels)

    labels.rotate(-labels.index(1))
    labels.popleft()

    return "".join(str(i) for i in labels)


def solve_part_two(input_: str, moves: int = 10_000_000) -> int:
    max_in_input = max(int(n) for n in input_)
    max_len = 1_000_000
    labels = deque(range(max_in_input + 1, max_len + max_in_input + 1), maxlen=max_len)
    labels.extendleft(int(i) for i in input_[::-1])

    import time

    start = time.perf_counter()

    for _ in range(1, moves + 1):
        labels = make_a_move(labels)

    index_of_1 = labels.index(1)
    next_after_1, next_after_2 = labels[index_of_1 + 1], labels[index_of_1 + 2]
    print(next_after_1, next_after_2)
    result = next_after_1 * next_after_2

    print(time.perf_counter() - start)  # ca. 2.2 s for 100 iterations o_O
    return result


if __name__ == "__main__":
    # test_solve_part_one()
    # print(solve_part_one("871369452"))
    test_solve_part_two()
    # print(solve_part_two("871369452", 2000))
