from collections import deque


def test_solve_part_one():
    input_ = "389125467"
    result = solve_part_one(input_, moves=10)
    assert result == "92658374", f"Expected 92658374, got {result}."
    result = solve_part_one(input_)
    assert result == "67384529", f"Expected 67384529, got {result}."


def test_solve_part_two():
    input_ = "389125467"
    result = solve_part_two(input_)
    assert result == 149245887792, f"Expected 149245887792, got {result}."


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


def solve_part_two(input_: str, moves: int = 10_000_000) -> int:
    max_in_input = max(int(n) for n in input_)
    max_len = 1_000_000
    labels = deque(
        range(max_in_input + 1, max_len + max_in_input + 1), maxlen=1_000_000
    )
    labels.extendleft(int(i) for i in input_[::-1])

    import time

    start = time.perf_counter()

    for _ in range(1, moves + 1):
        # print(f"\n-- move {_} --")
        labels = make_a_move(labels)

    index_of_1 = labels.index(1)
    result = labels[index_of_1 + 1] * labels[index_of_1 + 2]

    print(time.perf_counter() - start)  # ca. 7.5 s for 100 iterations o_O
    return result


if __name__ == "__main__":
    # test_solve_part_one()
    # print(solve_part_one("871369452"))
    # test_solve_part_two()
    print(solve_part_two("871369452", 100))
