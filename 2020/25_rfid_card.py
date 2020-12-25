from typing import Tuple

DIVISOR = 20201227


def test_solve_part_one():
    card_pubkey, door_pubkey = 5764801, 17807724
    result = solve_part_one(card_pubkey, door_pubkey)
    assert result == 14897079, f"Expected 14897079, got {result}"


def transform(value: int, subject_number: int = 7) -> int:
    value *= subject_number
    return value % DIVISOR


def find_loop_sizes(card_pubkey: int, door_pubkey: int) -> Tuple[int, int, int]:
    value = 1
    loop_size = 0
    card_loop_size = door_loop_size = 0

    while True:
        loop_size += 1
        value = transform(value)
        if value == card_pubkey:
            card_loop_size = loop_size
        if value == door_pubkey:
            door_loop_size = loop_size
        if card_loop_size and door_loop_size:
            return card_loop_size, door_loop_size, value


def solve_part_one(card_pubkey: int, door_pubkey: int) -> int:
    card_loop_size, door_loop_size, value = find_loop_sizes(card_pubkey, door_pubkey)

    new_subject_number = value
    value = 1
    for _ in range(min(card_loop_size, door_loop_size)):
        value = transform(value, new_subject_number)

    return value


if __name__ == "__main__":
    with open("25_input") as f:
        card_pubkey, door_pubkey = [int(n) for n in f.read().splitlines()]

    # test_solve_part_one()
    print(solve_part_one(card_pubkey, door_pubkey))
