def test_solve_part_one():
    card_pubkey, door_pubkey = 5764801, 17807724
    result = solve_part_one(card_pubkey, door_pubkey)
    assert result == 14897079, f"Expected 14897079, got {result}"


def solve_part_one(card_pubkey: int, door_pubkey: int) -> int:
    subject_number = 7
    value = 1
    divisor = 20201227
    loop_size = 0

    card_loop_size = door_loop_size = 0

    while True:
        loop_size += 1
        value *= subject_number
        value %= divisor
        if value == card_pubkey:
            card_loop_size = loop_size
        if value == door_pubkey:
            door_loop_size = loop_size
        if card_loop_size and door_loop_size:
            break

    new_loop_size = min(card_loop_size, door_loop_size)
    subject_number = value
    value = 1
    for _ in range(new_loop_size):
        value *= subject_number
        value %= divisor

    return value


if __name__ == "__main__":
    with open("25_input") as f:
        card_pubkey, door_pubkey = [int(n) for n in f.read().splitlines()]

    # test_solve_part_one()
    print(solve_part_one(card_pubkey, door_pubkey))
