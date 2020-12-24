import re
from typing import Set, Tuple


def solve_part_one() -> int:
    with open("24_input") as f:
        instructions = f.read().splitlines()

    p = re.compile("^(sw|nw|se|ne|e|w)(?:.*)")

    steps_coords = {
        "e": (1, 0),
        "se": (0, -1),
        "sw": (-1, -1),
        "w": (-1, 0),
        "nw": (0, 1),
        "ne": (1, 1),
    }
    ref_tile = (0, 0)
    black_tiles: Set[Tuple[int, int]] = set()

    for steps_seq in instructions:
        position = ref_tile
        while steps_seq:
            m = p.match(steps_seq)
            assert m is not None
            prefix = m.group(1)
            steps_seq = steps_seq[len(prefix) :]  # In Python 3.9 there's `.removeprefix`...
            position = (
                position[0] + steps_coords[prefix][0],
                position[1] + steps_coords[prefix][1],
            )

        if position in black_tiles:
            black_tiles.remove(position)
        else:
            black_tiles.add(position)

    return len(black_tiles)


if __name__ == "__main__":
    print(solve_part_one())
