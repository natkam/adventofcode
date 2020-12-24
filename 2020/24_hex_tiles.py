import re
from typing import Set, Tuple

STEPS_COORDS = {
        "e": (1, 0),
        "se": (0, -1),
        "sw": (-1, -1),
        "w": (-1, 0),
        "nw": (0, 1),
        "ne": (1, 1),
    }


def get_black_tiles_after_day_1() -> Set[Tuple[int, int]]:
    with open("24_input") as f:
        instructions = f.read().splitlines()

    p = re.compile("^(sw|nw|se|ne|e|w)(?:.*)")
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
                position[0] + STEPS_COORDS[prefix][0],
                position[1] + STEPS_COORDS[prefix][1],
            )

        if position in black_tiles:
            black_tiles.remove(position)
        else:
            black_tiles.add(position)

    return black_tiles


def get_black_neighbours_count(tile: Tuple[int, int], black_tiles) -> int:
    neighbours = [(tile[0] + x, tile[1] + y) for x, y in STEPS_COORDS.values()]
    return len([n for n in neighbours if n in black_tiles])


def solve_part_one() -> int:
    black_tiles = get_black_tiles_after_day_1()
    return len(black_tiles)


def solve_part_two() -> int:
    black_tiles = get_black_tiles_after_day_1()
    next_black_tiles: Set[Tuple[int, int]] = set()

    for i in range(100):
        for black_tile in black_tiles:
            black_count = get_black_neighbours_count(black_tile, black_tiles)
            if black_count in {1, 2}:
                next_black_tiles.add(black_tile)

            neighbours = [(black_tile[0] + x, black_tile[1] + y) for x, y in STEPS_COORDS.values()]
            for neighbour in neighbours:
                if neighbour not in black_tiles and get_black_neighbours_count(neighbour, black_tiles) == 2:
                    next_black_tiles.add(neighbour)

        black_tiles = next_black_tiles.copy()
        next_black_tiles = set()

    return len(black_tiles)

if __name__ == "__main__":
    # print(solve_part_one())
    print(solve_part_two())
