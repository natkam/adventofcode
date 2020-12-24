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


def find_tile_position(steps_seq: str) -> Tuple[int, int]:
    p = re.compile("^(sw|nw|se|ne|e|w)")
    position = (0, 0)
    while steps_seq:
        m = p.match(steps_seq)
        assert m is not None
        prefix = m.group(1)
        steps_seq = p.sub("", steps_seq)  # In Python 3.9 there's `.removeprefix`...
        position = (
            position[0] + STEPS_COORDS[prefix][0],
            position[1] + STEPS_COORDS[prefix][1],
        )
    return position


def init_black_tiles() -> Set[Tuple[int, int]]:
    with open("24_input") as f:
        instructions = f.read().splitlines()

    black_tiles: Set[Tuple[int, int]] = set()

    for steps_seq in instructions:
        position = find_tile_position(steps_seq)
        if position in black_tiles:
            black_tiles.remove(position)
        else:
            black_tiles.add(position)

    return black_tiles


def count_black_adjacent_tiles(
    tile: Tuple[int, int], black_tiles: Set[Tuple[int, int]]
) -> int:
    return len(
        [
            (tile[0] + x, tile[1] + y)
            for x, y in STEPS_COORDS.values()
            if (tile[0] + x, tile[1] + y) in black_tiles
        ]
    )


def get_white_adjacent_tiles(
    tile: Tuple[int, int], black_tiles: Set[Tuple[int, int]]
) -> Set[Tuple[int, int]]:
    return {
        (tile[0] + x, tile[1] + y)
        for x, y in STEPS_COORDS.values()
        if (tile[0] + x, tile[1] + y) not in black_tiles
    }


def get_black_tiles_next_day(black_tiles: Set[Tuple[int, int]]) -> Set[Tuple[int, int]]:
    next_black_tiles: Set[Tuple[int, int]] = set()

    for black_tile in black_tiles:
        black_adjacent_count = count_black_adjacent_tiles(black_tile, black_tiles)
        if black_adjacent_count in {1, 2}:
            next_black_tiles.add(black_tile)

        white_adjacent = get_white_adjacent_tiles(black_tile, black_tiles)
        for tile in white_adjacent:
            if count_black_adjacent_tiles(tile, black_tiles) == 2:
                next_black_tiles.add(tile)

    return next_black_tiles.copy()


def solve_part_one() -> int:
    black_tiles = init_black_tiles()
    return len(black_tiles)


def solve_part_two() -> int:
    black_tiles = init_black_tiles()
    for i in range(100):
        black_tiles = get_black_tiles_next_day(black_tiles)
    return len(black_tiles)


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
