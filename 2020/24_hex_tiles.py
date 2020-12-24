import re
from typing import Set, Tuple

if __name__ == '__main__':
    with open("24_input") as f:
        lines = f.read().splitlines()

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

    for steps in lines:
        position = [ref_tile[0], ref_tile[1]]
        while steps:
            prefix = p.match(steps).group(1)
            steps = steps[len(prefix):]  # In Python 3.9 there's `.removeprefix`...
            position[0] += steps_coords[prefix][0]
            position[1] += steps_coords[prefix][1]

        if tuple(position) in black_tiles:
            # breakpoint()
            black_tiles.remove(tuple(position))
        else:
            black_tiles.add(tuple(position))

    print(len(black_tiles))
