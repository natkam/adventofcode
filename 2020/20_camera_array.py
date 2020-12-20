from typing import Dict, List, Optional, Tuple

import numpy as np


class Tile:
    def __init__(
        self, id_: int, data: np.ndarray, coords: Optional[Tuple[int, int]] = None
    ):
        self.id = id_
        self.data = data
        self.coords = coords

    def get_neighbours(self) -> List[Tuple[int, int]]:
        assert self.coords is not None
        y, x = self.coords
        neighbours = [(y, x + 1), (y + 1, x)]
        if x > 0:
            neighbours.append((y, x - 1))
        if y > 0:
            neighbours.append((y - 1, x))
        return neighbours

    def __str__(self):
        return f"Tile id={self.id}"

    def __repr__(self):
        return str(self)


class Solution:
    def __init__(self, img_side: int, start_tile: Tile):
        img_shape = (
            (img_side + 1) * 2 + 1,
            (img_side + 1) * 2 + 1,
        )  # Each tile is 10x10; added extra space on the edges to avoid IndexErrors.
        self.img = np.empty((*img_shape, 10, 10), dtype=str)
        self.tile_ids = np.zeros(img_shape, dtype=int)

        img_middle = (img_side + 1, img_side + 1)
        self.img[img_middle] = start_tile.data
        start_tile.coords = img_middle
        self.tile_ids[img_middle] = start_tile.id

    # TODO: define __getattr__ etc.? Anyway we only want to manipulate `self.img`.

    def matches(self, slot: Tuple[int, int], tile: Tile) -> bool:
        y, x = slot
        right_tile = self.img[y, x + 1]
        right_tile_id = self.tile_ids[y, x + 1]
        bottom_tile = self.img[y + 1, x]
        bottom_tile_id = self.tile_ids[y + 1, x]
        left_tile = self.img[y, x - 1]
        left_tile_id = self.tile_ids[y, x - 1]
        up_tile = self.img[y - 1, x]
        up_tile_id = self.tile_ids[y - 1, x]

        if right_tile_id != 0 and not (tile.data[:, -1] == right_tile[:, 0]).all():
            return False
        if bottom_tile_id != 0 and not (tile.data[-1, :] == bottom_tile[0, :]).all():
            return False
        if left_tile_id != 0 and not (tile.data[:, 0] == left_tile[:, -1]).all():
            return False
        if up_tile_id != 0 and not (tile.data[0, :] == up_tile[-1, :]).all():
            return False

        return True

    def get_corner_tiles(self):
        min_y, min_x = np.min(np.argwhere(self.tile_ids), axis=0)
        max_y, max_x = np.max(np.argwhere(self.tile_ids), axis=0)

        return (
            self.tile_ids[min_y, min_x]
            * self.tile_ids[min_y, max_x]
            * self.tile_ids[max_y, min_x]
            * self.tile_ids[max_y, max_x]
        )


def get_input_from_file() -> List[Tile]:
    with open("20_input") as f:
        tiles = [tile.split("\n") for tile in f.read().split("\n\n")]

    tiles_dict = {
        int(tile[0][5:9]): np.array([[c for c in row] for row in tile[1:]])
        for tile in tiles
    }
    return [Tile(id_, data) for id_, data in tiles_dict.items()]


def assemble(tiles):
    img_side = int(len(tiles) ** 0.5)
    start_tile = tiles.pop()
    sol = Solution(img_side, start_tile)
    assembled_tiles = [start_tile]
    while assembled_tiles:
        added_tile = assembled_tiles.pop()

        for slot in added_tile.get_neighbours():
            checked_tiles = []

            while tiles:
                tile = tiles.pop()
                for i in range(8):
                    if i == 4:  # Transpose the array after the 4th rotation.
                        tile.data = tile.data.T
                    tile.data = np.rot90(tile.data)
                    if sol.matches(slot, tile):
                        sol.img[slot] = tile.data
                        sol.tile_ids[slot] = tile.id
                        tile.coords = slot
                        assembled_tiles.append(tile)
                        break
                else:
                    checked_tiles.append(tile)
                    continue

                break

            tiles = tiles + checked_tiles
    return sol


def solve_part_one():
    tiles = get_input_from_file()

    sol = assemble(tiles)

    # print(sol.tile_ids)
    return sol.get_corner_tiles()


if __name__ == "__main__":
    print(solve_part_one())
