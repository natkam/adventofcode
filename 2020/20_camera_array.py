from typing import List, Optional, Tuple

import numpy as np  # type: ignore


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
        return str(self)  # For debugging only, so whatever


class Image:
    def __init__(self, img_side: int):
        img_shape = (
            (img_side) * 2 - 1,
            (img_side) * 2 - 1,
        )  # Each tile is 10x10; added extra space on the edges to avoid IndexErrors.
        self._img = np.empty((*img_shape, 10, 10), dtype=str)
        self._tile_ids = np.zeros(img_shape, dtype=int)
        self._img_middle = (img_side - 1, img_side - 1)
        self.assembled_tiles: List[Tile] = []

    # TODO: define __getitem__ etc.? Anyway we only want to manipulate `self._img`.

    def assemble(self, tiles) -> None:
        start_tile = tiles.pop()
        self._set_tile(self._img_middle, start_tile)

        while self.assembled_tiles:
            assembled_tile = self.assembled_tiles.pop()

            for slot in assembled_tile.get_neighbours():
                checked_tiles = []

                while tiles:
                    tile = tiles.pop()
                    matched = self._try_to_match_tile(slot, tile)
                    if matched:
                        break
                    checked_tiles.append(tile)

                tiles += checked_tiles

    def _set_tile(self, slot: Tuple[int, int], tile: Tile) -> None:
        self._img[slot] = tile.data
        self._tile_ids[slot] = tile.id
        tile.coords = slot
        self.assembled_tiles.append(tile)

    def _try_to_match_tile(self, slot: Tuple[int, int], tile: Tile) -> bool:
        for i in range(8):
            if i == 4:  # Transpose the array after the 4th rotation.
                tile.data = tile.data.T
            tile.data = np.rot90(tile.data)
            if self._matches(slot, tile):
                self._set_tile(slot, tile)
                return True

        return False

    def _matches(self, slot: Tuple[int, int], tile: Tile) -> bool:
        y, x = slot
        neighbour_coords = {
            "right": (y, x + 1),
            "bottom": (y + 1, x),
            "left": (y, x - 1),
            "up": (y - 1, x),
        }
        edges = {
            "right": (np.s_[:, -1], np.s_[:, 0]),
            "bottom": (np.s_[-1, :], np.s_[0, :]),
            "left": (np.s_[:, 0], np.s_[:, -1]),
            "up": (np.s_[0, :], np.s_[-1, :]),
        }
        neighbour_tiles = dict()
        ids = dict()
        edges_match = dict()
        for side, coords in neighbour_coords.items():
            try:
                neighbour_tiles[side] = self._img[coords]
            except IndexError:
                continue
            else:
                ids[side] = self._tile_ids[coords]
                edges_match[side] = (
                    tile.data[edges[side][0]] == neighbour_tiles[side][edges[side][1]]
                ).all()

        for side in neighbour_tiles.keys():
            if ids[side] != 0 and not edges_match[side]:
                return False

        return True

    def get_corners_product(self):
        min_y, min_x = np.min(np.argwhere(self._tile_ids), axis=0)
        max_y, max_x = np.max(np.argwhere(self._tile_ids), axis=0)

        return (
            self._tile_ids[min_y, min_x]
            * self._tile_ids[min_y, max_x]
            * self._tile_ids[max_y, min_x]
            * self._tile_ids[max_y, max_x]
        )

    def crop_borders(self):
        # Crop the empty slots:
        self._tile_ids = self._tile_ids[np.any(self._tile_ids != 0, axis=1), :]
        self._tile_ids = self._tile_ids.T[np.any(self._tile_ids != 0, axis=0), :].T
        # TODO: Do the same in the actual _img array

        img_without_borders = self._img[:, :, 1:-1, 1:-1]
        self.img = np.block([[col for col in row] for row in img_without_borders])


def get_input_from_file() -> List[Tile]:
    with open("20_test_input") as f:
        tiles = [tile.split("\n") for tile in f.read().split("\n\n")]

    tiles_dict = {
        int(tile[0][5:9]): np.array([[c for c in row] for row in tile[1:]])
        for tile in tiles
    }
    return [Tile(id_, data) for id_, data in tiles_dict.items()]


def solve_part_one() -> int:
    tiles = get_input_from_file()

    img_side = int(len(tiles) ** 0.5)
    solution = Image(img_side)
    solution.assemble(tiles)
    # print(solution._tile_ids)

    return solution.get_corners_product()


def solve_part_two() -> int:
    tiles = get_input_from_file()
    img_side = int(len(tiles) ** 0.5)
    solution = Image(img_side)
    solution.assemble(tiles)

    solution.crop_borders()
    # print(solution._tile_ids)
    breakpoint()


if __name__ == "__main__":
    # print(solve_part_one())
    print(solve_part_two())
