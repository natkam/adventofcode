import functools
import operator


def count_trees(step_right: int, step_down: int = 1) -> int:
    trees_count = 0
    x = 0
    for line in FOREST_MAP[::step_down]:
        if line[x] == "#":
            trees_count += 1
        x = (x + step_right) % LINE_LEN

    return trees_count


def solve_part_one():
    # move along the line: right 3, down 1
    return count_trees(step_right=3)


def solve_part_two():
    trees_counts = {
        (1, 1): None,
        (3, 1): None,
        (5, 1): None,
        (7, 1): None,
        (1, 2): None,
    }
    for slope in trees_counts.keys():
        trees_counts[slope] = count_trees(*slope)

    return functools.reduce(operator.mul, trees_counts.values())


if __name__ == "__main__":
    with open("03_input") as f:
        FOREST_MAP = f.read().splitlines()

    LINE_LEN = len(FOREST_MAP[0])
    print(solve_part_one())
    print(solve_part_two())
