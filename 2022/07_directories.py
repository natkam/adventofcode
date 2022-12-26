import uuid
from typing import Dict

dir_sizes: Dict[str, int] = {}
# sum_below_threshold: int = 0


def _get_dir_size(data):
    global dir_sizes
    i = 0
    size = 0
    while True:
        try:
            line = data[i]
        except IndexError:
            break
        match line.split():
            case ["$", "cd", ".."]:
                i += 1
                return i, size
            case ["$", "cd", subdir]:
                if subdir in dir_sizes:
                    # There are multiple subdirs with the same name, so just
                    # adding their size to the dict doesn't work - some of them
                    # would be overwritten! We add unique uuid to the name:
                    subdir = f"{subdir}_{uuid.uuid4()}"

                jump, sub_size = _get_dir_size(data[i + 1 :])
                dir_sizes[subdir] = sub_size
                # if sub_size < 100_000:
                #     sum_below_threshold += sub_size
                size += sub_size
                i += jump
            case ["$", "ls"]:
                pass
            case ["dir", subdir]:
                pass
            case [file_size, file_name]:
                size += int(file_size)
        i += 1

    return i, size


def part_one():
    with open("07_input") as f:
        data = f.read().splitlines()

    _get_dir_size(data)
    threshold = 100_000
    sum_below_threshold = sum(size for size in dir_sizes.values() if size <= threshold)

    return sum_below_threshold


def part_two():
    disk_size = 70_000_000
    necessary_free_space = 30_000_000

    global dir_sizes
    total_size = dir_sizes["/"]  # 48_044_502
    min_to_release = necessary_free_space - (disk_size - total_size)

    return min(v for v in dir_sizes.values() if v >= min_to_release)


if __name__ == "__main__":
    print(part_one())
    print(part_two())
