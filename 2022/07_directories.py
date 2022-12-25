from typing import Dict

dir_sizes: Dict[str, int] = {}
sum_below_threshold: int = 0


def _get_dir_size(data):
    global dir_sizes, sum_below_threshold
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
                jump, sub_size = _get_dir_size(data[i + 1 :])
                # Caution: there are multiple subdirs with the same name, so
                # just adding their size to the dict doesn't work - some of them
                # are overwritten!
                # dir_sizes[subdir] = sub_size
                if sub_size < 100_000:
                    sum_below_threshold += sub_size
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

    _, total_size = _get_dir_size(data)
    # total_size_below_threshold = sum(size for size in dir_sizes.values() if size <= 100_000)

    return sum_below_threshold


def part_two():
    with open("07_input_test") as f:
        data = f.read().splitlines()


if __name__ == "__main__":
    print(part_one())
    print(part_two())
