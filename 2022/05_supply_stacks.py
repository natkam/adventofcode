from typing import List, Tuple


def _parse(data: List[str]) -> Tuple[List[str], List[int]]:
    stacks = []
    moves = []
    parsing_stacks = True
    for line in data:
        if line == "\n":
            parsing_stacks = False
            continue
        if parsing_stacks:
            c = 0
            while col := line[4 * c : 4 * (c + 1)]:
                # Prepare a list of crates in the column, if it's not there yet.
                try:
                    stacks[c]
                except IndexError:
                    stacks.append([])
                # Leading columns may be empty (all spaces), so we don't break out of
                # the loop but continue until the end of the line.
                col = col.strip("[] \n")
                if not col:
                    c += 1
                    continue
                # One line contains only column numbers, we don't need it.
                if col.isdigit():
                    # print("Number of columns:", line.strip()[-1])
                    break
                # print(f"{c}: {col!r}")

                stacks[c].append(col)
                c += 1
        else:
            move = line.split()
            count, fr, to = int(move[1]), int(move[3]), int(move[5])
            moves.append([count, fr, to])

    # Keep the top items at the end of the list (easier to pop and append!)
    for stack in stacks:
        stack.reverse()
    return stacks, moves


def _move_one(stacks, how_many, fr, to):
    for crate in range(how_many):
        # Col. numbers in the instruction start with 1 => hence -1 in indexes.
        stacks[to - 1].append(stacks[fr - 1].pop())


def _move_two(stacks, how_many, fr, to):
    # Col. numbers in the instruction start with 1 => hence -1 in indexes.
    moved_crates = stacks[fr - 1][-how_many:]
    stacks[fr - 1][-how_many:] = []
    stacks[to - 1].extend(moved_crates)


def part_one():
    with open("05_input") as f:
        data = f.readlines()

    stacks, moves = _parse(data)
    for move in moves:
        _move_one(stacks, *move)

    return "".join(s.pop() for s in stacks)


def part_two():
    with open("05_input") as f:
        data = f.readlines()

    stacks, moves = _parse(data)
    for move in moves:
        _move_two(stacks, *move)

    return "".join(s.pop() for s in stacks)


if __name__ == "__main__":
    print(part_one())
    print(part_two())
