import functools
import operator


def solve_part_one():
    with open("06_input") as f:
        declarations = [
            len(set("".join(decl.split()))) for decl in f.read().split("\n\n")
        ]

    return sum(declarations)


def solve_part_two():
    with open("06_input") as f:
        declarations = [decl.split() for decl in f.read().split("\n\n")]

    common_answers = 0

    for group in declarations:
        for answer in group[0]:
            # all([]) is True!
            common_answers += 1 if all(answer in person for person in group[1:]) else 0

    return common_answers


def solve_part_two_2():
    with open("06_input") as f:
        declarations = [decl.split() for decl in f.read().split("\n\n")]

    common_answers = 0

    for group in declarations:
        group_s = [set(person) for person in group]
        common_answers += len(functools.reduce(operator.and_, group_s))

    return common_answers


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
    print(solve_part_two_2())
