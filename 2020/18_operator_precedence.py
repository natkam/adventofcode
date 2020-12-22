import ast
from typing import List



def evaluate(line: str) -> int:
    # Replace '*' with '-' ('-' and '+' have equal precedence)
    # Let Python create an ast tree
    # Replace 'ast.Sub' with 'ast.Mult' (͡° ͜ʖ͡°)
    #  ast.walk? or use a NodeTransformer?
    # Compile, evaluate
    line = line.replace("*", "-")
    p = ast.parse(line)
    ...


def solve_part_one(homework: List[str]) -> int:
    results_sum = 0
    for expression in homework:
        results_sum += evaluate(expression)

    return results_sum


if __name__ == "__main__":
    with open("18_input") as f:
        homework = f.read().splitlines()

    print(solve_part_one(homework))
