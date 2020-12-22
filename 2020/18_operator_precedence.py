import ast
from typing import List


def test_solve_part_one():
    expressions = [
        ("1 + 2 * 3 + 4 * 5 + 6", 71),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 26),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632),
    ]

    for expression, expected in expressions:
        result = evaluate(expression)
        assert result == expected, f"Expected {expected}, got {result}"


def evaluate(expression: str) -> int:
    # Replace '*' with '-' ('-' and '+' have equal precedence)
    # Let Python create an ast tree
    # Replace 'ast.Sub' with 'ast.Mult' (͡° ͜ʖ͡°)
    # Compile, evaluate - voilà!

    expr_sub = expression.replace("*", "-")
    p = ast.parse(expr_sub)

    for node in ast.walk(p):
        if hasattr(node, 'body'):
            node = node.body[0].value
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Sub):
            node.op = ast.Mult()

    e = ast.Expression(p.body[0].value)

    return eval(compile(e, "<string>", "eval"))


def solve_part_one(homework: List[str]) -> int:
    results_sum = 0
    for expression in homework:
        results_sum += evaluate(expression)

    return results_sum


if __name__ == "__main__":
    with open("18_input") as f:
        homework = f.read().splitlines()

    print(solve_part_one(homework))
