import abc
import ast
from typing import Dict, List, Type, Union, cast


def test_solve_part_one():
    expressions = [
        ("1 + 2 * 3 + 4 * 5 + 6", 71),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 26),
        ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
        ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
        ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632),
    ]
    s = SolvesPartOne()

    for expression, expected in expressions:
        result = s.evaluate(expression)
        assert result == expected, f"Expected {expected}, got {result}"


def test_solve_part_two():
    expressions = [
        ("1 + 2 * 3 + 4 * 5 + 6", 231),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 46),
    ]
    s = SolvesPartTwo()

    for expression, expected in expressions:
        result = s.evaluate(expression)
        assert result == expected, f"Expected {expected}, got {result}"


class OperatorPrecedenceChanger(abc.ABC):
    swapped_signs: Dict[str, Union[int, str, None]]
    ops_to_swap: Dict[Type[ast.operator], Type[ast.operator]]

    def evaluate(self, expression: str) -> int:
        expr_swapped = expression.translate(str.maketrans(self.swapped_signs))
        p = ast.parse(expr_swapped)

        for node in ast.walk(p):
            if isinstance(node, ast.BinOp) and isinstance(
                node.op, tuple(self.ops_to_swap)
            ):
                node.op = self.ops_to_swap[type(node.op)]()

        expr_node = cast(ast.Expr, p.body[0])
        e = ast.Expression(expr_node.value)

        return eval(compile(e, "<string>", "eval"))


class SolvesPartOne(OperatorPrecedenceChanger):
    swapped_signs = {"*": "-"}
    ops_to_swap = {ast.Sub: ast.Mult}

# def solve_part_one(homework: List[str]) -> int:
#     # Replace '*' with '-' ('-' and '+' have equal precedence)
#     # Let Python create an ast tree
#     # Replace 'ast.Sub' with 'ast.Mult' (͡° ͜ʖ͡°)
#     # Compile, evaluate - voilà!
#
#     # swapped_signs: Dict[str, Union[int, str, None]] = {"*": "-"}
#     # ops_to_swap: Dict[Type[ast.operator], Type[ast.operator]] = {ast.Sub: ast.Mult}
#     # s = OperatorPrecedenceChanger(swapped_signs, ops_to_swap)
#     s = SolvesPartOne()
#
#     return sum(s.evaluate(expression) for expression in homework)


class SolvesPartTwo(OperatorPrecedenceChanger):
    swapped_signs = {"+": "*", "*": "+"}
    ops_to_swap = {ast.Add: ast.Mult, ast.Mult: ast.Add}


# def solve_part_two(homework: List[str]) -> int:
#     # swapped_signs: Dict[str, Union[int, str, None]] = {"+": "*", "*": "+"}
#     # ops_to_swap = {ast.Add: ast.Mult, ast.Mult: ast.Add}
#     # s = OperatorPrecedenceChanger(swapped_signs, ops_to_swap)
#     s = SolvesPartTwo()
#
#     return sum(s.evaluate(expression) for expression in homework)


def solve(s: OperatorPrecedenceChanger, homework: List[str]) -> int:
    return sum(s.evaluate(expression) for expression in homework)


if __name__ == "__main__":
    with open("18_input") as f:
        homework = f.read().splitlines()

    # test_solve_part_one()
    # print(solve_part_one(homework))
    # test_solve_part_two()
    # print(solve_part_two(homework))
    print(solve(SolvesPartOne(), homework))
    print(solve(SolvesPartTwo(), homework))
