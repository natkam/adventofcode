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
    for expression, expected in expressions:
        result = SolvesPartOne.evaluate(expression)
        assert result == expected, f"Expected {expected}, got {result}"


def test_solve_part_two():
    expressions = [
        ("1 + 2 * 3 + 4 * 5 + 6", 231),
        ("1 + (2 * 3) + (4 * (5 + 6))", 51),
        ("2 * 3 + (4 * 5)", 46),
    ]
    for expression, expected in expressions:
        result = SolvesPartTwo.evaluate(expression)
        assert result == expected, f"Expected {expected}, got {result}"


class OperatorPrecedenceChanger(abc.ABC):
    swapped_signs: Dict[str, Union[int, str, None]]
    ops_to_swap: Dict[Type[ast.operator], Type[ast.operator]]

    def __init__(self, homework: List[str]):
        self.homework = homework

    @classmethod
    def evaluate(cls, expression: str) -> int:
        """Evaluates the expression string with overridden operator precedence.

        How the operator precedence is overriden:
        1. Replace the operator signs in `expressions` according to `cls.swapped_signs`
        translation table.
        2. Let Python build an AST, in compliance with overriden operator precedences.
        3. Traverses the tree, replacing operator instances back to what they would be
        if the original `expression` was parsed.
        4. Compiles the expression and evaluates it, returning an integer (the result).
        """
        expr_swapped = expression.translate(str.maketrans(cls.swapped_signs))
        p = ast.parse(expr_swapped)

        for node in ast.walk(p):
            if isinstance(node, ast.BinOp) and isinstance(
                node.op, tuple(cls.ops_to_swap)
            ):
                node.op = cls.ops_to_swap[type(node.op)]()

        expr_node = cast(ast.Expr, p.body[0])
        e = ast.Expression(expr_node.value)

        return eval(compile(e, "<string>", "eval"))

    def solve(self) -> int:
        return sum(self.evaluate(expression) for expression in self.homework)


class SolvesPartOne(OperatorPrecedenceChanger):
    swapped_signs = {"*": "-"}
    ops_to_swap = {ast.Sub: ast.Mult}


class SolvesPartTwo(OperatorPrecedenceChanger):
    swapped_signs = {"+": "*", "*": "+"}
    ops_to_swap = {ast.Add: ast.Mult, ast.Mult: ast.Add}


if __name__ == "__main__":
    with open("18_input") as f:
        homework = f.read().splitlines()

    test_solve_part_one()
    test_solve_part_two()
    print(SolvesPartOne(homework).solve())
    print(SolvesPartTwo(homework).solve())
