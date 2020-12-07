import re
from typing import Callable, Dict, List

PASSPORT_RULES: Dict[str, List[Callable[[str], bool]]] = {
    "byr": [
        lambda x: len(x) == 4,
        lambda x: 1920 <= int(x) <= 2002,
    ],
    "iyr": [
        lambda x: len(x) == 4,
        lambda x: 2010 <= int(x) <= 2020,
    ],
    "eyr": [
        lambda x: len(x) == 4,
        lambda x: 2020 <= int(x) <= 2030,
    ],
    "hgt": [
        lambda x: bool(re.match(r"^\d+(cm|in)$", x)),
        lambda x: (x[-2:] == "cm" and 150 <= int(x[:-2]) <= 193)
                  or (x[-2:] == "in" and 59 <= int(x[:-2]) <= 76),
    ],
    "hcl": [
        lambda x: x.startswith("#"),
        lambda x: all(c.isdigit() or c in "abcdef" for c in x[1:]),
    ],
    "ecl": [
        lambda x: x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
    ],
    "pid": [
        lambda x: len(x) == 9,
        lambda x: x.isdigit(),
    ],
}


def keys_are_valid(doc: List[str]) -> bool:
    doc_keys = [item[:3] for item in doc]
    if all(field in doc_keys for field in PASSPORT_RULES.keys()):
        return True

    return False


def values_are_valid(doc: List[str]) -> bool:
    doc_fields: Dict[str, str] = dict([field.split(":") for field in doc])

    for required_field, validators in PASSPORT_RULES.items():
        value = doc_fields.get(required_field)
        if value is None:
            return False  # This document lacks some required fields.
        if not all(validate(value) for validate in validators):
            return False  # Some field has an invalid value.

    return True


def solve_part_one():
    valid_docs_count = 0
    for doc in DOCS:
        if keys_are_valid(doc):
            valid_docs_count += 1
    return valid_docs_count


def solve_part_two():
    valid_docs_count = 0
    for doc in DOCS:
        if values_are_valid(doc):
            valid_docs_count += 1
    return valid_docs_count


if __name__ == "__main__":
    with open("04_input") as f:
        DOCS = [doc.split() for doc in f.read().split("\n\n")]

    print(solve_part_one())
    print(solve_part_two())
