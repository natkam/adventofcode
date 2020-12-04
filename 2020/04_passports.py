import re
from typing import List


def keys_are_valid(doc: List[str]) -> bool:
    required_fields = [
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid",
    ]
    doc_keys = [item[:3] for item in doc]
    if all(field in doc_keys for field in required_fields):
        return True
    
    return False


def solve_part_one():
    valid_docs_count = 0
    for doc in DOCS:
        if keys_are_valid(doc):
            valid_docs_count += 1
    return valid_docs_count


def solve_part_two():
    validators = {
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
            lambda x: re.match(r"^\d+(cm|in)$", x),
            lambda x: (x[-2:] == "cm" and 150 <= int(x[:-2]) <= 193)
            or (x[-2:] == "in" and 59 <= int(x[:-2]) <= 76),
        ],
        "hcl": [
            lambda x: x.startswith("#")
            and all(c.isdigit() or c in "abcdef" for c in x[1:]),
        ],
        "ecl": [
            lambda x: x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        ],
        "pid": [
            lambda x: len(x) == 9 and x.isdigit(),
        ],
        "cid": [],
    }

    valid_docs_count = 0

    for doc in DOCS:
        if keys_are_valid(doc):
            doc_fields = [field.split(":") for field in doc]
            for field, value in doc_fields:
                if not all(validate(value) for validate in validators[field]):
                    break
            else:
                valid_docs_count += 1

    return valid_docs_count


if __name__ == "__main__":
    with open("04_input") as f:
        DOCS = [doc.split() for doc in f.read().split("\n\n")]

    print(solve_part_one())
    print(solve_part_two())
