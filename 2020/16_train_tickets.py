import re
from functools import reduce
from typing import Dict, List, Tuple

import pandas as pd  # type: ignore


def read_string_data() -> Tuple[str, str, str]:
    with open("16_input") as f:
        data = f.read()

    rules, my_ticket, nearby_tickets = data.split("\n\n")
    return rules, my_ticket, nearby_tickets


def get_ranges(valid_values: str) -> List[range]:
    m = re.match(r"(\d+)-(\d+) or (\d+)-(\d+)", valid_values)
    assert m is not None  # for mypy
    start_1, end_1, start_2, end_2 = [int(val) for val in m.groups()]

    return [range(start_1, end_1 + 1), range(start_2, end_2 + 1)]


def get_input_one() -> Tuple[List[range], List[List[int]]]:
    rules, _, nearby_tickets = read_string_data()

    all_ranges = []
    for rule in rules.splitlines():
        _, valid_values = rule.split(": ")
        all_ranges.extend(get_ranges(valid_values))

    tickets = [
        [int(number) for number in ticket.split(",")]
        for ticket in nearby_tickets.splitlines()[1:]
    ]

    return all_ranges, tickets


def solve_part_one() -> int:
    valid_values, tickets = get_input_one()

    error_rate = 0
    for ticket in tickets:
        for number in ticket:
            if not any(number in values_range for values_range in valid_values):
                error_rate += number
                break

    return error_rate


def get_rules_input(rules: str) -> Dict[str, List[range]]:
    parsed_rules = dict()

    for rule in rules.splitlines():
        field, valid_values = rule.split(": ")
        parsed_rules[field] = get_ranges(valid_values)

    return parsed_rules


def get_input_two() -> Tuple[Dict[str, List[range]], List[int], pd.DataFrame]:
    rules, my_ticket, nearby_tickets = read_string_data()

    parsed_rules = get_rules_input(rules)
    all_ranges = reduce(lambda a, b: a + b, parsed_rules.values())

    my_ticket_numbers = [int(number) for number in re.findall(r"\d+", my_ticket)]

    tickets = [
        [int(number) for number in ticket.split(",")]
        for ticket in nearby_tickets.splitlines()[1:]
    ]
    valid_tickets = []
    for ticket in tickets:
        if all(
            any(number in values_range for values_range in all_ranges)
            for number in ticket
        ):
            valid_tickets.append(ticket)

    return parsed_rules, my_ticket_numbers, pd.DataFrame(valid_tickets)


def solve_part_two() -> int:
    rules, my_ticket, tickets = get_input_two()

    possible_columns = dict()
    possible_indexes = dict()

    for field, values in rules.items():
        possible_columns[field] = tickets.T.loc[
            tickets.isin((*values[0], *values[1])).all()
        ]
        possible_indexes[field] = set(possible_columns[field].index.values)

    field_indexes = dict()

    while possible_indexes:
        single_indexes = {
            field: min(i) for field, i in possible_indexes.items() if len(i) == 1
        }
        field_indexes.update(single_indexes)

        for single_field, single_index in single_indexes.items():
            del possible_indexes[single_field]
            for field in possible_indexes.keys():
                possible_indexes[field] -= {single_index}

    departure_fields = [
        val for field, val in field_indexes.items() if field.startswith("departure")
    ]

    return reduce(lambda a, b: a * b, [my_ticket[i] for i in departure_fields])


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
