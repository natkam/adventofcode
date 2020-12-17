import re
from functools import reduce
from typing import Dict, List, Tuple

import pandas as pd


def get_input_one() -> Tuple[List[range], List[List[int]]]:
    with open("16_input") as f:
        data = f.read()

    rules, _, nearby_tickets = data.split("\n\n")

    parsed_rules = []
    for rule in rules.splitlines():
        _, valid_values_str = rule.split(": ")
        m = re.match(r"(\d+)-(\d+) or (\d+)-(\d+)", valid_values_str)
        start_1, end_1, start_2, end_2 = [int(val) for val in m.groups()]
        parsed_rules.extend([range(start_1, end_1 + 1), range(start_2, end_2 + 1)])

    tickets = [
        [int(number) for number in ticket.split(",")]
        for ticket in nearby_tickets.splitlines()[1:]
    ]

    return parsed_rules, tickets


def solve_part_one() -> int:
    valid_values, tickets = get_input_one()

    error_rate = 0
    for ticket in tickets:
        for number in ticket:
            if not any(number in values_range for values_range in valid_values):
                error_rate += number
                break

    return error_rate


def get_input_two() -> Tuple[Dict[str, List[range]], List[int], pd.DataFrame]:
    with open("16_input") as f:
        data = f.read()

    rules, my_ticket, nearby_tickets = data.split("\n\n")

    tmp_rules: Dict[str, str] = {
        field: values
        for field, values in [line.split(": ") for line in rules.splitlines()]
    }
    parsed_rules = dict()

    for rule, values in tmp_rules.items():
        m = re.match(r"(\d+)-(\d+) or (\d+)-(\d+)", values)
        assert m is not None  # for mypy
        start_1, end_1, start_2, end_2 = [int(val) for val in m.groups()]
        parsed_rules[rule] = [range(start_1, end_1 + 1), range(start_2, end_2 + 1)]

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
    valid_tickets_df = pd.DataFrame(valid_tickets)

    return parsed_rules, my_ticket_numbers, valid_tickets_df


def solve_part_two() -> int:
    rules, my_ticket, tickets = get_input_two()

    possible_columns = dict()
    poss_indexes = dict()

    for field, values in rules.items():
        possible_columns[field] = tickets.T.loc[
            tickets.isin((*values[0], *values[1])).all()
        ]
        poss_indexes[field] = set(possible_columns[field].index.values)

    field_indexes = dict()

    while poss_indexes:
        single_indexes = {field: i for field, i in poss_indexes.items() if len(i) == 1}
        field_indexes.update(single_indexes)

        for single_field, single_index in single_indexes.items():
            del poss_indexes[single_field]
            for field in poss_indexes.keys():
                poss_indexes[field] -= single_index

    departure_fields = [
        list(val)[0]
        for field, val in field_indexes.items()
        if field.startswith("departure")
    ]
    print(departure_fields)

    return reduce(lambda a, b: a * b, [my_ticket[i] for i in departure_fields])


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
