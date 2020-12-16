import re


def solve_part_one():
    valid_values = []
    for rule in rules.splitlines():
        _, valid_values_str = rule.split(": ")
        m = re.match(r"(\d+)-(\d+) or (\d+)-(\d+)", valid_values_str)
        start_1, end_1, start_2, end_2 = [int(val) for val in m.groups()]
        valid_values.extend([range(start_1, end_1 + 1), range(start_2, end_2 + 1)])

    # my_ticket_numbers = re.findall(r"\d+", my_ticket)

    error_rate = 0
    for ticket in tickets:
        for number in ticket:
            if not any(number in values_range for values_range in valid_values):
                error_rate += number
                break

    return error_rate


if __name__ == "__main__":
    with open("16_input") as f:
        rules, my_ticket, nearby_tickets = f.read().split("\n\n")

    tickets = [
        [int(number) for number in ticket.split(",")]
        for ticket in nearby_tickets.splitlines()[1:]
    ]

    print(solve_part_one())
