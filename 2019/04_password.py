pass_min = 138307
pass_max = 654504


def adjacent_cond_1(password: str):
    for digit, next_digit in zip(password, password[1:]):
        if digit == next_digit:
            return True

    return False


def increasing_cond(password: str):
    for digit, next_digit in zip(password, password[1:]):
        if digit > next_digit:  # works for chars in Python :)
            return False

    return True


def adjacent_cond_2(password: str):
    for digit in set(password):
        if 2 * digit in password and 3 * digit not in password:
            return True

    return False


def solve_first_part():
    valid_count = 0
    for password in range(pass_min, pass_max):
        p = str(password)
        if adjacent_cond_1(p) and increasing_cond(p):
            valid_count += 1

    return valid_count


def solve_second_part():
    valid_count = 0
    for password in range(pass_min, pass_max):
        p = str(password)
        if adjacent_cond_2(p) and increasing_cond(p):
            valid_count += 1

    return valid_count


if __name__ == "__main__":
    print(solve_first_part())
    print(solve_second_part())
