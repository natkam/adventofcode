import itertools
import time

def solve_part_one(data):
    # start = time.perf_counter()

    number_of_phases = 100
    base_pattern = [0, 1, 0, -1]
    number_of_rounds = len(data)
    input_signal = list(map(int, data))

    # print(f"[start for loop] {time.perf_counter() - start}")

    for phase in range(number_of_phases):
        next_input = []
        for round_number in range(1, number_of_rounds + 1):
            multiplier = itertools.cycle(elem for elem in base_pattern for _ in range(round_number))

            next(multiplier)  # skip the 1st elem. of the pattern

            next_elem = abs(sum(a * b for a, b in zip(input_signal, multiplier))) % 10
            next_input.append(next_elem)

        input_signal = next_input

    # print(f"[end for loop] {time.perf_counter() - start}")
    return "".join(map(str, input_signal[:8]))

if __name__ == '__main__':
    with open("16_input", "r") as f:
        data = f.read()
    if data[-1] == "\n":
        data = data[:-1]

    print(solve_part_one(data))  # takes ca. 5.5 seconds

    # test_data = "12345678"
    # print(solve_part_one(test_data))
