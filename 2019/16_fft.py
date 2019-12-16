import itertools as it
import time
import typing


# g = it.chain.from_iterable(it.repeat("1234", 3))  # <itertools.chain at ...>
# next(g)
# or
# for _ in g:
#     print(_)  # will print 12 numbers


def apply_fft(
    input_signal: typing.List[int],
    # input_signal: typing.Generator,
    signal_length: int,
    number_of_phases: int = 100,
    base_pattern: typing.Tuple[int] = (0, 1, 0, -1),
) -> typing.Iterator:
    number_of_rounds = signal_length
    # number_of_rounds = len(input_signal)
    signal_gen = (signal for signal in input_signal)


    for phase in range(number_of_phases):
        next_input = []
        for round_number in range(1, number_of_rounds + 1):
            multipliers = it.cycle(
                elem for elem in base_pattern for _ in range(round_number)
            )

            next(multipliers)  # skip the 1st elem. of the pattern
            # asdf = [a * b for a, b in zip(signal_gen, multipliers)]
            # print(asdf, len(asdf), sum(asdf))

            next_elem: int = abs(sum(a * b for a, b in zip(signal_gen, multipliers))) % 10
            next_input.append(next_elem)
            # signal_gen = (signal for signal in input_signal)
            # print(list(signal_gen))
            signal_gen = (signal for signal in input_signal)

        # signal_gen = (elem for elem in next_input * number_of_rounds)
        # print(len(next_input))
        input_signal = next_input
        signal_gen = it.chain(next_input)

    return signal_gen


def solve_part_one(data):
    start = time.perf_counter()

    input_signal = list(map(int, data))

    print(f"[start applying FFT] {time.perf_counter() - start}")
    # output = apply_fft(input_signal)
    # input_gen = (int(char) for char in data)
    # output = apply_fft(input_gen, len(data))
    output = apply_fft(input_signal, len(data))

    print(f"[end applying FFT] {time.perf_counter() - start}")

    result = "".join(map(str, it.islice(output, 8)))

    return result


# def solve_part_two(data):
    # TODO: check out sys.getsizeof(data) for generators
#     msg_offset = int(data[:7])
#     print(msg_offset)
#     # input_signal = list(map(int, data * 10_000))
#     # input_signal = it.chain.from_iterable(it.repeat(data, 10_000))
#     input_signal = (int(char) for char in data * 10_000)
#     output = apply_fft(input_signal, len(data) * 10_000)
#     print(len(output))
#     result = "".join(map(str, output))[msg_offset : msg_offset + 8]
#     # return result


if __name__ == "__main__":
    # test_data = "12345678"
    # print(solve_part_one(test_data))

    with open("16_input", "r") as f:
        data = f.read()
    if data[-1] == "\n":
        data = data[:-1]
    #
    print(solve_part_one(data))  # takes ca. 5.5 seconds
    # print(solve_part_two(data))
