from collections import Counter


def solve_part_one(layers):
    max_zeros = {0: 150, 1: None, 2: None}
    for no, layer in enumerate(layers):
        c = Counter(layer)
        if c.get("0", 150) < max_zeros[0]:
            max_zeros = {0: c.get("0", 0), 1: c.get("1", 0), 2: c.get("2", 0)}

    return max_zeros[1] * max_zeros[2]


if __name__ == "__main__":
    with open("08_input", "r") as f:
        image = f.read()[:-1]  # the last char is newline

    width = 25
    height = 6
    number_of_layers = int(len(image) / width / height)
    layers = [
        image[i * width * height : (i + 1) * width * height]
        for i in range(number_of_layers)
    ]
    print(solve_part_one(layers))
