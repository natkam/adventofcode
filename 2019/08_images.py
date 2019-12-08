from collections import Counter

WIDTH = 25
HEIGHT = 6


def solve_part_one(layers):
    max_zeros = {0: 150, 1: None, 2: None}
    for no, layer in enumerate(layers):
        c = Counter(layer)
        if c.get("0", 150) < max_zeros[0]:
            max_zeros = {0: c.get("0"), 1: c.get("1", 0), 2: c.get("2", 0)}

    return max_zeros[1] * max_zeros[2]


def get_front_colour(pixels):
    for pixel in pixels:
        if pixel == "2":  # transparent pixel
            continue
        return pixel


def print_image(image):
    # image = ["▮" if px == "0" else "▯" for px in image]
    image = ["0" if px == "0" else "." for px in image]
    for y in range(HEIGHT):
        print(*image[y * WIDTH : (y + 1) * WIDTH], sep="")


def solve_part_two(layers):
    final_image = [get_front_colour(pixels) for pixels in zip(*layers)]
    print_image(final_image)
    return final_image


if __name__ == "__main__":
    with open("08_input", "r") as f:
        image = f.read()[:-1]  # the last char is newline

    number_of_layers = int(len(image) / WIDTH / HEIGHT)
    layers = [
        image[i * WIDTH * HEIGHT : (i + 1) * WIDTH * HEIGHT]
        for i in range(number_of_layers)
    ]
    print(solve_part_one(layers))
    solve_part_two(layers)
