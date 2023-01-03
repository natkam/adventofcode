from math import copysign
from dataclasses import dataclass


@dataclass
class Point:
    x: int = 0
    y: int = 0


@dataclass
class Head(Point):
    def step(self, direction: str):
        if direction == "R":
            self.x += 1
        elif direction == "L":
            self.x -= 1
        elif direction == "U":
            self.y += 1
        elif direction == "D":
            self.y -= 1
        else:
            raise ValueError(f"Invalid direction: {direction}")


@dataclass
class Tail(Point):
    def follow(self, head: Point):
        diff_x = head.x - self.x
        diff_y = head.y - self.y

        if abs(diff_x) > 1 and abs(diff_y) > 1:
            # This variant is only possible in part 2 (longer rope)
            self.x += copysign(abs(diff_x) - 1, diff_x)
            self.y += copysign(abs(diff_y) - 1, diff_y)

        elif abs(diff_x) > 1:
            self.x += copysign(abs(diff_x) - 1, diff_x)
            self.y += diff_y

        elif abs(diff_y) > 1:
            self.x += diff_x
            self.y += copysign(abs(diff_y) - 1, diff_y)


def part_one():
    with open("09_input") as f:
        data = f.read().splitlines()

    tail_positions = set()
    head = Head()
    tail = Tail()

    for line in data:
        direction, steps = line.split()
        for i in range(int(steps)):
            head.step(direction)
            tail.follow(head)
            tail_positions.add((tail.x, tail.y))
    return len(tail_positions)


def part_two():
    with open("09_input") as f:
        data = f.read().splitlines()

    tail_positions = set()
    head = Head()
    rope = [head, *[Tail() for _ in range(9)]]

    for j, line in enumerate(data):
        direction, steps = line.split()
        for i in range(int(steps)):
            head.step(direction)
            for h, t in zip(rope, rope[1:]):
                t.follow(h)
            tail_positions.add((t.x, t.y))

    return len(tail_positions)


if __name__ == "__main__":
    print(part_one())
    print(part_two())
