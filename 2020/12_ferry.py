from typing import List, Tuple

FULL_ANGLE = 360


def solve_part_one(instructions: List[Tuple[str, int]]) -> int:
    angles = {0: "N", 90: "E", 180: "S", 270: "W"}
    position = {"x": 0, "y": 0}
    angle = 90

    for action, val in instructions:
        if action == "L":
            angle = (angle - val) % FULL_ANGLE
        elif action == "R":
            angle = (angle + val) % FULL_ANGLE
        else:
            if action == "F":
                action = angles[angle]

            if action == "N":
                position["y"] += val
            elif action == "S":
                position["y"] -= val
            elif action == "E":
                position["x"] += val
            elif action == "W":
                position["x"] -= val

    return abs(position["x"]) + abs(position["y"])


def solve_part_two(instructions: List[Tuple[str, int]]) -> int:
    position = {"x": 0, "y": 0}
    waypoint = {"x": 10, "y": 1}  # relative to the ship's position!

    for action, val in instructions:
        if action == "N":
            waypoint["y"] += val
        elif action == "S":
            waypoint["y"] -= val
        elif action == "E":
            waypoint["x"] += val
        elif action == "W":
            waypoint["x"] -= val
        elif action == "F":
            position["x"] += val * waypoint["x"]
            position["y"] += val * waypoint["y"]
        else:
            if val == 180:
                waypoint["x"] -= 2 * waypoint["x"]
                waypoint["y"] -= 2 * waypoint["y"]
            elif (action == "R" and val == 90) or (action == "L" and val == 270):
                waypoint["x"] = waypoint["y"]
                waypoint["y"] = -waypoint["x"]
            elif (action == "L" and val == 90) or (action == "R" and val == 270):
                waypoint["x"] = -waypoint["y"]
                waypoint["y"] = waypoint["x"]

    return abs(position["x"]) + abs(position["y"])


if __name__ == "__main__":
    with open("12_input") as f:
        instructions = [(instr[0], int(instr[1:])) for instr in f.read().splitlines()]

    print(solve_part_one(instructions))
    print(solve_part_two(instructions))
