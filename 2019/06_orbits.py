with open("06_test_input", "r") as f:
    test_data = f.read().splitlines()

with open("06_input", "r") as f:
    data = f.read().splitlines()


def find_outermost_objects(orbits):
    lhs = {obj[0] for obj in orbits}
    rhs = {obj[1] for obj in orbits}
    return rhs - lhs

def solve_first_part(data):
    orbits = [line.split(")") for line in data]
    orbit_count = 0
    counted_objects = 0

    outermost_objects = find_outermost_objects(orbits)
    orbits = [orbit for orbit in orbits if orbit[1] not in outermost_objects]
    orbit_count += len(outermost_objects) + counted_objects
    counted_objects += len(outermost_objects)  # no!! fix this



    # find objects NOT orbited by anything (RHS)
    # pop them out of the list, count them as +1 each
    # find objects NOT orbited by anything in the list
    # pop them out, count all the pop items as +1 each
    # continue


if __name__ == "_main__":
    test_result = solve_first_part(test_data)
    assert test_result == 42, f"wrong result: {test_result}"

    # print(solve_first_part(data))
