with open("06_test_input", "r") as f:
    test_data = f.read().splitlines()

with open("06_input", "r") as f:
    data = f.read().splitlines()


known_parents = dict()


# def find_outermost_objects(orbits):
# lhs = {obj[0] for obj in orbits}
# rhs = {obj[1] for obj in orbits}
# return rhs - lhs


def find_parents(child, direct_parents):
    global known_parents
    direct_parent = direct_parents[child]
    parents = known_parents.get(child, [])
    parents.append(direct_parent)
    known_parents[child] = parents
    print(f"[{direct_parent}){child}] {known_parents[child]}")

    if direct_parent != "COM":
        parents.extend(known_parents[direct_parent])
        known_parents[child] = parents
        # find_parents(direct_parent, direct_parents)

    return known_parents[child]


def solve_first_part(data):
    direct_parents = dict(line.split(")")[::-1] for line in data)
    orbit_count = 0
    all_children = sorted(list(set(direct_parents)))
    print(all_children[:100])
    all_parents = set(direct_parents.values())
    all_objects = all_children.copy()
    all_objects.append("COM")
    # print(
    #     f"all_parents: {len(all_parents)}, all_children: {len(all_children)}, all_objects: {len(all_objects)}"
    # )

    for child in all_children:
        print(f"=== {child} ===")
        orbit_count += len(find_parents(child, direct_parents))
    #     parents = find_parent(obj, all_parents)
    # orbit_count += len(parents)
    return orbit_count

    # counted_objects = 0

    # outermost_objects = find_outermost_objects(orbits)
    # orbits = [orbit for orbit in orbits if orbit[1] not in outermost_objects]
    # orbit_count += len(outermost_objects) + counted_objects
    # counted_objects += len(outermost_objects)  # no!! fix this

    # find objects NOT orbited by anything (RHS)
    # pop them out of the list, count them as +1 each
    # find objects NOT orbited by anything in the list
    # pop them out, count all the pop items as +1 each
    # continue


if __name__ == "__main__":
    # test_result = solve_first_part(test_data)
    # print(test_result)
    # assert test_result == 42, f"wrong result: {test_result}"

    print(solve_first_part(data))
