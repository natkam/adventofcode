import operator


def solve_part_one():
    waiting_times = {bus_id: bus_id - ts % bus_id for bus_id in ids}
    best_bus = min(waiting_times.items(), key=operator.itemgetter(1))

    return best_bus[0] * best_bus[1]


if __name__ == '__main__':
    with open("13_input") as f:
        timestamp, all_ids = f.read().splitlines()
    ids = [int(bus_id) for bus_id in all_ids.split(",") if bus_id != "x"]
    ts = int(timestamp)

    print(solve_part_one())
