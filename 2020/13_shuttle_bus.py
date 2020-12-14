import operator
from typing import List


def solve_part_one() -> int:
    ids = [int(bus_id) for bus_id in all_ids.split(",") if bus_id != "x"]
    waiting_times = {bus_id: bus_id - ts % bus_id for bus_id in ids}
    best_bus = min(waiting_times.items(), key=operator.itemgetter(1))

    return best_bus[0] * best_bus[1]


def solve_part_two(ids: List[str]):
    # The bus that's supposed to leave at time 0 (the timestamp we're looking for) is 19.
    # Four of the buses will leave 19 minutes af the timestamp we're looking for (ts):
    # 41, 29, 383 - and 19, obviously. Therefore the ts has to be a multiple of these
    # four numbers product (8652353), minus 19.

    # ts is supposed to be bigger than 100_000_000_000_000, so let's start with:
    import time
    start = time.perf_counter()
    ts = 8652353 * 11557549 - 19  # = 99999993762778
    while True:
        waiting_times = {bus_id: int(bus_id) - ts % int(bus_id) for bus_id in ids if bus_id != "x"}
        # waiting_times = sorted(waiting_times.items(), key=operator.itemgetter(1))
        for minute, bus_id in enumerate(ids):
            if bus_id == "x" or bus_id == ids[0]:
                continue
            if waiting_times[bus_id] != minute % int(bus_id) or (not minute % int(bus_id) and waiting_times[bus_id] == int(bus_id)):
                # print(f"{ts=}, {minute=} ({minute % int(bus_id)}), {bus_id=}, ")
                break
        else:
            print(f"{ts=}, {waiting_times}")
            break


        # print(waiting_times)
        ts += 8652353  # The product of 19, 41, 29 and 383
        if ts > 1_000_000_000_000_000:
            print("Exceeded 10^15, breaking")
            return
        if ts == 108_652_353_000_000:
            print(f"{ts=} checkpoint: {time.perf_counter() - start} seconds")
        if ts == 186_523_523_762_778:  # 10k rounds from the start
            print(f"{ts=} checkpoint: {time.perf_counter() - start} seconds")
        if ts == 619_141_173_762_778:  # 60k rounds
            print(f"{ts=} checkpoint: {time.perf_counter() - start} seconds")
        if ts == 965_235_293_762_778:  # 100k rounds
            print(f"{ts=} checkpoint: {time.perf_counter() - start} seconds")
    print(ids)
    return ts


def test_part_two():
    all_ids = "17,x,13,19".split(",")
    assert solve_part_two(all_ids) == 3417

    all_ids = "67,7,59,61".split(",")
    assert solve_part_two(all_ids) == 754018

    all_ids = "67,x,7,59,61".split(",")
    assert solve_part_two(all_ids) == 779210

    all_ids = "67,7,x,59,61".split(",")
    assert solve_part_two(all_ids) == 1261476

    # import time
    # print("Starting the last test...")
    # start = time.perf_counter()
    all_ids = "1789,37,47,1889".split(",")
    assert solve_part_two(all_ids) == 1202161486
    # print(f"The test took {time.perf_counter() - start} seconds.")  # ~3 seconds



if __name__ == '__main__':
    with open("13_input") as f:
        timestamp, all_ids = f.read().splitlines()
    ts = int(timestamp)

    # print(solve_part_one())
    # test_part_two()
    print(solve_part_two(all_ids.split(",")))
