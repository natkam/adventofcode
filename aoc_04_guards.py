import re
import timeit
import numpy as np

shift_start_p = re.compile(r'\[\d{4}-\d{2}-\d{2} \d{2}:(?P<minutes>\d{2})\] Guard #(?P<id>\d+) begins shift')
falls_asleep_p = re.compile(r'\[\d{4}-\d{2}-\d{2} \d{2}:(?P<minutes>\d{2})\] falls asleep')
wakes_up_p = re.compile(r'\[\d{4}-\d{2}-\d{2} \d{2}:(?P<minutes>\d{2})\] wakes up')


def calculate_guards_nap_times(chrono_records):
    nap_times = {}
    guard_id = 0

    for record in chrono_records:
        if shift_start_p.match(record):
            guard_id = int(shift_start_p.match(record).group('id'))
            if guard_id not in nap_times:
                nap_times[guard_id] = 0
        elif falls_asleep_p.match(record):
            falls_asleep_minute = int(falls_asleep_p.match(record).group('minutes'))
        elif wakes_up_p.match(record):
            wakes_up_minute = int(wakes_up_p.match(record).group('minutes'))
            sleep_minutes = wakes_up_minute - falls_asleep_minute
            nap_times[guard_id] += sleep_minutes

    return nap_times


def count_sleep_minutes_from_all_days(chrono_records, sleepy_guard_id):
    minutes_table = np.zeros(60, np.int8)
    guard_id = 0

    for record in chrono_records:
        if shift_start_p.match(record):
            guard_id = int(shift_start_p.match(record).group('id'))
        elif falls_asleep_p.match(record):
            falls_asleep_minute = int(falls_asleep_p.match(record).group('minutes'))
        elif wakes_up_p.match(record):
            wakes_up_minute = int(wakes_up_p.match(record).group('minutes'))
            if guard_id == sleepy_guard_id:
                minutes_table[falls_asleep_minute:wakes_up_minute] += 1

    return minutes_table


def solve_part_one(data):
    chrono_records = sorted(data.splitlines())

    nap_times = calculate_guards_nap_times(chrono_records)
    sleepy_guard_id = max(nap_times, key=nap_times.get)  # 3209

    minutes_table = count_sleep_minutes_from_all_days(chrono_records, sleepy_guard_id)
    sleepy_minute = minutes_table.argmax()

    return int(sleepy_guard_id) * sleepy_minute


class SolvesPartTwoVersion1:
    def __init__(self, data):
        self.chrono_records = sorted(data.splitlines())
        self.nap_times = calculate_guards_nap_times(self.chrono_records)
        self.guard_ids = set(self.nap_times.keys())

    def solve(self):
        max_asleep_minutes = {}

        for guard_id in self.guard_ids:
            minutes_table = count_sleep_minutes_from_all_days(self.chrono_records, guard_id)
            sleepy_minute = minutes_table.argmax()
            max_asleep_minutes[guard_id] = (sleepy_minute, minutes_table[sleepy_minute])

        guard_data = max(max_asleep_minutes.items(), key=self._get_times_asleep)
        guard_id = guard_data[0]
        minute_number = guard_data[1][0]

        return guard_id * minute_number

    @staticmethod
    def _get_times_asleep(guard_data):
        return guard_data[1][1]


class SolvesPartTwoVersion2:
    """
    This solution seems to be significantly faster, even if solve() is rather messy...
    """

    def __init__(self, data):
        self.chrono_records = sorted(data.splitlines())
        self.nap_times = calculate_guards_nap_times(self.chrono_records)
        guard_ids = set(self.nap_times.keys())
        self.minutes_table = np.zeros((len(guard_ids), 60), np.int8)
        self.guard_ids = {guard_id: array_index for array_index, guard_id in enumerate(guard_ids)}

    def solve(self):
        self.fill_minutes_table()

        sleepy_minutes = self.minutes_table.argmax(axis=1)
        sleep_counts = np.amax(self.minutes_table, axis=1)

        minutes_and_counts = np.hstack((sleepy_minutes.reshape((-1, 1)), sleep_counts.reshape((-1, 1))))

        max_minute_number = max(minutes_and_counts, key=lambda t: t[1])[0]
        max_index = minutes_and_counts.argmax(axis=0)[1]

        guard_id = list(self.guard_ids.keys())[list(self.guard_ids.values()).index(max_index)]

        return guard_id * max_minute_number
        ## ok, lol, whatever xD but this version works too.

    def fill_minutes_table(self):
        """
        Fill the self.minutes_table so that each row contains the 60 minutes
        during each guard's shifts and the numbers in each minute are counts
        of times when given guard was asleep during this minute.

        :return: None
        """

        for record in self.chrono_records:
            if shift_start_p.match(record):
                guard_id = int(shift_start_p.match(record).group('id'))
            elif falls_asleep_p.match(record):
                falls_asleep_minute = int(falls_asleep_p.match(record).group('minutes'))
            elif wakes_up_p.match(record):
                wakes_up_minute = int(wakes_up_p.match(record).group('minutes'))
                self.minutes_table[self.guard_ids[guard_id]][falls_asleep_minute:wakes_up_minute] += 1

    @staticmethod
    def _get_times_asleep(guard_data):
        return guard_data[1][1]


if __name__ == '__main__':
    with open('04_input', 'r') as f:
        data = f.read()

    print(solve_part_one(data))
    print(SolvesPartTwoVersion2(data).solve())
    # print('Part 2, ver. 1: ',
    #       timeit.timeit('SolvesPartTwoVersion1(data).solve()', number=100, globals=globals()))
    #       # 4.881550515005074 s
    # print('Part 2, ver. 2: ',
    #       timeit.timeit('SolvesPartTwoVersion2(data).solve()', number=100, globals=globals()))
    #       # 0.5612120069999946 s
