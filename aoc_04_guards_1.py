import re

import numpy as np

with open('04_input', 'r') as f:
    data = f.read()

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


class SolvesPartTwo:
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


if __name__ == '__main__':
    with open('04_input', 'r') as f:
        data = f.read()

    # print(solve_part_one(data))
    print(SolvesPartTwo(data).solve())
