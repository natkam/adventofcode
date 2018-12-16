import re
from copy import deepcopy
from string import ascii_uppercase


class SolvesPartOne:
    LINE_PATTERN = re.compile(r'Step (?P<prev_letter>\w) must be finished before step (?P<next_letter>\w) can begin.')

    def __init__(self, data):
        self.sorted_data = sorted([self.LINE_PATTERN.match(line).groups() for line in data.splitlines()])

        self.next_letters = list(map(lambda x: x[1], self.sorted_data))
        self.solution = []
        self.data_dict = self._fill_data_dict(self.sorted_data)

    @staticmethod
    def _fill_data_dict(sorted_data):
        data_dict = {}

        for prev_letter, next_letter in sorted_data:
            if prev_letter in data_dict:
                data_dict[prev_letter].append(next_letter)
            else:
                data_dict[prev_letter] = [next_letter]

        return data_dict

    def check_if_step_possible(self, letter):
        if letter not in self.next_letters:
            self.solution.append(letter)

            for next_letter in self.data_dict[letter]:
                if len(self.next_letters) == 1:
                    self.solution.append(self.next_letters[0])

                self.next_letters.remove(next_letter)

            del self.data_dict[letter]

            return True
        return False

    def solve(self):
        while self.next_letters:
            for letter in self.data_dict.keys():
                added_step = self.check_if_step_possible(letter)
                if added_step:
                    break

        return ''.join(self.solution)


class SolvesPartTwo(SolvesPartOne):
    max_number_of_workers = 4 + 1  # +1 is me
    number_of_busy_workers = 0

    def __init__(self, data):
        super().__init__(data)
        self.predecessors_dict = self._fill_predecessors_dict()
        self.available_steps = []
        self.current_steps = {}
        self.already_performed_steps = []

    def _fill_predecessors_dict(self):
        predecessors_dict = {}

        for letter in ascii_uppercase:
            predecessors_dict[letter] = self._get_predecessors(letter)

        return predecessors_dict

    def _get_predecessors(self, letter):
        predecessors = []

        for key, dependants in self.data_dict.items():
            if letter in dependants:
                predecessors.append(key)

        return predecessors

    def engage_another_worker(self):
        if self.number_of_busy_workers < self.max_number_of_workers:
            self.number_of_busy_workers += 1
        else:
            raise ValueError(f"Oops, all the workers are busy! "
                             f"Number of busy workers: {self.number_of_busy_workers}")

    def release_worker(self):
        if self.number_of_busy_workers > 0:
            self.number_of_busy_workers -= 1
        else:
            raise ValueError(f"Oops, there's no worker to release! "
                             f"Number of busy workers: {self.number_of_busy_workers}")

    def solve(self):
        steps_to_perform = super().solve()
        self.data_dict = self._fill_data_dict(self.sorted_data)

        stopwatch = 0

        while len(steps_to_perform) != len(self.already_performed_steps):
            self._update_available_steps()
            self._start_work_on_available_steps()

            finished_steps, min_time = self._get_next_finished_steps_and_time()

            for finished_step in finished_steps:
                self._finish_step(finished_step)

            self._update_times_left_for_steps_in_progress(min_time)

            stopwatch += min_time

        return stopwatch

    def _update_available_steps(self):
        for key, values in deepcopy(self.predecessors_dict).items():
            if not values:
                self.available_steps.append(key)
                del self.predecessors_dict[key]

    def _start_work_on_available_steps(self):
        while self.number_of_busy_workers < self.max_number_of_workers and self.available_steps:
            self.engage_another_worker()
            available_step = self.available_steps.pop(0)
            self.current_steps.update({available_step: self._get_step_duration(available_step)})

            if len(self.available_steps) == 0:
                break

    @staticmethod
    def _get_step_duration(letter):
        return 60 + (ord(letter) % 64)  # ord('A') = 65

    def _get_next_finished_steps_and_time(self):
        min_time = min(self.current_steps.values())
        finished_steps = [step for step in self.current_steps if self.current_steps[step] == min_time]
        return finished_steps, min_time

    def _finish_step(self, finished_step):
        del self.current_steps[finished_step]
        self.already_performed_steps.append(finished_step)
        self.release_worker()

        for dependant in self.data_dict.get(finished_step, []):
            self.predecessors_dict[dependant].remove(finished_step)

    def _update_times_left_for_steps_in_progress(self, time):
        for current_step in self.current_steps:
            self.current_steps[current_step] -= time


if __name__ == '__main__':
    with open('./07_input', 'r') as f:
        data = f.read()

    print(SolvesPartTwo(data).solve())
