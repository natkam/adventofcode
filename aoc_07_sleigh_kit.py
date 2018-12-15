import re


class SolvesPartOne:
    LINE_PATTERN = re.compile(r'Step (?P<prev_letter>\w) must be finished before step (?P<next_letter>\w) can begin.')

    def __init__(self, data):
        sorted_data = sorted([self.LINE_PATTERN.match(line).groups() for line in data.splitlines()])

        self.next_letters = list(map(lambda x: x[1], sorted_data))
        self.solution = []
        self.data_dict = self._fill_data_dict(sorted_data)

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


if __name__ == '__main__':
    with open('./07_input', 'r') as f:
        data = f.read()

    print(SolvesPartOne(data).solve())
