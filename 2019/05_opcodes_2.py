class PartOneSolution:
    def __init__(self, opcodes):
        self.opcodes = opcodes
        self.advance_pointer = False

    def _get_params(self, index, params_number):
        modes = str(self.opcodes[index])[:-2]
        params = []
        for i in range(1, params_number + 1):
            if not modes or modes[-1] == "0":
                params.append(self.opcodes[self.opcodes[index + i]])
            elif modes[-1] == "1":
                params.append(self.opcodes[index + i])
            else:
                raise ValueError(f"Unknkown mode! {modes[-1]}")
            modes = modes[:-1]

        return params

    def _process_opcode_3(self, index):
        """ Take the input ("1") and write it to the specified position. """
        result_index = self.opcodes[index + 1]
        value_to_write = 1
        self.opcodes[result_index] = value_to_write

        return

    def _process_opcode_4(self, index):
        """ Take the value specified by the param, and output it to sdtout. """
        params = self._get_params(index, 1)
        print(f"[opcode 4] {params[0]}")

        return

    def _process_opcode_1(self, index):
        """ Sum together two numbers and write the result to a specified position.

        The numbers to sum are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params = self._get_params(index, 2)
        result_index = self.opcodes[index + 3]
        value_to_write = params[0] + params[1]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = True

        return

    def _process_opcode_2(self, index):
        """ Multiply two numbers and write the result to a specified position.

        The numbers to multiply are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params = self._get_params(index, 2)
        result_index = self.opcodes[index + 3]
        value_to_write = params[0] * params[1]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = True

        return

    def _process_opcode_99(self):
        # TODO: how to handle this case?
        pass

    def _get_opcode_processor(self, opcode: int):
        if opcode == 1:
            return self._process_opcode_1
        elif opcode == 2:
            return self._process_opcode_2
        elif opcode == 3:
            return self._process_opcode_3
        elif opcode == 4:
            return self._process_opcode_4
        elif opcode == 99:
            return self._process_opcode_99
        else:
            raise ValueError(f"Unknown opcode! {opcode} - {type(opcode)}")

    def solve(self):
        for index in range(0, len(self.opcodes), 2):
            if self.advance_pointer:
                self.advance_pointer = False
                continue

            opcode = int(str(self.opcodes[index])[-2:])

            if opcode == 99:
                break

            opcode_processor = self._get_opcode_processor(opcode)
            opcode_processor(index)


def test_case_1():
    opcodes = [int(code) for code in "3,0,4,0,99".split(",")]
    PartOneSolution(opcodes).solve()  # prints "1"


if __name__ == "__main__":
    # test_case_1()

    with open("05_input", "r") as f:
        opcodes = [int(code) for code in f.read().split(",")]
    PartOneSolution(opcodes).solve()
