import typing


class Computer:
    """ A slightly modified version of the solution to day 5, pt. 2.

    Solves the problem in day 7.
    """

    def __init__(
        self,
        opcodes: typing.List[int],
        initial_input: typing.Optional[int] = None,
        next_input: typing.Optional[int] = None,
    ):
        self.opcodes = opcodes
        self.advance_pointer = 0
        self.initial_input = initial_input  # the phase setting
        self.next_input = next_input  # output from the previous amplifier

    def _get_params(self, index, params_number=3):
        modes = str(self.opcodes[index])[:-2]
        params = []
        for i in range(1, params_number + 1):  # max 3 params allowed
            if not modes or modes[-1] == "0":
                params.append(self.opcodes[self.opcodes[index + i]])
            elif modes[-1] == "1":
                params.append(self.opcodes[index + i])
            else:
                raise ValueError(f"Unknkown mode! {modes[-1]}")
            modes = modes[:-1]

        return params

    def _process_opcode_1(self, index):
        """ Sum together two numbers and write the result to a specified position.

        The numbers to sum are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params_number = 3
        params = self._get_params(index, params_number)
        result_index = self.opcodes[index + 3]
        value_to_write = params[0] + params[1]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _process_opcode_2(self, index):
        """ Multiply two numbers and write the result to a specified position.

        The numbers to multiply are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params_number = 3
        params = self._get_params(index, params_number)
        result_index = self.opcodes[index + 3]
        value_to_write = params[0] * params[1]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _process_opcode_3(self, index):
        """ Take the input and write it to the specified position.

        For the 1st part of the day 5 task, the input should be "1".
        For the 2nd part of the day 5 task, the input should be "5".
        """
        params_number = 1
        # params = self._get_params(index, params_number)  # not needed here
        result_index = self.opcodes[index + 1]
        # value_to_write = int(input("Provide a number: "))
        if self.initial_input is not None:
            value_to_write = self.initial_input
            self.initial_input = None
        elif self.next_input is not None:
            value_to_write = self.next_input
            self.next_input = None
        else:
            value_to_write = int(input("Provide a number: "))
        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _process_opcode_4(self, index):
        """ Take the value specified by the param, and output it to sdtout. """
        params_number = 1
        params = self._get_params(index, params_number)
        print("[OPCODE 4]", params[0])
        self.next_input = params[0]
        self.advance_pointer = params_number

        return

    def _process_opcode_5(self, index):
        """ If the 1st param != 0, move the pointer to the value from the 2nd param. """
        params_number = 2
        params = self._get_params(index, params_number)
        if params[0] != 0:
            # set the instruction pointer to the value from the second parameter
            start_at = params[1]

            return start_at

        self.advance_pointer = params_number

    def _process_opcode_6(self, index):
        """ If the 1st param is 0, move the pointer to the value from the 2nd param. """
        params_number = 2
        params = self._get_params(index, params_number)
        if params[0] == 0:
            # set the instruction pointer to the value from the second parameter
            start_at = params[1]

            return start_at

        self.advance_pointer = params_number

    def _process_opcode_7(self, index):
        """ If 1st param < 2nd, set the position from the 3rd param to 1; else - to 0. """
        params_number = 3
        params = self._get_params(index, params_number)
        if params[0] < params[1]:
            self.opcodes[self.opcodes[index + params_number]] = 1
        else:
            self.opcodes[self.opcodes[index + params_number]] = 0
        self.advance_pointer = params_number

    def _process_opcode_8(self, index):
        """ If 1st param == 2nd, set the position from the 3rd param to 1; else to 0. """
        params_number = 3
        params = self._get_params(index, params_number)
        if params[0] == params[1]:
            self.opcodes[self.opcodes[index + params_number]] = 1
        else:
            self.opcodes[self.opcodes[index + params_number]] = 0
        self.advance_pointer = params_number

    def _process_opcode_99(self):
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
        elif opcode == 5:
            return self._process_opcode_5
        elif opcode == 6:
            return self._process_opcode_6
        elif opcode == 7:
            return self._process_opcode_7
        elif opcode == 8:
            return self._process_opcode_8
        elif opcode == 99:
            return self._process_opcode_99
        else:
            raise ValueError(f"Unknown opcode! {opcode} - {type(opcode)}")
        return processor

    def solve(self):
        start_at = 0
        while True:
            for index in range(start_at, len(self.opcodes)):
                if self.advance_pointer != 0:
                    self.advance_pointer -= 1
                    continue

                opcode = int(str(self.opcodes[index])[-2:])

                if opcode == 99:
                    break

                opcode_processor = self._get_opcode_processor(opcode)
                start_at = opcode_processor(index)
                if start_at and start_at != index:
                    break  # leave the `for` loop

            if opcode == 99:
                break  # Leave `while`, i.e. end the execution

        return self.opcodes[0]  # for compatibility with day 2 solution
