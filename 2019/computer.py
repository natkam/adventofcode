from __future__ import annotations

from typing import List, Optional, Callable, Union, cast


# TODO: This solution works, but... Check the write-params (e.g. for opcodes 1, 2, 3, ...)
# TODO: to see if an IndexError can occur there!
# TODO:#2 Refactor the code to handle both read and write params together, nicely


class PauseExecutionException(Exception):
    pass


class Computer:
    """A slightly modified version of the solution to day 5, pt. 2.

    Solves the problem in days 7 and 9, but works for day 5 too.
    """

    instance_counter = 0

    def __init__(
        self,
        opcodes: List[int],
        initial_input: Optional[int] = None,
        next_input: Optional[int] = None,
        previous: Optional[Computer] = None,
    ):
        self.opcodes: List[int] = opcodes
        self.advance_pointer: int = 0
        self.relative_base: int = 0
        self.start_at: Optional[int] = 0
        self.initial_input: Optional[int] = initial_input  # [d7] phase setting
        self.next_input: Optional[int] = next_input  # [d7] set to output from prev. amplifier
        self.output_for_next_computer: Optional[int] = None  # [d7]
        self.previous: Optional[Computer] = previous  # [d7] prev. amplifier

        self.is_paused: bool = False

        self._set_name()

    def _set_name(self) -> None:
        """Automatically set the name attr. (A, B, C, ...) for every created instance."""
        offset = ord("A")
        self.name = chr(self.instance_counter + offset)
        Computer.instance_counter += 1

    def _extend_opcodes(self, target_index: int) -> None:
        """Add missing 'memory', i.e. zeros at the end of the opcodes list."""
        missing_zeros = range(target_index - len(self.opcodes) + 1)
        self.opcodes.extend(0 for _ in missing_zeros)

    def _get_target_index(self, modes: str, param_index: int):
        if not modes or modes[-1] == "0":
            return self.opcodes[param_index]
        elif modes[-1] == "1":
            return param_index
        elif modes[-1] == "2":
            return self.relative_base + self.opcodes[param_index]
        else:
            raise ValueError(f"Unknkown mode! {modes[-1]}")

    def _get_params(self, index: int, params_number: int = 3) -> List[int]:
        modes: str = str(self.opcodes[index])[:-2]
        params: List[int] = []
        for i in range(1, params_number + 1):  # max 3 params allowed
            target_index = self._get_target_index(modes, index + i)

            if len(self.opcodes) <= target_index:
                self._extend_opcodes(target_index)

            params.append(self.opcodes[target_index])
            modes = modes[:-1]

        return params

    def _process_opcode_1(self, index: int) -> None:
        """Sum together two numbers and write the result to a specified position.

        The numbers to sum are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params_number = 3
        params = self._get_params(index, params_number)
        value_to_write = params[0] + params[1]

        # The 3rd param of the opcode is a write param...
        third_param_mode = str(self.opcodes[index])[:-4]
        if third_param_mode == "2":
            result_index = self.relative_base + self.opcodes[index + 3]
        else:
            result_index = self.opcodes[index + 3]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _process_opcode_2(self, index: int) -> None:
        """Multiply two numbers and write the result to a specified position.

        The numbers to multiply are specified by the first two params.
        The third param specifies the position to write the result.
        """
        params_number = 3
        params = self._get_params(index, params_number)
        value_to_write = params[0] * params[1]

        # The 3rd param of the opcode is the where-to-write param
        third_param_mode = str(self.opcodes[index])[:-4]
        if third_param_mode == "2":
            result_index = self.relative_base + self.opcodes[index + params_number]
        else:
            result_index = self.opcodes[index + params_number]

        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _process_opcode_3(self, index: int) -> None:
        """Take the input and write it to the specified position.

        For the 1st part of the day 5 task, the input should be "1".
        For the 2nd part of the day 5 task, the input should be "5".
        """
        params_number = 1

        # The param of the opcode is the where-to-write param
        param_mode = str(self.opcodes[index])[:-2]
        if param_mode == "2":
            result_index = self.relative_base + self.opcodes[index + params_number]
        else:
            result_index = self.opcodes[index + params_number]

        if self.initial_input is not None:
            value_to_write = self.initial_input
            self.initial_input = None
        elif self.next_input is not None:
            value_to_write = self.next_input
            self.next_input = None
        else:
            value_to_write = self._get_input()
        self.opcodes[result_index] = value_to_write
        self.advance_pointer = params_number

        return

    def _get_input(self) -> int:
        """An auxiliary method to provide input for the opcode 3."""
        input_value = input("Provide a number: ")
        self.next_input = int(input_value)

        return self.next_input

    def _process_opcode_4(self, index: int) -> None:
        """Take the value specified by the param, and output it to stdout."""
        params_number = 1
        params = self._get_params(index, params_number)
        output = params[0]
        # print("[OPCODE 4]", output)
        self.advance_pointer = params_number

        if self.previous is not None:  # [d7, pt.2]
            # The execution should pause and pass the output to the next computer
            self.output_for_next_computer = output
            raise PauseExecutionException
        else:  # other days' riddles
            self.next_input = output

        return

    def _process_opcode_5(self, index: int) -> Optional[int]:
        """If the 1st param != 0, move the pointer to the value from the 2nd param."""
        params_number = 2
        params = self._get_params(index, params_number)
        if params[0] != 0:
            # Set the instruction pointer to the value from the second parameter
            start_at = params[1]

            return start_at

        self.advance_pointer = params_number

        return None

    def _process_opcode_6(self, index: int) -> Optional[int]:
        """If the 1st param is 0, move the pointer to the value from the 2nd param."""
        params_number = 2
        params = self._get_params(index, params_number)
        if params[0] == 0:
            # Set the instruction pointer to the value from the second parameter
            start_at = params[1]

            return start_at

        self.advance_pointer = params_number

        return None

    def _process_opcode_7(self, index: int) -> None:
        """If 1st param < 2nd, set the position from the 3rd param to 1; else - to 0."""
        params_number = 3
        params = self._get_params(index, params_number)

        # The 3rd param of the opcode is the where-to-write param
        third_param_mode = str(self.opcodes[index])[:-4]
        if third_param_mode == "2":
            result_index = self.relative_base + self.opcodes[index + 3]
        else:
            result_index = self.opcodes[index + 3]

        if params[0] < params[1]:
            self.opcodes[result_index] = 1
        else:
            self.opcodes[result_index] = 0
        self.advance_pointer = params_number

        return

    def _process_opcode_8(self, index: int) -> None:
        """If 1st param == 2nd, set the position from the 3rd param to 1; else to 0."""
        params_number = 3
        params = self._get_params(index, params_number)

        # The 3rd param of the opcode is the where-to-write param
        third_param_mode = str(self.opcodes[index])[:-4]
        if third_param_mode == "2":
            result_index = self.relative_base + self.opcodes[index + 3]
        else:
            result_index = self.opcodes[index + 3]

        if params[0] == params[1]:
            self.opcodes[result_index] = 1
        else:
            self.opcodes[result_index] = 0
        self.advance_pointer = params_number

        return

    def _process_opcode_9(self, index: int) -> None:
        """Change the relative base by the value of the opcode's param."""
        params_number = 1
        params = self._get_params(index, params_number)
        self.relative_base += params[0]
        self.advance_pointer = params_number

        return

    def _process_opcode_99(self, _: int) -> None:
        pass

    def _get_opcode_processor(
        self, opcode: int
    ) -> Callable[[int], Union[Optional[int], None]]:
        try:
            processor = getattr(self, f"_process_opcode_{opcode}", None)
        except AttributeError:
            raise ValueError(f"Unknown opcode! {opcode} - {type(opcode)}")

        return processor

    def solve(self) -> None:
        # start_at: Optional[int]
        while True:
            self.start_at = cast(int, self.start_at)
            for index in range(self.start_at, len(self.opcodes)):
                if self.advance_pointer != 0:
                    self.advance_pointer -= 1
                    continue

                opcode = int(str(self.opcodes[index])[-2:])

                if opcode == 99 or opcode == 0:  # 0 - additional "memory"
                    break

                opcode_processor = self._get_opcode_processor(opcode)
                try:
                    self.start_at = opcode_processor(index)
                except PauseExecutionException:
                    self.start_at = index + 1
                    self.is_paused = True
                    return

                if self.start_at and self.start_at != index:
                    break  # leave the `for` loop

            if self.start_at is None or opcode == 99 or opcode == 0:
                # 0 - additional "memory"
                break  # Leave `while`, i.e. end the execution


    def resume(self):
        """Resume paused program execution, using input from the previous computer."""
        # print(f"amp_{self.name}: RESUMING (start_at {self.start_at})")
        self.is_paused = False
        self.next_input = self.previous.output_for_next_computer
        self.solve()
