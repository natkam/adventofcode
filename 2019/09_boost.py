from computer import Computer


def test_cases_pt_1():
    opcodes = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99".split(",")
    opcodes = [int(code) for code in opcodes]
    c = Computer(opcodes)
    c.solve()  # no input, outputs a copy of itself - i.e. self.opcodes? or stdout??
    print(c.opcodes, "\n")

    opcodes = [int(code) for code in "1102,34915192,34915192,7,4,7,99,0".split(",")]
    c = Computer(opcodes)
    c.solve()  # output a 16-digit number
    print(c.opcodes, "\n")

    opcodes = [int(code) for code in "104,1125899906842624,99".split(",")]
    c = Computer(opcodes)
    c.solve()  # output the large number in the middle
    print(c.opcodes, "\n")


if __name__ == "__main__":
    test_cases_pt_1()

    with open("09_input", "r") as f:
        data = f.read()

    opcodes = [int(code) for code in data.split(",")]

    # part one
    Computer(opcodes, initial_input=1).solve()
    # part two
    Computer(opcodes, initial_input=2).solve()
