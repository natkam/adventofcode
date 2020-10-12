from computer import Computer


def test_case_pt_1():
    opcodes = [int(code) for code in "3,0,4,0,99".split(",")]
    Computer(opcodes).solve()  # prints the value received as the input


def test_case_pt_2():
    opcodes = [int(code) for code in "3,9,8,9,10,9,4,9,99,-1,8".split(",")]
    Computer(opcodes).solve()  # prints "1" if input=8, else "0"

    opcodes = [int(code) for code in "3,9,7,9,10,9,4,9,99,-1,8".split(",")]
    Computer(opcodes).solve()  # prints "1" if input<8, else "0"

    opcodes = [int(code) for code in "3,3,1108,-1,8,3,4,3,99".split(",")]
    Computer(opcodes).solve()  # prints "1" if input=8, else "0"

    opcodes = [int(code) for code in "3,3,1107,-1,8,3,4,3,99".split(",")]
    Computer(opcodes).solve()  # prints "1" if input<8, else "0"

    opcodes = [
        int(code) for code in "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".split(",")
    ]
    Computer(opcodes).solve()  # prints "1" if input!=0, else "0"

    opcodes = [int(code) for code in "3,3,1105,-1,9,1101,0,0,12,4,12,99,1".split(",")]
    Computer(opcodes).solve()  # prints "1" if input!=0, else "0"

    opcodes_str = (
        "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
        + "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104"
        + ",999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    )
    opcodes = [int(code) for code in opcodes_str.split(",")]
    Computer(opcodes).solve()
    # prints "999" if input<8, "1000" if input=8, else "1001"


if __name__ == "__main__":
    test_case_pt_1()
    test_case_pt_2()

    with open("05_input", "r") as f:
        opcodes = [int(code) for code in f.read().split(",")]
    Computer(opcodes, inputs=[1]).solve()

    with open("05_input", "r") as f:
        opcodes = [int(code) for code in f.read().split(",")]
    Computer(opcodes, inputs=[5]).solve()
