"""
(see 02_checksum1.py)

It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.

For example, given the following spreadsheet:

5 9 2 8
9 4 7 3
3 8 6 5

    In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
    In the second row, the two numbers are 9 and 3; the result is 3.
    In the third row, the result is 2.

In this example, the sum of the results would be 4 + 3 + 2 = 9.

What is the sum of each row's result in your puzzle input?
"""

def calculate_checksum(spreadsheet):
    int_array = make_int_array(spreadsheet)
    checksum = 0
    for row in int_array:
        row.sort()
        checksum += find_dividing_pair(row)
    return checksum

def make_int_array(spreadsheet):
    rows = spreadsheet.splitlines()
    str_array = []
    for row in rows:
        int_list_from_row = [int(item) for item in row.split()]
        str_array.append(int_list_from_row)
    return str_array

def find_dividing_pair(int_list):
    length = len(int_list)
    for i in range(0, length-1):
        divisor = int_list[i]
        for j in range(i+1, length):
            divided_number = int_list[j]
            if not divided_number % divisor:
                return int(divided_number/divisor)

def test_calculate_checksum(test_spreadsheet, expected_checksum):
    actual_checksum = calculate_checksum(test_spreadsheet)
    assert actual_checksum == expected_checksum, "actual_checksum = " + str(actual_checksum)

test_spreadsheet1 = '5 9 2 8\n' + '9 4 7 3\n' + '3 8 6 5'
test_calculate_checksum(test_spreadsheet1, 9)
test_spreadsheet2 = '5 5 5 5\n' + '12 7 1\n' + '-1 1'
test_calculate_checksum(test_spreadsheet2, 7)

#  Note that I/O methods such as readline() or readlines() leave whitespaces
# (\t, \n) in the returned strings, so they are not satisfactory for spliting
# the input into rows.
with open('02_input.txt', 'r') as file_in:
    input_spreadsheet = file_in.read()
print(calculate_checksum(input_spreadsheet))
