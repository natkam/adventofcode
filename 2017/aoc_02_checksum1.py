"""
The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

For example, given the following spreadsheet:

5 1 9 5
7 5 3
2 4 6 8

    The first row's largest and smallest values are 9 and 1, and their difference is 8.
    The second row's largest and smallest values are 7 and 3, and their difference is 4.
    The third row's difference is 6.

In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

What is the checksum for the spreadsheet in your puzzle input?
"""

def calculate_checksum(spreadsheet):
    int_array = make_int_array(spreadsheet)
    checksum = 0
    for row in int_array:
        row.sort()
        checksum += row[-1] - row[0]
    return checksum

def make_int_array(spreadsheet):
    rows = spreadsheet.splitlines()
    str_array = []
    for row in rows:
        int_list_from_row = [int(item) for item in row.split()]
        str_array.append(int_list_from_row)
    return str_array

def test_calculate_checksum(test_spreadsheet, expected_checksum):
    actual_checksum = calculate_checksum(test_spreadsheet)
    assert actual_checksum == expected_checksum, "actual_checksum = " + str(actual_checksum)

test_spreadsheet1 = '5 1 9 5\n' + '7 5 3\n' + '2 4 6 8'
test_calculate_checksum(test_spreadsheet1, 18)
test_spreadsheet2 = '5 5 5 5\n' + '7\n' + '-1 1'
test_calculate_checksum(test_spreadsheet2, 2)

#  Note that I/O methods such as readline() or readlines() leave whitespaces
# (\t, \n) in the returned strings, so they are not satisfactory for spliting
# the input into rows.
with open('02_input.txt', 'r') as file_in:
    input_spreadsheet = file_in.read()
print(calculate_checksum(input_spreadsheet))
