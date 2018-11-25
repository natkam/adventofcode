""" Sum all the numbers in the json file. There are no strings containing numbers. """

import re

with open('./2015_12_input.txt', 'r') as f:
    data = f.read()

numbers = [int(n) for n in re.split('\[|\]|}|{|"|,|:', data) if n.isdigit() or n.startswith('-')]
result = sum(numbers)
print(result)
