""" Sum all the numbers in the json file, but ignore all the objects (dicts)
with the value "red" for any key, and all the child objects of such dicts. """

import json

RED = "red"

def process(data):
    res = 0

    if type(data) is dict:
        values = data.values()

        if RED in values:
            return 0

        for val in values:
            res += process(val)
    elif type(data) is list:
        for item in data:
            res += process(item)
    elif type(data) is int:
        res += data

    return res


with open('./2015_12_input.txt', 'r') as f:
    data_str = f.read()

data = json.loads(data_str)  # I want `data` to be a dict
result = process(data)

print(result)
