"""
Now, the jumps are even stranger: after each jump, if the offset was three or
more, instead decrease it by 1. Otherwise, increase it by 1 as before.
"""

with open('05_input.txt', 'r') as f:
    data = f.read()

offsets = [int(offset) for offset in data.splitlines()]

def jump(offsets):
    position = 0
    jump_count = 0

    while True:
        try:
            jump_len = offsets[position]

            if jump_len >= 3:
                offsets[position] -= 1
            else:
                offsets[position] += 1
            position += jump_len
            jump_count += 1

            if position < 0:
                raise IndexError

        except IndexError as e:
            return jump_count

print(jump(offsets))
