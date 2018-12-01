"""
Starting from a state that has already been seen, how many block redistribution
cycles must be performed before that same state is seen again?
"""


from copy import deepcopy
from itertools import cycle


NUMBER_OF_BANKS = 16

def balance_the_blocks(state):
    history = []
    cycles_count = 0

    while True:
        history.append(deepcopy(state))
        state = redistribute(state)
        cycles_count += 1

        if state in history:
            return cycles_count - history.index(state)

def redistribute(state):
    blocks_to_move = max(state)
    start_index = state.index(blocks_to_move)

    state[start_index] = 0

    for index in range(start_index + 1, blocks_to_move + start_index + 1):
        state[index % NUMBER_OF_BANKS] += 1

    return state

initial_state = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

assert NUMBER_OF_BANKS == len(initial_state)

number_of_redistributions = balance_the_blocks(initial_state)
print(number_of_redistributions)
