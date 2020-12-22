
from collections import deque


def solve_part_one():
    with open("22_input") as f:
        my_hand, crab_hand = f.read().split("\n\n")

    my_hand = deque(int(card) for card in my_hand.splitlines()[1:])
    crab_hand = deque(int(card) for card in crab_hand.splitlines()[1:])

    while my_hand and crab_hand:
        my_card = my_hand.popleft()
        crab_card = crab_hand.popleft()

        if my_card > crab_card:
            my_hand.extend([my_card, crab_card])
        else:
            crab_hand.extend([crab_card, my_card])

    winner_hand = my_hand or crab_hand
    return sum(i * card for i, card in enumerate(reversed(winner_hand), 1))


if __name__ == '__main__':
    print(solve_part_one())
    