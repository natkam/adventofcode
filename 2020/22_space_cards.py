from collections import deque
from itertools import islice
from typing import Tuple


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


def solve_part_two() -> int:
    with open("22_input") as f:
        hand_1, hand_2 = f.read().split("\n\n")

    deck_1 = deque(int(card) for card in hand_1.splitlines()[1:])
    deck_2 = deque(int(card) for card in hand_2.splitlines()[1:])

    def play(deck_1, deck_2) -> Tuple[int, deque]:
        past_decks_1 = set()
        past_decks_2 = set()
        # print("=== New Game ===\n")
        # n = 1
        while deck_1 and deck_2:
            # print(f"--- Round {n} ---")
            # print(f"Player 1: {deck_1}")
            # print(f"Player 2: {deck_2}")
            if tuple(deck_1) in past_decks_1 or tuple(deck_2) in past_decks_2:
                # print(f"\nInfinite game prevention: {deck_1}, {deck_2}. "
                #       f"Player 1 wins this game.\n")
                return 1, deck_1

            past_decks_1.add(tuple(deck_1))
            past_decks_2.add(tuple(deck_2))

            card_1 = deck_1.popleft()
            card_2 = deck_2.popleft()
            # print(f"Player 1 plays: {card_1}")
            # print(f"Player 2 plays: {card_2}")

            if card_1 <= len(deck_1) and card_2 <= len(deck_2):
                # print("Starting a new game...\n")
                new_deck_1 = deque(islice(deck_1, 0, card_1))
                new_deck_2 = deque(islice(deck_2, 0, card_2))
                winner, _ = play(new_deck_1, new_deck_2)
                # print(f"\nBack to round {n}")
            else:
                winner = 1 if card_1 > card_2 else 2

            if winner == 1:
                deck_1.extend([card_1, card_2])
                # print(f"Player 1 wins round {n} ({card_1} > {card_2})")
            else:
                deck_2.extend([card_2, card_1])
                # print(f"Player 2 wins round {n} ({card_2} > {card_1})")
            # n += 1

        return (1, deck_1) if deck_1 else (2, deck_2)

    _, winner_deck = play(deque(deck_1), deque(deck_2))

    return sum(i * card for i, card in enumerate(reversed(winner_deck), 1))


if __name__ == "__main__":
    print(solve_part_one())
    print(solve_part_two())
