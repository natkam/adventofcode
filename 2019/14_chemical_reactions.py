# from __future__ import annotations

import math
import typing
from pprint import pprint


class Chemical:
    def __init__(
        self,
        amount: int,
        name: str,
        substrates: typing.Optional[typing.Dict[str, int]] = None,
    ):
        self.name = name
        self.amount = amount
        self.substrates = substrates

    def __eq__(self, other):
        # one chemical is produced by exactly one reaction, so the name is enough
        return self.name == other.name

    def __repr__(self):
        return f"<{self.name} ({self.amount}): {self.substrates}>"


def create_chemicals(data: typing.List[str]) -> typing.Dict[str, Chemical]:
    ore = Chemical(amount=1, name="ORE")
    all_chemicals = {"ORE": ore}

    reactions = [reaction.split(" => ") for reaction in data]
    substrates_and_products = [
        [reaction[0].split(", "), reaction[1]] for reaction in reactions
    ]
    for reaction in substrates_and_products:

        substrates = [substrate.split() for substrate in reaction[0]]
        substrates = {name: int(amount) for amount, name in substrates}
        product_amount, product_name = reaction[1].split()
        product = Chemical(
            amount=int(product_amount), name=product_name, substrates=substrates,
        )
        all_chemicals[product_name] = product

    return all_chemicals


def get_required_ore(all_chemicals, chemical_name: str, required_amount: int):
    required_ore = 0
    chemical: Chemical = all_chemicals[chemical_name]
    print(f"chemical {chemical}")
    for substrate_name, substrate_amount in chemical.substrates.items():
        if substrate_name != "ORE":
            print(
                f"==== recursion: {substrate_name}, required amount: {substrate_amount} ===="
            )
            ore = get_required_ore(all_chemicals, substrate_name, substrate_amount)
        else:
            print(f"ORE!")
            ore = substrate_amount
        required_ore += ore * math.ceil(required_amount / chemical.amount)
        print(
            f"[ceiling] {required_amount}/{chemical.amount} = "
            f"{math.ceil(required_amount/chemical.amount)}"
        )
        print(f"Added substrate {substrate_name} ore: {required_ore}")

    print(f"[Left the for loop] required ore: {required_ore}")

    # TODO: find some way to optimise the amount of ore for each chemical
    #  (collect the remnants, subtract them from the required ore?)
    return required_ore


def solve_part_one(data: typing.List[str]):
    all_chemicals = create_chemicals(data)
    ore_amount = get_required_ore(all_chemicals, "FUEL", 1)
    return ore_amount

    # pprint(all_chemicals)


def test():
    with open("14_test_input_0", "r") as f:
        test_data_0 = f.read().splitlines()

    with open("14_test_input_1", "r") as f:
        test_data_1 = f.read().splitlines()

    with open("14_test_input_2", "r") as f:
        test_data_2 = f.read().splitlines()

    with open("14_test_input_3", "r") as f:
        test_data_3 = f.read().splitlines()

    print(solve_part_one(test_data_0))  # returns 222, which is too much :(
    # print(solve_part_one(test_data_1))
    # print(solve_part_one(test_data_2))
    # print(solve_part_one(test_data_3))

    # assert solve_part_one(test_data_0) == 164
    # assert solve_part_one(test_data_1) == 13312
    # assert solve_part_one(test_data_2) == 180697
    # assert solve_part_one(test_data_3) == 2210736


if __name__ == "__main__":
    # with open("14_input", "r") as f:
    #     data = f.read().splitlines()
    #
    # solve_part_one(data)

    test()
