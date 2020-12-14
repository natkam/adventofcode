from itertools import product
from typing import List, Tuple

BIT_COUNT = 36


def to_binary(number: int) -> str:
    return f"{number:0{BIT_COUNT}b}"


def apply_bitmask_to_values(val: str, mask: str) -> str:
    return "".join(v if m == "X" else m for v, m in zip(val, mask))


def solve_part_one() -> int:
    bitmask = ""
    memory = dict()
    for action, val in instructions:
        if action == "mask":
            bitmask = val
        else:
            address = action.strip("me[]")
            masked_val = apply_bitmask_to_values(to_binary(int(val)), bitmask)
            memory[address] = int(masked_val, base=2)

    return sum(memory.values())


def apply_bitmask_to_address(
    address: bytearray, mask: bytearray
) -> Tuple[bytearray, List[int]]:
    floating_indexes = []
    for i, a, m in zip(range(BIT_COUNT), address, mask):
        if m == ord("0"):
            continue
        else:
            address[i] = m
            if m == ord("X"):
                floating_indexes.append(i)
    return address, floating_indexes


def get_floating_addresses(
    address: bytearray, floating_indexes: List[int]
) -> List[bytearray]:
    addresses = []
    for bits in product("01", repeat=len(floating_indexes)):
        new_address = address[:]
        for i, bit in zip(floating_indexes, bits):
            new_address[i] = ord(bit)
        addresses.append(new_address)

    return addresses


def solve_part_two() -> int:
    """I used bytearrays instead of str, so as to be able to mutate the addresses."""
    bitmask = bytearray()
    memory = dict()
    for action, val in instructions:
        if action == "mask":
            bitmask = bytearray(val, "utf-8")
        else:
            byte_address = bytearray(to_binary(int(action.strip("me[]"))), "utf-8")
            address, floating_indexes = apply_bitmask_to_address(byte_address, bitmask)
            addresses = get_floating_addresses(address, floating_indexes)

            for addr in addresses:
                memory[int(addr)] = int(val)  # Bytearray is unhashable.

    return sum(memory.values())


if __name__ == "__main__":
    with open("14_input") as f:
        instructions = [line.split(" = ") for line in f.read().splitlines()]

    print(solve_part_one())
    print(solve_part_two())
