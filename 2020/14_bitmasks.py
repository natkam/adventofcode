from itertools import product
from typing import List

BIT_COUNT = 36


def apply_bitmask_to_values(val: str, mask: str) -> str:
    masked_val = ""
    for v, m in zip(val, mask):
        if m == "X":
            masked_val += v
        else:
            masked_val += m
    return masked_val


def solve_part_one() -> int:
    bitmask = ""
    memory = dict()
    for action, val in instructions:
        if action == "mask":
            bitmask = val
        else:
            address = action.strip("me[]")
            masked_val = apply_bitmask_to_values(f"{int(val):0{BIT_COUNT}b}", bitmask)
            memory[address] = int(masked_val, base=2)

    return sum(memory.values())


def apply_bitmask_to_address(address: bytearray, mask: bytearray) -> List[int]:
    floating_bits = []
    for i, a, m in zip(range(BIT_COUNT), address, mask):
        if m == ord("0"):
            continue
        elif m == ord("1"):
            address[i] = ord("1")
        elif m == ord("X"):
            address[i] = ord("X")
            floating_bits.append(i)

    addresses = []
    for bits in product("01", repeat=len(floating_bits)):
        new_address = address[:]
        for i, bit in zip(floating_bits, bits):
            new_address[i] = ord(bit)
        addresses.append(int(new_address))

    return addresses


def solve_part_two() -> int:
    bitmask = bytearray()
    memory = dict()
    for action, val in instructions:
        if action == "mask":
            bitmask = bytearray(val, "utf-8")
        else:
            address = action.strip("me[]")
            addresses = apply_bitmask_to_address(
                bytearray(f"{int(address):0{BIT_COUNT}b}", "utf-8"), bitmask
            )

            for addr in addresses:
                memory[addr] = int(val)

    return sum(memory.values())


if __name__ == "__main__":
    with open("14_input") as f:
        instructions = [line.split(" = ") for line in f.read().splitlines()]

    print(solve_part_one())
    print(solve_part_two())
