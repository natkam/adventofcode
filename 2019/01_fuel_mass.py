with open("01_input") as f:
    data = f.read()


def reduce_fuel(mass):
    total_fuel = 0
    fuel = mass // 3 - 2
    if fuel > 0:
        total_fuel += fuel + reduce_fuel(fuel)
    return total_fuel


def solve_part_1(data):
    total_fuel = sum(int(mass) // 3 - 2 for mass in data.splitlines())
    return total_fuel


def solve_part_2(initial_fuel):
    module_masses = map(int, data.splitlines())
    module_fuels = map(reduce_fuel, module_masses)
    return sum(module_fuels)



if __name__ == "__main__":
    total_fuel = solve_part_1(data)
    print(solve_part_2(total_fuel))
