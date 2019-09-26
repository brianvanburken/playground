alkaline_earth_metals = [
    [4, 9.012],
    [12, 24.305],
    [20, 40.078],
    [38, 87.62],
    [56, 137.327],
    [88, 226]
]
for [number, weight] in alkaline_earth_metals:
    print(number)
    print(weight)

number_and_weight = []
for metal in alkaline_earth_metals:
    number_and_weight.extend(metal)
print(number_and_weight)
