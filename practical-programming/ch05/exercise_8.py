def convert_temperatures(temperature: int, source: str, target: str) -> int:
    """ Converts any temperature given from its source unit to target unit
    """

    celsius = temperature

    if source == "Kelvin":
        celsius = temperature - 273.15
    elif source == "Fahrenheit":
        celsius = (temperature - 32) * (5 / 9)
    elif source == "Rankine":
        celsius = (temperature - 491.67) * (5 / 9)
    elif source == "Delisle":
        celsius = (100 - temperature) * (2 / 3)
    elif source == "Newton":
        celsius = temperature * (100 / 33)
    elif source == "Reaumur":
        celsius = temperature * (5 / 4)
    elif source == "Romer":
        celsius = (temperature - 7.5) * (40 / 21)

    if target == "Kelvin":
        return celsius + 273.15
    elif target == "Fahrenheit":
        return (celsius * (5 / 9)) + 32
    elif target == "Rankine":
        return (celsius + 273.15) * (9 / 5)
    elif target == "Delisle":
        return (100 - temperature) * (3 / 2)
    elif target == "Newton":
        return temperature * (33 / 100)
    elif target == "Reaumur":
        return temperature * (4 / 5)
    elif target == "Romer":
        return temperature * (21 / 40) + 7.5
    return celsius

# To add new temperature you need to add 2 if's. One for conversion to celsius
# and one from.
