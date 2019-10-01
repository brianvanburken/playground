def dutch_flag(colors: list) -> list:
    """ Sets the list of random colors in the correct order of the Dutch flag.
    So reds first, then whites, then blues.

    >>> a = ['red', 'blue', 'white']
    >>> dutch_flag(a)
    ['red', 'white', 'blue']
    >>> a = ['red', 'blue', 'blue', 'red', 'white']
    >>> dutch_flag(a)
    ['red', 'red', 'white', 'blue', 'blue']
    """
    # create a list for each color
    reds = []
    whites = []
    blues = []

    # loop through each color
    for color in colors:
        # place each color in the correct list
        if color == 'red':
            reds.append(color)
        elif color == 'white':
            whites.append(color)
        elif color == 'blue':
            blues.append(color)
    # join the list of colors
    return reds + whites + blues
