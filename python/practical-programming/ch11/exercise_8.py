def is_balanced(color: dict) -> bool:
    """ Checks if the values of the color added up to 1.0.

    >>> is_balanced({'R': 0.1, 'G': 0.7, 'B': 0.2})
    True
    >>> is_balanced({'R': 0.4, 'G': 0.7, 'B': 0.2})
    False
    """
    return sum(color.values()) == 1.0
