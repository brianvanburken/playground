def count_values(a: dict) -> int:
    """ Counts the number of distict values of a dict.

    >>> count_values({})
    0
    >>> count_values({'red': 1, 'green': 1})
    1
    >>> count_values({'red': 1, 'green': 1, 'blue': 2})
    2
    >>> count_values({'red': 1, 'green': 2, 'blue': 3})
    3
    """
    values = set()
    for key in a:
        values.add(a[key])
    return len(values)
