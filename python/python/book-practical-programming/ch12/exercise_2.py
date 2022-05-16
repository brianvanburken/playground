def min_index(a: list) -> (any, int):
    """ Gets the minimum value and its index in the list.

    >>> a = [0, 1, 2, 3]
    >>> min_index(a)
    (0, 0)
    >>> a = [1, 8, 0, 3]
    >>> min_index(a)
    (0, 2)
    >>> a = [1, 8, 0, 3, 0]
    >>> min_index(a)
    (0, 2)
    """
    current_index = 0
    current_value = a[current_index]
    for index in range(1, len(a)):
        value = a[index]
        if value < current_value:
            current_index = index
            current_value = value
    return (current_value, current_index)


def max_index(a: list) -> (any, int):
    """ Gets the maximum value and its index in the list.

    >>> a = [3, 2, 1, 0]
    >>> max_index(a)
    (3, 0)
    >>> a = [1, 8, 0, 3]
    >>> max_index(a)
    (8, 1)
    >>> a = [1, 8, 0, 3, 0, 8]
    >>> max_index(a)
    (8, 1)
    """
    current_index = 0
    current_value = a[current_index]
    for index in range(1, len(a)):
        value = a[index]
        if value > current_value:
            current_index = index
            current_value = value
    return (current_value, current_index)


def min_or_max_index(a: list, minimum: bool) -> (any, int):
    """ Gets the minimum value with index of the given list if the second
    argument is True, else it will get the maximum value with index.

    >>> a = [0, 1, 2, 3]
    >>> min_or_max_index(a, True)
    (0, 0)
    >>> a = [0, 1, 3, 2]
    >>> min_or_max_index(a, False)
    (3, 2)
    """
    return min_index(a) if minimum else max_index(a)
