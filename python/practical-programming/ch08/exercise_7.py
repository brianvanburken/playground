def same_first_last(L: list) -> bool:
    """ Precondition: len(L) >= 2

    Return True if and only if first item of th elist is the same as the last.

    >>> same_first_last([3, 4, 2, 8, 3])
    True
    >>> same_first_last(['apple', 'banana', 'pear'])
    False
    >>> same_first_last([4.0, 4.5])
    False
    """
    return L[0] == L[-1]
