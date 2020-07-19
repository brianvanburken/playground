def different(a: any, b: any) -> bool:
    """ Checks if value a and b are different.

    >>> different(1, 2)
    True
    >>> different(1, 1)
    False
    >>> different('Test', 'test')
    True
    """
    return a != b
