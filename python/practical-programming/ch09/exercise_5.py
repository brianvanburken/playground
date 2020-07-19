def mystery_function(values: list) -> list:
    """ Reverses the inner lists

    >>> mystery_function([])
    []
    >>> mystery_function([[0, 1, 2, 3]])
    [[3, 2, 1, 0]]
    >>> mystery_function([[0, 1, 2, 3], [4, 5, 6]])
    [[3, 2, 1, 0], [6, 5, 4]]
    """
    result = []
    for sublist in values:  # nested list
        result.append([sublist[0]])  # append sublist
        for i in sublist[1:]:  # reverse insert in last list we just appended
            result[-1].insert(0, i)  # insert last element at beginning
    return result
