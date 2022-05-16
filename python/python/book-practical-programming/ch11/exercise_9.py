def dict_intersect(dict1: dict, dict2: dict) -> dict:
    """ Takes two dictionaries and returns a dictiontary that contains only the
    key/value pairs found in both dictionaries passed.

    >>> dict_intersect({'a': 1, 'b': 2}, {'a': 1, 'b': 2})
    {'a': 1, 'b': 2}
    >>> dict_intersect({'a': 1, 'b': 3}, {'a': 1, 'b': 2})
    {'a': 1}
    >>> dict_intersect({'a': 1, 'b': 3}, {'a': 2, 'b': 4})
    {}
    """
    intersect = {}
    for key, value in dict1.items():
        if key in dict2 and dict2[key] == value:
            intersect[key] = value
    return intersect
