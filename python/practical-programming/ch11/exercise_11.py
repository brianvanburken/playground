def db_consistent(data: dict) -> bool:
    """ Returns True if all the key in the inner dictonaries are exactly the same.

    >>> a = {'a': {'b': 1, 'c': False}, 'b': {'b': 2, 'c': True, 'd': 3}}
    >>> db_consistent(a)
    False
    >>> a = {'a': {'b': 1, 'c': False}, 'b': {'b': 2, 'c': True}}
    >>> db_consistent(a)
    True
    """
    keys = set()
    for subdict in data.values():
        subkeys = set(subdict.keys())
        if len(keys) == 0:
            keys = subkeys
        elif keys != subkeys:
            return False
    return True
