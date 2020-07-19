def count_duplicates(data: dict) -> int:
    """ Accepts a dictionary and counts the number of values that appear two or
    more times.

    >>> count_duplicates({'a': 1, 'b': 2})
    0
    >>> count_duplicates({'a': 1, 'b': 1})
    1
    >>> count_duplicates({'a': 1, 'b': 1, 'c': 2, 'd': 2})
    2
    """
    processed = set()
    duplicates = set()

    for value in data.values():
        if value in processed:
            duplicates.add(value)
        else:
            processed.add(value)

    return len(duplicates)
