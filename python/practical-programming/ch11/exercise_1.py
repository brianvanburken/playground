def find_dups(numbers: list) -> set:
    """ Accepts a list of integers and returns a set of integers that appear in
    the list more than once.

    >>> find_dups([1, 2, 3, 4, 1])
    {1}
    >>> find_dups([1, 1, 2, 2, 3, 4])
    {1, 2}
    """
    processed = set()
    duplicates = set()

    for number in numbers:
        if number in processed:
            duplicates.add(number)
        else:
            processed.add(number)

    return duplicates
