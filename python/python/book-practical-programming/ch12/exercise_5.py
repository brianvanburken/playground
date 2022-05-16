from typing import List, Tuple


def find_two_smallest(L: List[float]) -> Tuple[int, int]:
    """ Return a tuple of the the indices of the two smalles values in list L.

    >>> find_two_smallest([809, 834, 477, 478, 307, 122, 96, 102, 324, 476])
    (6, 7)
    >>> find_two_smallest([2, 1])
    (1, 0)
    >>> find_two_smallest([1, 2])
    (0, 1)
    >>> find_two_smallest([2, 2, 1])
    (2, 0)
    >>> find_two_smallest([2, 2, 1, 1])
    (2, 3)
    >>> find_two_smallest([2, 2])
    (0, 1)
    >>> find_two_smallest([1])
    (0, None)
    >>> find_two_smallest([])
    (None, None)
    """
    smallest = min(L)
    min1 = L.index(smallest)
    L.remove(smallest)

    next_smallest = min(L)
    min2 = L.index(next_smallest)

    L.insert(min1, smallest)

    if min1 <= min2:
        min2 += 1

    return (min1, min2)
