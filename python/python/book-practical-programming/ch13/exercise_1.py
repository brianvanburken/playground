from typing import Any


def linear_search1(lst: list, value: Any) -> int:
    """ Return the index of the last occurrence of value in lst, or return
    -1 if value is not in lst.

    >>> linear_search1([2, 5, 1, -3], 5)
    1
    >>> linear_search1([2, 4, 2], 2)
    2
    >>> linear_search1([2, 5, 1, -3], 4)
    -1
    >>> linear_search1([], 5)
    -1
    """
    i = len(lst) - 1
    while i > 0 and lst[i] != value:
        i -= 1
    if i == 0:
        return -1
    else:
        return i


def linear_search2(lst: list, value: Any) -> int:
    """Return the index of the last occurrence of value in lst, or return
    -1 if value is not in lst.

    >>> linear_search2([2, 5, 1, -3], 5)
    1
    >>> linear_search2([2, 4, 2], 2)
    2
    >>> linear_search2([2, 5, 1, -3], 4)
    -1
    >>> linear_search2([], 5)
    -1
    """
    for i in range(len(lst) - 1, -1, -1):
        if lst[i] == value:
            return i
    return -1


def linear_search3(lst: list, value: Any) -> int:
    """ Return the index of the first occurrence of value in lst, or return
    -1 if value is not in lst.

    >>> linear_search3([2, 5, 1, -3], 5)
    1
    >>> linear_search3([2, 4, 2], 2)
    2
    >>> linear_search3([2, 5, 1, -3], 4)
    -1
    >>> linear_search3([], 5)
    -1
    """
    l = [value] + lst

    i = len(l) - 1

    while l[i] != value:
        i -= 1

    if i == 0:
        return -1
    else:
        return i - 1  # keep sentinal index in mind


if __name__ == "__main__":
    import doctest
    doctest.testmod()
