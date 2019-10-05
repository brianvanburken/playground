def bubble_sort_2(L: list) -> list:
    """ Reorder the items in L from smallest to largest.

    >>> L = [3, 4, 7, -1, 2, 5]
    >>> bubble_sort_2(L)
    [-1, 2, 3, 4, 5, 7]

    >>> L = []
    >>> bubble_sort_2(L)
    []

    >>> L = [1]
    >>> bubble_sort_2(L)
    [1]

    >>> L = [2, 1]
    >>> bubble_sort_2(L)
    [1, 2]

    >>> L = [3, 3, 3]
    >>> bubble_sort_2(L)
    [3, 3, 3]

    >>> L = [-5, 3, 0, 3, -6, 2, 1, 1]
    >>> bubble_sort_2(L)
    [-6, -5, 0, 1, 1, 2, 3, 3]
    """
    sorted = False  # start with assumption that list isn't sorted
    while not sorted:
        sorted = True  # make assumption that list is sorted
        for i in range(len(L) - 1, 0, -1):  # loop through each index backward
            if L[i] < L[i-1]:  # if ancestor is larger than the current index
                L[i], L[i - 1] = L[i - 1], L[i]  # swap both positions
                sorted = False  # we performed a swap so we should check again
    return L

if __name__ == "__main__":
    import doctest
    doctest.testmod()
