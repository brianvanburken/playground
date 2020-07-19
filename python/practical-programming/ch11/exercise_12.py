def sparse_add(vector1: dict, vector2: dict) -> dict:
    """ Takes two sparse vectors and returns the sum of the two.

    >>> a = {0: 1, 2: 3}
    >>> b = {0: 1, 5: 9}
    >>> sparse_add(a, b)
    {0: 2, 2: 3, 5: 9}
    """
    summed = vector1
    for key, value in vector2.items():
        summed[key] = summed.get(key, 0) + value
    return summed


def sparse_dot(vector1: dict, vector2: dict) -> dict:
    """ Takes two sparse vectors and returns the sum of the two.

    >>> a = {0: 1, 2: 3}
    >>> b = {0: 1, 2: 4, 5: 9}
    >>> sparse_dot(a, b)
    13
    """
    total = 0
    for key, value in vector1.items():
        total += value * vector2.get(key, 0)
    return total

a = {0: 1, 2: 3}
b = {0: 1, 5: 9}
print(sparse_add(a, b))

a = {0: 1, 2: 3}
b = {0: 1, 2: 4, 5: 9}
print(sparse_dot(a, b))
