from typing import List


def remove_neg(num_list: List[float]) -> None:
    """ Remove the negative numbers from the list num_list.

    >>> numbers = [-5, 1, -3, 2]
    >>> remove_neg(numbers)
    [1, 2]
    """
    result = []
    for i in num_list:
        if i >= 0:
            result.append(i)
    return result
