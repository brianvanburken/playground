def find_min_max(values: list) -> None:
    """ Print the minimum and maximum value from values.

    >>> find_min_max([])
    The minimum value is None
    The maximum value is None
    >>> find_min_max([1])
    The minimum value is 1
    The maximum value is 1
    >>> find_min_max([2, 1, 5, 6, 3])
    The minimum value is 1
    The maximum value is 6
    """

    # These two lines should not be None since you can't compare those with
    # the values in the list. Fix is setting them to the first value and check
    # if the list is larger than zero
    min = None
    max = None

    if len(values) > 0:
        min = values[0]
        max = values[0]

        for value in values:
            if value > max:
                max = value
            if value < min:
                min = value

    print('The minimum value is {0}'.format(min))
    print('The maximum value is {0}'.format(max))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
