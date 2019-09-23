def averageOfThree(grade1: int, grade2: int, grade3: int) -> int:
    """Calculates the average between the three grades given.

    >>> average(1, 1, 1)
    1
    >>> average(1, 50, 99)
    50
    >>> average(71, 43, 92)
    68
    """

    return (grade1 + grade2 + grade3) // 3
