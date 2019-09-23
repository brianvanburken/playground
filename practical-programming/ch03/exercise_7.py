from exercise_6 import *


def averageOfFour(grade1: int, grade2: int, grade3: int, grade4: int) -> int:
    """Calculates the average between the four best grades given.

    >>> average(1, 1, 1, 1)
    1
    >>> average(1, 50, 99, 1)
    50
    >>> average(71, 43, 92, 2)
    68
    """
    # First sort all grades from smallest to biggest
    grades = [grade1, grade2, grade3, grade4]
    best_grades = sorted(grades, reverse=True)[:3]
    return averageOfThree(best_grades[0], best_grades[1], best_grades[2])
