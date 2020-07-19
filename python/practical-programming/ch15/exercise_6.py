import unittest
from typing import List


def average(values: List[float]) -> float:
    """ Returns the average of the numbers in values. Some items in values are
    None, and they are not counted towards the average.

    >>> average([20, 30])
    25.0
    >>> average([None, 20, 30])
    25.0
    """
    count = 0  # The number of values seen so far.
    total = 0  # The sum of values seen so far.

    for value in values:
        if value is not None:
            total += value
            # The fix is indenting the following line. It should only up the
            # count if the value should count towards the average.
            count += 1

    # Second check if we do have a count. Else we get a division by zero error.
    if count == 0:
        return 0.0
    else:
        return total / count


class TestAverage(unittest.TestCase):
    """ Test average """

    def test_average_empty(self):
        """ Test an empty list. """
        expected = 0.0
        actual = average([])
        self.assertEqual(expected, actual)

    def test_average_single(self):
        """ Test an single item. """
        expected = 5.0
        actual = average([5])
        self.assertEqual(expected, actual)

    def test_average_single_none(self):
        """ Test an single item that is None. """
        expected = 0.0
        actual = average([None])
        self.assertEqual(expected, actual)

    def test_average_two_with_none(self):
        """ Test two items, one is None. """
        expected = 5.0
        actual = average([5.0, None])
        self.assertEqual(expected, actual)

    def test_average_multi_numbers(self):
        """ Test multiple items. """
        expected = 15.0
        actual = average([15, 26, 32, -13])
        self.assertEqual(expected, actual)

    def test_average_multi_numbers_with_none(self):
        """ Test multiple items with None's. """
        expected = 15.0
        actual = average([15, 26, 32, None, -13, None])
        self.assertEqual(expected, actual)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
    unittest.main()
