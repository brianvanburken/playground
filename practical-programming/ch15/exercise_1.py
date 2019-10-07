import unittest
from typing import List


def double_preceding(values: List[float]) -> None:
    """ Replace each item in the list with twice the value of preceding item,
    and replace the first item with 0.

    >>> L = [1, 2, 3]
    >>> double_preceding(L)
    >>> L
    [0, 2, 4]
    """
    if values != []:
        temp = values[0]
        values[0] = 0

    for i in range(1, len(values)):
        values[i], temp = 2 * temp, values[i]
        # the bugs was that the value was changed and then set as temp
        # the fix was setting the temp before the value gets changed


class TestDoublePreceding(unittest.TestCase):
    """ Tests for double_preceding. """

    def test_double_preceding_empty(self):
        """ Test an empty list. """
        argument = []
        expected = []
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_zero(self):
        """ Test single zero. """
        argument = [0]
        expected = [0]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_one(self):
        """ Test a one-item list. """
        argument = [2]
        expected = [0]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_two(self):
        """ Test two positive items. """
        argument = [2, 3]
        expected = [0, 4]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_neg(self):
        """ Test single negative item. """
        argument = [-5]
        expected = [0]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_two_neg(self):
        """ Test two negative items. """
        argument = [-6, -3]
        expected = [0, -12]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_neg_pos(self):
        """ Test positive and negative items. """
        argument = [3, -12, 1]
        expected = [0, 6, -24]
        double_preceding(argument)
        self.assertEqual(expected, argument)

    def test_double_preceding_all_zeros(self):
        """ Test all zeros items. """
        argument = [0, 0, 0]
        expected = [0, 0, 0]
        double_preceding(argument)
        self.assertEqual(expected, argument)

unittest.main()
