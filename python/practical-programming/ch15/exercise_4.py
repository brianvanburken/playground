import unittest


# This definition is to satisfy the compiler.
def is_sorted(a: list) -> bool:
    return True


class TestIsSorted(unittest.TestCase):
    """ Test is_sorted """

    def test_is_sorted_empty(self):
        """ Test an empty list. """
        expected = True
        actual = is_sorted([])
        self.assertEqual(expected, actual)

    def test_is_sorted_single(self):
        """ Test single item. """
        expected = True
        actual = is_sorted([5])
        self.assertEqual(expected, actual)

    def test_is_sorted_dups(self):
        """ Test a list with duplicate values. """
        expected = True
        actual = is_sorted([1, 2, 3, 4, 5])
        self.assertEqual(expected, actual)

    def test_is_sorted_desc(self):
        """ Test a list in descending order. """
        expected = False
        actual = is_sorted([5, 4, 3, 2])
        self.assertEqual(expected, actual)

    def test_is_sorted_mixed(self):
        """ Test an unsorted list. """
        expected = False
        actual = is_sorted([1, 5, 2, 4])
        self.assertEqual(expected, actual)

unittest.main()
