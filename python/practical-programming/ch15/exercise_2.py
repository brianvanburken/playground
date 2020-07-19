import unittest


# This definition is to satisfy the compiler
def line_intersect(a: any) -> any:
    return a


class TestLineIntersect(unittest.TestCase):
    """ Tests for line_intersect. """

    def test_distant_pair(self):
        """ Test an distant pair non intersecting lines. """
        argument = [[5, 0], [6, 5]]
        expected = None
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

    def test_coincident(self):
        """ Test a coincident lines. """
        argument = [[0, 0], [10, 0]]
        expected = [[0, 0]]
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

    def test_intersection(self):
        """ Test an intersection. """
        argument = [[0, 0], [1, 3]]
        expected = [[0.75, 2.25]]
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

    def test_parallel(self):
        """ Test parallel lines. """
        argument = [[0, 0], [2, 0]]
        expected = None
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

    def test_non_distinct(self):
        """ Test non-distinct lines. """
        argument = [[0, 0], [0, 0]]
        expected = None
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

    def test_distinct(self):
        """ Test distinct lines. """
        argument = [[2, 5], [3, 2]]
        expected = None
        actual = line_intersect(argument)
        self.assertEqual(expected, actual)

unittest.main()
