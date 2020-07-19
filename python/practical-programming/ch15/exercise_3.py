import unittest


# This definition is to satisfy the compiler.
def all_prefixes(s: str) -> set:
    return set()


class TestAllPrefixes(unittest.TestCase):
    """ Test all_prefixes """

    def test_empty_string(self):
        """ Test an empty string. """
        expected = set()
        actual = all_prefixes("")
        self.assertEqual(expected, actual)

    def test_single_char(self):
        """ Test a single character string """
        expected = {'s'}
        actual = all_prefixes("s")
        self.assertEqual(expected, actual)

    def test_multi_char_string(self):
        """ Test a normal string. """
        expected = {'b', 'br', 'bri', 'bria', 'brian'}
        actual = all_prefixes("brian")
        self.assertEqual(expected, actual)

    def test_similar_chars(self):
        """ Test non-distinct first letter. """
        expected = {'p', 'pu', 'pup', 'pupp', 'puppe', 'puppet', 'pp', 'ppe',
                    'ppet', 'pe', 'pet'}
        actual = all_prefixes("puppet")
        self.assertEqual(expected, actual)
