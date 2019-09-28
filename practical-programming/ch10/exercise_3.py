from io import StringIO
from typing import TextIO


def read_backwards(reader: TextIO) -> list:
    """ Returns the lines of the file backwards

    >>> file = StringIO('''beryllium 4 9.012
    magnesium 12 24.305
    calcium 20 20.078
    strontium 38 87.62
    barium 56 137.327
    radium 88 226''')
    >>> read_backwards(file)
    ['radium 88 226.0', 'barium 56 137.327', 'strontium 38 87.62',
        'calcium 20 20.078', 'magnesium 12 24.305', 'beryllium 4 9.012']
    """
    lines = reader.readlines()
    return lines.reverse()

if __name__ == '__main__':
    file = StringIO('''beryllium 4 9.012
magnesium 12 24.305
calcium 20 20.078
strontium 38 87.62
barium 56 137.327
radium 88 226''')
    print(read_backwards(file))
