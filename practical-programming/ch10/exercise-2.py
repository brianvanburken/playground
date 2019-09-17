from io import StringIO
from typing import TextIO


def read_alkaline_metals(reader: TextIO) -> list:
    """
    Reads from file all the alkaline metals and return them as a list of lists

    >>> file = StringIO('''beryllium 4 9.012
magnesium 12 24.305
calcium 20 20.078
strontium 38 87.62
barium 56 137.327
radium 88 226''')

    >>> read_alkaline_metals(file)
    [['beryllium', '4', '9.012'], ['magnesium', '12', '24.305'], ['calcium', '20', '20.078'], ['strontium', '38', '87.62'], ['barium', '56', '137.327'], ['radium', '88', '226']]
    """

    alkaline_metals = []
    line = reader.readline()
    while line:
        metal = line.strip().split()
        alkaline_metals.append(metal)
        line = reader.readline()

    return alkaline_metals


if __name__ == '__main__':
    file = StringIO('''beryllium 4 9.012
magnesium 12 24.305
calcium 20 20.078
strontium 38 87.62
barium 56 137.327
radium 88 226''')
    print(read_alkaline_metals(file))
