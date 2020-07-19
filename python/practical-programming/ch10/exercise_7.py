from typing import TextIO
from io import StringIO


def read_molecule(reader: TextIO) -> list:
    """Read a single molecule from reader and return it, or return None to
    signal end of file.  The first item in the result is the name of the
    compound; each list contains an atom type and the X, Y, and Z coordinates
    of that atom.
    >>> instring = 'COMPND TEST  \\  nATOM 1 N 0.1 0.2 0.3  \\  nATOM 2 N 0.2 0.1 0.0  \\  nEND  \\  n'
    >>> infile = StringIO(instring)
    >>> read_molecule(infile)
    ['TEST', ['N', '0.1', '0.2', '0.3'], ['N', '0.2', '0.1', '0.0']]
    """
    # If there isn't another line, we're at the end of the file.
    line = reader.readline()
    if not line:
        return None
    # Name of the molecule: "COMPND   name"
    parts = line.split()
    name = parts[1]
    # Other lines are either "END" or "ATOM num atom_type x y z" 
    molecule = [name]
    reading = True
    while reading:
        line = reader.readline()
        if line.startswith('END'):
            reading = False
        elif not (line.startswith('CMNT') or line.isspace() or line.startswith('\t')):
            _, _, atom_type, x, y, z = line.split()
            if molecule is None:
                molecule = []
                molecule.append([atom_type, x, y, z])
    return molecule
