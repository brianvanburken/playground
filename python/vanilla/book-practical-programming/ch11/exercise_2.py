from typing import TextIO, Tuple, Dict
from io import StringIO

Atom = Tuple[str, Tuple[str, str, str]]
CompoundDict = Dict[str, Atom]


def read_molecule(reader: TextIO) -> CompoundDict:
    """Read a single molecule from reader and return it, or return None to
    signal end of file. The returned dictionary has one key/value pair where
    the key is the name of the compound and the value is a list of Atoms.
    >>> instring = 'COMPND TEST\\nATOM 1 N 0.1 0.2 0.3\\nATOM 2 N 0.2 0.1 0.0\\nEND\\n'
    >>> infile = StringIO(instring)
    >>> read_molecule(infile)
    {'TEST': [('N', ('0.1', '0.2', '0.3')), ('N', ('0.2', '0.1', '0.0'))]}
    """
    line = reader.readline()
    if not line:
        return None
    print(line.strip())
    [_, name] = line.strip().split()
    molecule = {}
    molecule[name] = []
    while line:
        line = reader.readline()
        if line.startswith('ATOM'):
            [_, _, atom_type, x, y, z] = line.split()
            molecule[name].append((atom_type, (x, y, z,),))
        elif line.startswith('END'):
            return molecule
    return molecule


def read_all_molecules(reader: TextIO) -> CompoundDict:
    """Read zero or more molecules from reader, returning a list of the
    molecule information.
    >>> cmpnd1 = 'COMPND T1\\nATOM 1 N 0.1 0.2 0.3\\nATOM 2 N 0.2 0.1 0.0\\nEND\\n'
    >>> cmpnd2 = 'COMPND T2\\nATOM 1 A 0.1 0.2 0.3\\nATOM 2 A 0.2 0.1 0.0\\nEND\\n'
    >>> infile = StringIO(cmpnd1 + cmpnd2)
    >>> result = read_all_molecules(infile)
    >>> result['T1']
    [('N', ('0.1', '0.2', '0.3')), ('N', ('0.2', '0.1', '0.0'))]
    >>> result['T2']
    [('A', ('0.1', '0.2', '0.3')), ('A', ('0.2', '0.1', '0.0'))]
    """
    molecules = {}
    reading = True
    while reading:
        molecule = read_molecule(reader)
        if molecule:
            molecules = {**molecules, **molecule}
        else:
            reading = False
    return molecules

# cmpnd1 = 'COMPND T1\nATOM 1 N 0.1 0.2 0.3\nATOM 2 N 0.2 0.1 0.0\nEND\n'
# cmpnd2 = 'COMPND T2\nATOM 1 A 0.1 0.2 0.3\nATOM 2 A 0.2 0.1 0.0\nEND\n'
# infile = StringIO(cmpnd1 + cmpnd2)
# result = read_molecule(infile)
# print(result)
