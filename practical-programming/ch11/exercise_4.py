from io import StringIO
from typing import TextIO


def read_authors_from_files(files: list) -> list:
    """ Reads all authors from a list of PDB files.

    >>> read_authors(['authors.pdb'])
    {'bob', 'john', 'jack'}
    """
    all_authors = set()
    for file in files:
        with open(file) as reader:
            authors = read_authors(reader)
            all_authors = all_authors.union(authors)
    return all_authors


def read_authors(reader: TextIO) -> set:
    """ Reads all authors from a file.

    >>> instring = 'AUTHOR bob\nAuthor jack\n\nauthor\tjohn\n'
    >>> infile = StringIO(instring)
    >>> read_authors(infile)
    {'bob', 'jack', 'john'}
    """
    authors = set()
    lines = reader.readlines()
    for line in lines:
        line = line.strip()
        if line.upper().startswith('AUTHOR'):
            authors.add(line[6:].strip())
    return authors
