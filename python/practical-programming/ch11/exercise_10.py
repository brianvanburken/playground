def db_headings(data: dict) -> set:
    """ Returns a set of keys used in any of the inner dictionaries.

    >>> a = {'a': {'b': 1, 'c': False}, 'b': {'b': 2, 'c': True, 'd': 3}}
    >>> db_headings(a)
    {'b', 'c'}
    """
    keys = set()
    for subdict in data.values():
        subkeys = set(subdict.keys())
        if len(keys) == 0:
            keys = subkeys
        else:
            keys = subkeys.intersection(keys)
    return keys


# example = {
#     'jgoodall': {
#         'surname': 'Goodall',
#         'forename': 'Jane',
#         'born': 1934,
#         'died': None,
#         'notes': 'primate researcher',
#         'author': ['In the Shadow of Man', 'The Chimpanzees of Gombe']
#     },
#     'rfranklin': {
#         'surname': 'Franklin',
#         'forename': 'Rosalind',
#         'born': 1920,
#         'died': 1957,
#         'notes': 'contributed to discovery of DNA'
#     },
#     'rcarson': {
#         'surname': 'Carson',
#         'forename': 'Rachel',
#         'born': 1907,
#         'died': 1964,
#         'notes': 'raised awareness of effects of DDT',
#         'author': ['Silent Spring']
#     }
# }
# print(db_headings(example))
