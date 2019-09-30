from typing import Tuple, Set

Pair = Tuple[str, str]
Pairs = Set[Pair]


def mating_pairs(males: set, females: set) -> Pairs:
    """ Precondition: males length == females length
    Returns a set of mating pairs

    >>> males = set(['male gerbil 1', 'male gerbil 2'])
    >>> females = set(['female gerbil 1', 'female gerbil 2'])
    >>> mating_pairs(males, females)
    """
    pairs = set()
    while len(males) > 0:
        pairs.add((males.pop(), females.pop()))
    return pairs
