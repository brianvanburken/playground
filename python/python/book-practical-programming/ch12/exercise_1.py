def complement(dna: str) -> str:
    """ Complements the DNA string.

    >>> complement('AATTGCCGT')
    'TTAACGGCA'
    """
    # create new complement string
    complement_dna = ''
    # loop through each character
    for ch in dna:
        # add for each character with its complement to the new string
        if ch == 'T':
            complement_dna += 'A'
        elif ch == 'A':
            complement_dna += 'T'
        elif ch == 'G':
            complement_dna += 'C'
        elif ch == 'C':
            complement_dna += 'G'
    # return the string
    return complement_dna
