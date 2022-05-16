class Nematode:
    """ Information about C. elegans"""

    def __init__(self, body_length: int, gender: str, age: int) -> None:
        """ Represents a Nematode

        >>> nematode = Nematode(1, 'hermaphrodite', 30)
        >>> nematode.body_length
        1
        >>> nematode.gender
        'hermaphrodite'
        >>> nematode.age
        30
        """
        self.body_length = body_length
        self.gender = gender
        self.age = age

    def __str__(self) -> str:
        """ Returns a string representation of the Nematode.

        >>> nematode = Nematode(1, 'hermaphrodite', 30)
        >>> nematode.__str__()
        'This nematode is 1 mm in length is a hermaphrodite and is 30 days old.'
        """
        return 'This nematode is {} mm in length is a {} and is {} days old.' \
            .format(self.body_length, self.gender, self.age)

    def __repr__(self) -> str:
        """ Returns a string representation of the Nematode.

        >>> nematode = Nematode(1, 'hermaphrodite', 30)
        >>> nematode
        Nematode(1, 'hermaphrodite', 30)
        """
        return "Nematode({}, '{}', {})" \
            .format(self.body_length, self.gender, self.age)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
