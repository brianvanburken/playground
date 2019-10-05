class Country:
    def __init__(self, name: str, population: int, area: int) -> None:
        """ Create a country.

        >>> canada = Country('Canada', 34482779, 9984670)
        >>> canada.name
        'Canada'
        >>> canada.population
        34482779
        >>> canada.area
        9984670
        """
        self.name = name
        self.population = population
        self.area = area

    def is_larger(self, other) -> bool:
        """ Returns if the country is larger than the other.

        >>> usa = Country('United States of America', 313914040, 9826675)
        >>> canada = Country('Canada', 34482779, 9984670)
        >>> canada.is_larger(usa)
        True
        """
        return self.area > other.area

    def population_density(self) -> float:
        """ Returns the population density of the country.

        >>> canada = Country('Canada', 34482779, 9984670)
        >>> canada.population_density()
        3.4535722262227995
        """
        return self.population / self.area

    def __str__(self) -> str:
        """ Creates a string human readable representation of the country.

        >>> usa = Country('United States of America', 313914040, 9826675)
        >>> print(usa)
        United States of America has a population of 313914040 and is 9826675 square km.
        """
        return '{0} has a population of {1} and is {2} square km.' \
            .format(self.name, self.population, self.area)

    def __repr__(self) -> str:
        """ Creates a string representation of the country.

        >>> canada = Country('Canada', 34482779, 9984670)
        >>> canada
        Country('Canada', 34482779, 9984670)
        >>> [canada]
        [Country('Canada', 34482779, 9984670)]
        """
        return "Country('{0}', {1}, {2})" \
            .format(self.name, self.population, self.area)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
