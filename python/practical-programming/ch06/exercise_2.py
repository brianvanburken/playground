from calendar import isleap, leapdays, weekday


def next_leap_year(current_year: int) -> int:
    """ Calculates the next leap year from the year given.

    >>> next_leap_year(2019)
    2020
    >>> next_leap_year(2020)
    2020
    >>> next_leap_year(2021)
    2024
    """
    for year in range(current_year, current_year + 10):
        if isleap(year):
            return year
    return 0

print(next_leap_year(2019))
print(leapdays(1999, 2051))
print(weekday(2016, 6, 29))
