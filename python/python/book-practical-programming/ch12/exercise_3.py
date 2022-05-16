from typing import TextIO


def hopedale_average(file_name: str) -> int:
    """ Returns the average of pelts per year from the Hopedale data set.

    >>> hopedale_average('./hopedale.txt')
    41.44444444444444
    """
    total_pelts = 0
    years = 1
    with open(file_name) as reader:
        reader.readline()  # skip header
        # remove comments/metadata
        line = reader.readline().strip()
        while line.startswith('#'):
            line = reader.readline().strip()
        # line is now the first data
        total_pelts += int(line)
        for line in reader:
            total_pelts += int(line.strip())  # add data to sum
            years += 1  # up number of years
    # divide sum by number of years
    return total_pelts / years

print(hopedale_average('./hopedale.txt'))
