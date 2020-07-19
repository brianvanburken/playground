import re
import pickle
from os import listdir
from os.path import isfile, join


def read_file(filename: str) -> str:
    """ Returns the contents of a file.

    >>> read_file("example.txt")
    "Hello, world!"
    """
    with open(filename) as file:
        return str(file.read().strip())
    return ""


def extract_numbers(doc: str) -> list:
    """ Extracts all the numbers from the given string.
    """
    numbers_regex = r"(\+?[0-9\(\)\-\s]+)"
    return re.findall(numbers_regex, doc)


def sanitize(number: str) -> str:
    """ Removes all formatting
    """
    return number \
        .replace("+", "00") \
        .replace("(", "") \
        .replace(")", "") \
        .replace("-", "") \
        .replace(" ", "") \
        .strip()


def is_phone_number(number: str) -> bool:
    return len(number) >= 7


def extract_phone_numbers_from_file(path: str) -> dict:
    content = read_file(path)
    numbers = extract_numbers(content)
    normalized = map(sanitize, numbers)
    return list(filter(is_phone_number, normalized))


if __name__ == "__main__":
    # Write a program that extracts all phone numbers from a given text file.
    # This is not an easy task, as there are several dozens of national
    # conventions for writing phone numbers (see
    # https://en.wikipedia.org/wiki/National_conventions_for_writing_telephone_numbers
    # ). Can you design one regular expression that catches them all?
    path = input("Enter path to file: ")
    print(extract_phone_numbers_from_file(path))
