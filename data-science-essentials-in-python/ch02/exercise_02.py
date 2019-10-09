import re
import pickle
from os import listdir
from os.path import isfile, join


def find_files(path: str) -> list:
    """ Returns all the files in a directory.

    >>> find_files("/test")
    ["example.txt"]
    """
    return [file for file in listdir(path) if isfile(join(path, file))]


def read_file(filename: str) -> str:
    """ Returns the contents of a file.

    >>> read_file("example.txt")
    "Hello, world!"
    """
    with open(filename) as file:
        return str(file.read().strip())
    return ""


def extract_words(doc: str) -> list:
    """ Extracts all the words from the given string.

    >>> extract_words('hello world this is a test')
    ['hello', 'world', 'this', 'is', 'a', 'test']
    """
    return re.findall(r"\w+", doc, re.I)


def pickle_index(index: dict) -> dict:
    """ Store index as pickle.
    """
    with open("./index.pickle", 'wb') as file:
        pickle.dump(index, file)
    return index


def index_files(path: str) -> dict:
    """ Creates an index of all the files in a directory. The index is created
    with the content of the file if readable.
    """
    files = find_files(path)
    index = {}
    for filename in files:
        content = read_file(join(path, filename))
        words = extract_words(content)
        for word in words:
            index[word] = index.get(word, [])
            index[word].append(filename)
            index[word] = list(set(index[word]))
    return pickle_index(index)


if __name__ == "__main__":
    # Write a program that indexes all files in a certain user-designated
    # directory (folder). The program should construct a dictionary where the
    # keys are all unique words in all the files (as described by the regular
    # expression r"\w+"; treat the words as case-insensitive), and the value of
    # each entry is a list of file names that contain the word. For instance,
    # if the word “aloha” is mentioned in the files “early-internet.dat” and
    # “hawaiian-travel.txt,” the dictionary will have an entry {..., ’aloha’:
    # [’early-internet.dat’, ’hawaiian-travel.txt’], ...}
    path = input("Give the absolute path to the directory to index: ")
    print(index_files(path))
