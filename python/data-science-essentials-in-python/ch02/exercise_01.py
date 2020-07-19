from urllib import request
from collections import Counter
import re


def download(url: str) -> str:
    """ Downloads the content of a page as HTML string.
    """
    try:
        with request.urlopen(url) as doc:
            return str(doc.read())
    except Exception:
        return ""


def extract_words(doc: str) -> list:
    """ Extracts all the words from the given string.

    >>> extract_words('hello world this is a test')
    ['hello', 'world', 'this', 'is', 'a', 'test']
    """
    return re.findall(r"\w+", doc, re.I)


def count_words(words: list) -> list:
    """ Gives back a list of counted words.

    >>> count_words(['hello', 'hello', 'world'])
    [('hello', 2), ('world', 1)]
    """
    return Counter(words).most_common(10)


def top_10_words(url: str) -> dict:
    """ Takes an url and returns and returns a dictionary of the top 10 words on
    that page.
    """
    content = download(url)
    words = extract_words(content)
    return count_words(words)


if __name__ == "__main__":
    # Write a program that downloads a web page requested by the user and
    # reports up to ten most frequently used words. The program should treat
    # all words as case-insensitive. For the purpose of this exercise, assume
    # that a word is described by the regular expression r"\w+".
    url = input("Give an URL: ")
    print(top_10_words(url))
