import json
import nltk
from nltk.corpus import stopwords
from collections import Counter
from urllib.request import urlopen
from urllib.parse import quote
from bs4 import BeautifulSoup


def build_wikimedia_url(title: str) -> str:
    """ Builds a WikiMedia JSON API url for the given page title.
    """
    title = quote(title)
    api_url = "https://en.wikipedia.org/w/api.php"
    return "{}?action=parse&page={}&format=json".format(api_url, title)


def download_json(url: str) -> dict:
    """ Downloads the JSON from the API
    """
    try:
        with urlopen(url) as doc:
            return json.loads(doc.read())
    except Exception:
        return {}


def extract_wikimedia_page(json: dict) -> str:
    """ Returns the content from the WikiMedia JSON dictonary. If the structure
    can't be traversed, it will return an empty string.
    """
    return json \
        .get('parse', {}) \
        .get('text', {}) \
        .get('*', '')


def extract_text(html: str) -> str:
    """ Extract the text from the HTML string.

    >>> extract_text('<div>Hello</div>')
    'Hello'
    """
    soup = BeautifulSoup(html, "lxml")
    return soup.text.strip()


def extract_words(text: str) -> list:
    """ Extracts all the words from the given text.

    >>> extract_words('Hello world')
    ['Hello', 'world']
    """
    words = nltk.word_tokenize(text)
    return [w.lower() for w in words]


def eliminate_stop_words(words: list) -> list:
    """ Removes stopwords from the list of words.

    >>> eliminate_stop_words(['the', 'hello', 'world'])
    ['hello', 'world']
    """
    return [
        w for w in words
        if w not in stopwords.words("english") and w.isalnum()
    ]


def extract_stems(words: list) -> list:
    """ Extracts the stem from each word in the list.
    """
    ls = nltk.LancasterStemmer()
    return map(ls.stem, words)


def most_common_ten(stems: list) -> list:
    """ Counts each stem and returns the ten most common.
    """
    frequency = Counter(stems)
    return frequency.most_common(10)


def most_frequent_stems(title: str) -> list:
    """ Extracts the ten most frequent stems from the WikiMedia page given.
    """
    url = build_wikimedia_url(title)
    json = download_json(url)
    html = extract_wikimedia_page(json)
    text = extract_text(html)
    all_words = extract_words(text)
    words = eliminate_stop_words(all_words)
    stems = extract_stems(words)
    return most_common_ten(stems)


if __name__ == "__main__":
    # MediaWiki (a Wikimedia project[13]) provides a JSON-based API that
    # enables programmable access to Wikipedia data and metadata. Write a
    # program that reports ten most frequently used stems in the Wikipedia page
    # titled “Data science.
    title = "Data science"
    print(most_frequent_stems(title))
