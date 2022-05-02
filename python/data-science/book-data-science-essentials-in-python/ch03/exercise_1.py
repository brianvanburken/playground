from urllib.request import urlopen
from bs4 import BeautifulSoup


def download_page(url: str) -> str:
    """ Downloads the content of a page as HTML string.
    """
    try:
        with urlopen(url) as doc:
            return str(doc.read())
    except Exception:
        return None


def extract_links(content: str) -> list:
    """ Extracts all the links from the HTML page given.
    """
    soup = BeautifulSoup(content, 'lxml')
    return soup.find_all('a')


def extract_hrefs(links: list) -> list:
    """ Extracts the href and link content from list of links.
    """
    hrefs = []
    for link in links:
        hrefs.append((link.string, link["href"]))
    return hrefs


def test_links(hrefs: list, base: str) -> list:
    """ Loops through all links and checks if the link is broken. The function
    returns all the links found to be broken.
    """
    broken = []
    for (name, href) in hrefs:
        if is_broken_url(base + href):
            broken.append((name, href))
    return broken


def is_broken_url(url: str) -> bool:
    """ Checks if the given URL is broken.
    """
    try:
        with urlopen(url):
            return True
    except Exception:
        return False


def detect_broken_links(url: str) -> list:
    """ Given an URL, it downloads the page, extracts all the links, and checks
    each link if it is broken. The broken links are returned.
    """
    content = download_page(url)
    links = extract_links(content)
    hrefs = extract_hrefs(links)
    return test_links(hrefs, url)


if __name__ == "__main__":
    # Write a program that, given a URL of a web page, reports the names and
    # destinations of broken links in the page. For the purpose of this
    # exercise, a link is broken if an attempt to open it with
    # urllib.request.urlopen fails.
    url = 'http://www.deadlinkcity.com/'
    broken_links = detect_broken_links(url)
    print(broken_links)
