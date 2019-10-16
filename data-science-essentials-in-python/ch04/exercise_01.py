import pymysql
import nltk
from os import listdir
from os.path import isfile, join


def open_connection():
    """ Opens a connection to the database. Then returns the connection and
    the cursor.
    """
    conn = pymysql.connect(
        host="localhost",
        port=3306,
        user="root",
        passwd="",
        db="exercise_01"
    )
    return conn, conn.cursor()


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
    tokenizer = nltk.WordPunctTokenizer()
    return tokenizer.tokenize(doc)


def add_pos_to_words(words: list) -> list:
    """ Adds the POS to each word.

    >>> add_pos_to_words(['start'])
    [('start', 'VBP')]
    """
    return nltk.pos_tag(words)


def build_query(words: list, file: str, conn) -> None:
    """ Builds a MYSQL INSERT query to insert the list of data into the
    database. The query is returned.
    """
    values = [
        '("{}", "{}", {}, "{}")'.format(
            conn.escape_string(file),
            conn.escape_string(word),
            i,
            conn.escape_string(pos)
        )
        for i, (word, pos) in enumerate(words, start=1)
    ]
    query = "INSERT INTO indexer (file, word, ordinal, pos) VALUES "
    query += ','.join(values)
    return query


def execute(query: str, cur) -> None:
    """ Executes the query.
    """
    cur.execute(query)


def close_database(conn) -> None:
    """ Closes the database.
    """
    conn.commit()
    conn.close()


def index_file(file: str) -> list:
    conn, cur = open_connection()
    content = read_file(file)
    words = extract_words(content)
    words_with_pos = add_pos_to_words(words)
    print(words_with_pos)
    query = build_query(words_with_pos, file, conn)
    execute(query, cur)
    close_database(conn)


if __name__ == '__main__':
    file = input("Enter a file to index: ")
    index_file(file)

