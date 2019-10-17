import pymysql
import pymongo

def open_connection_mysql(database: str):
    """ Opens a connection to the MySQL database. Then returns the connection
    and the cursor. Instead of using DESCRIBE I used the DictCursor to fetch
    all data as dict.
    """
    conn = pymysql.connect(
        host="localhost",
        port=3306,
        user="root",
        passwd="",
        db=database
    )
    return conn, conn.cursor(pymysql.cursors.DictCursor)

def open_connection_mongodb(database: str, collection: str):
    """ Opens a connection to the MongoDB database. Then gets the datase and
    retrieve the collection. The client and collection pointers are returned.
    """
    client = pymongo.MongoClient()
    db = client[database]
    collection = db[collection]
    return client, collection


def fetch_all_data_as_dict(table: str, cur) -> list:
    query = "SELECT * FROM {}".format(table)
    cur.execute(query)
    return cur.fetchall()


def close_mysql_database(conn) -> None:
    """ Closes the database.
    """
    conn.commit()
    conn.close()


def insert_data_collection(data: list, collection) -> None:
    """ Insert the list of data in the MongoDB collection.
    """
    return collection.insert_many(data)


def migrate_mysql_to_mongo(database: str, table: str) -> None:
    """ Opens a connection to the MySQL and MongoDB database. Fetches the data
    from the MySQL table and inserts it into the same named MongoDB collection.
    """
    connection, cursor = open_connection_mysql(database)
    client, collection = open_connection_mongodb(database, table)
    data = fetch_all_data_as_dict(table, cursor)
    result = insert_data_collection(data, collection)
    print(result, result.inserted_ids)
    close_mysql_database(connection)


if __name__ == '__main__':
    # The MySQL statement DESCRIBE table_name reports the names, data types,
    # constraints, default values, and so on of all columns in the table. Write
    # a Python program that transfers all data from a MySQL table (designated by
    # the user) to a MongoDB document. The program must not modify timestamps.
    database = "exercise_01"
    table = "indexer"
    migrate_mysql_to_mongo(database, table)

