import sqlite3

con = sqlite3.connect('exercise_3.db')
cur = con.cursor()

# Create database if not exsits
cur.execute('CREATE TABLE IF NOT EXISTS Numbers (Val INTEGER)')

# Empty database in case it exists
cur.execute('DELETE FROM Numbers')

cur.execute('INSERT INTO Numbers VALUES(1)')
cur.execute('INSERT INTO Numbers VALUES(2)')

con.commit()

cur.execute('SELECT * FROM Numbers WHERE 1/0')
print(cur.fetchall(), end='\n\n')

cur.execute('SELECT * FROM Numbers WHERE 1/0 AND Val > 0')
print(cur.fetchall(), end='\n\n')

cur.execute('SELECT * FROM Numbers WHERE Val > 0 AND 1/0')
print(cur.fetchall(), end='\n\n')

con.close()
