import sqlite3

con = sqlite3.connect('census.db')
cur = con.cursor()

# Create database if not exsits
cur.execute('''
CREATE TABLE IF NOT EXISTS Density (name TEXT, population INTEGER, area REAL)
''')

# Empty database in case it exists
cur.execute('DELETE FROM Density')

# Insert data
data = [
    ('Newfoundland and Labrador', 512930, 370501.69),
    ('Prince Edward Island', 135294, 5684.39),
    ('Nova Scotia', 908007, 52917.43),
    ('New Brunswick', 729498, 71355.67),
    ('Quebec', 7237479, 1357743.08),
    ('Ontario', 11410046, 907655.59),
    ('Manitoba', 1119583, 551937.87),
    ('Saskatchewan', 978933, 586561.35),
    ('Alberta', 2974807, 639987.12),
    ('Britisch Columbia', 3907738, 926492.48),
    ('Yukon Territory', 28674, 474706.97),
    ('Northwest Territories', 37360, 1141108.37),
    ('Nunavut', 26745, 1925460.18),
]
for row in data:
    cur.execute('INSERT INTO Density VALUES(?, ?, ?)', row)

con.commit()

# Retrieve contents of the table
cur.execute('SELECT * FROM Density')
print(cur.fetchall(), end='\n\n')

# Retrieve the populations
cur.execute('SELECT population FROM Density')
print(cur.fetchall(), end='\n\n')

# Retrieve provinces that have populations of less than 1 million
cur.execute('SELECT name FROM Density WHERE population < 1000000')
print(cur.fetchall(), end='\n\n')

# Retrieve provinces that have populations of less than 1 million
cur.execute('SELECT name FROM Density WHERE population < 1000000')
print(cur.fetchall(), end='\n\n')

# Retrieve provinces that have populations of less than 1 million or greater
# than 5 million.
cur.execute('''
SELECT name FROM Density WHERE population < 1000000 OR population > 5000000
''')
print(cur.fetchall(), end='\n\n')

# Retrieve population of provinces that have a land area greater than 200,000
# square kilometers.
cur.execute('SELECT population FROM Density WHERE area > 200000')
print(cur.fetchall(), end='\n\n')

# Retrieve the provinces along with their population density.
cur.execute('SELECT name, (population / area) FROM Density')
print(cur.fetchall(), end='\n\n')

con.close()
