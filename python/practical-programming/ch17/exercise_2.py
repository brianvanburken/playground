import sqlite3

con = sqlite3.connect('census.db')
cur = con.cursor()

# Create database if not exsits
cur.execute('''
CREATE TABLE IF NOT EXISTS Captitals
(name TEXT, capital TEXT, population INTEGER)
''')

# Empty database in case it exists
cur.execute('DELETE FROM Captitals')

data = [
    ('Newfoundland and Labrador', "St. John's", 172918),
    ('Prince Edward Island', 'Charlottetown', 58358),
    ('Nova Scotia', 'Halifax', 359183),
    ('New Brunswick', 'Frederiction', 81346),
    ('Quebec', 'Quebec city', 682757),
    ('Ontario', 'Toronto', 4682897),
    ('Manitoba', 'Winnipeg', 671274),
    ('Saskatchewan', 'Regina', 192800),
    ('Alberta', 'Edmonton', 937845),
    ('Britisch Columbia', 'Victoria', 311902),
    ('Yukon Territory', 'Whitehorse', 21405),
    ('Northwest Territories', 'Yellowknife', 16541),
    ('Nunavut', 'Iqaluit', 5236),
]
for row in data:
    cur.execute('INSERT INTO Captitals VALUES (?, ?, ?)', row)

con.commit()

# Retrieve contents of the table
cur.execute('SELECT * FROM Captitals')
print(cur.fetchall(), end='\n\n')

# Retrieve the populations of the provinces and capitals
cur.execute('''
SELECT D.population, C.population
FROM Captitals as C, Density as D
''')
print(cur.fetchall(), end='\n\n')

# Retrieve the land area of provinces whose capitals have populations greater
# than 100,000
cur.execute('''
SELECT DISTINCT D.area
FROM Captitals as C, Density as D
WHERE C.population > 100000
''')
print(cur.fetchall(), end='\n\n')

# Retrieve the provinces with land densities less than two people per square
# kilometer and capital city populations more than 500,000
cur.execute('''
SELECT DISTINCT D.name
FROM Captitals as C, Density as D
WHERE (D.population / D.area) < 2.0
AND C.population > 500000
''')
print(cur.fetchall(), end='\n\n')

# Retrieve the total land area of Canada
cur.execute('SELECT SUM(area) FROM Density')
print(cur.fetchall(), end='\n\n')

# Retrieve the average capital city population
cur.execute('SELECT AVG(population) FROM Captitals')
print(cur.fetchall(), end='\n\n')

# Retrieve the lowest capital city population
cur.execute('SELECT MIN(population) FROM Captitals')
print(cur.fetchall(), end='\n\n')

# Retrieve the highest province/territory population
cur.execute('SELECT MAX(population) FROM Density')
print(cur.fetchall(), end='\n\n')

# Retrieve the provinces that have land densities within 0.5 persons per square
# kilometer of on another—have each pair of provinces reported only once
cur.execute('''
SELECT A.name, B.name
FROM Density AS A
INNER JOIN Density AS B
WHERE (ABS(A.population / A.area) - (B.population / B.area)) <= 0.5
AND (A.name < B.name)
''')
print(cur.fetchall(), end='\n\n')

con.close()
