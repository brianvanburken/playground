fruit = 'pineapple'
fruit.find('p', fruit.count('p')) == 5  # count, find
fruit.count(fruit.upper().swapcase()) == 1  # upper, swapcase, count
fruit.replace(fruit.swapcase(), fruit.lower()) \
    == fruit  # swapcase, lower, replace
