ch = 'T'
width = 7

i = 1
while i <= width:
    print(ch * i)
    i += 1

i = 1
while i <= width:
    print((' ' * (width - i)) + (ch * i))
    i += 1
