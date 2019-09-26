ch = 'T'
width = 7

# using rjust is cheating?
# for i in range(1, width + 1):
#     print((ch * i).rjust(width))

for i in range(1, width + 1):
    print((' ' * (width - i)) + (ch * i))
