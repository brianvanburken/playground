'hello'.upper() == 'HELLO'
'Happy Birthday!'.lower() == 'happy birthday!'
'WeeeEEEEeeeEEEEeee'.swapcase() == 'wEEEeeeeEEEeeeeEEE'
'ABC123'.isupper() is True
'aeiouAEIOU'.count('a') == 1
'hello'.endswith('o') is True
'hello'.startswith('H') is False
'Hello {0}'.format('Python') == 'Hello Python'
'Hello {0}! Hello {1}!'.format('Python', 'World') \
    == 'Hello Python! Hello World!'
