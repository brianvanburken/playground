units = [
    ['km', 'miles', 'league'],
    ['kg', 'pound', 'stone']
]
units[0] == ['km', 'miles', 'league']
units[1] == ['kg', 'pound', 'stone']
units[0][0] == 'km'
units[1][0] == 'kg'
units[0][1:3] == ['miles', 'league']
units[1][0:2] == ['kg', 'pound']
