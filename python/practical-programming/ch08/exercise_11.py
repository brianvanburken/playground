units = [
    ['km', 'miles', 'league'],
    ['kg', 'pound', 'stone']
]
units[-2] == ['km', 'miles', 'league']
units[-1] == ['kg', 'pound', 'stone']
units[-2][-3] == 'km'
units[-1][-3] == 'kg'
units[-2][-2:] == ['miles', 'league']
units[-1][-3:-1] == ['kg', 'pound']
