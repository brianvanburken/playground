rat_1_weight = 1
rat_2_weight = 1.5
rat_1_rate = 4
rat_2_rate = 1

rat_1_target_weight = rat_1_weight * 1.25
weeks = 0
rat_1_progress_weight = rat_1_weight
while rat_1_progress_weight <= rat_1_target_weight:
    rat_1_progress_weight = rat_1_progress_weight * (1 + (rat_1_rate / 100))
    weeks += 1
print(
    'Rat 1 will become 25% heaver than originally in {0} weeks'.format(
        weeks
    )
)

weeks = 0
rat_1_progress_weight = rat_1_weight
rat_2_progress_weight = rat_2_weight
while rat_1_progress_weight < (rat_2_progress_weight * 1.10):
    rat_1_progress_weight = rat_1_progress_weight * (1 + (rat_1_rate / 100))
    rat_2_progress_weight = rat_2_progress_weight * (1 + (rat_2_rate / 200))
    weeks += 1

print(
    'Rat 1 will be 10% heavier than rat 2 in {0} weeks'.format(
        weeks
    )
)
