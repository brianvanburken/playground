rat_1 = [1, 2, 3]
rat_2 = [3, 1, 4]

if rat_1[0] > rat_2[0]:
    print("Rat 1 weighed more than rat 2 on day 1.")
else:
    print("Rat 1 weighed less than rat 2 on day 1.")

if rat_1[0] > rat_2[0] and rat_1[-1] > rat_2[-1]:
    print("Rat 1 remained heavier than Rat 2.")
else:
    print("Rat 2 became heavier than Rat 1.")
