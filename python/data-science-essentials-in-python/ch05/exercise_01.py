import numpy as np

# Partial sums are a rough equivalent of an integral. In fact, calculus defines
# an integral as an infinite sum of infinitesimal elements. Partial differences
# arri+1-arri are a rough equivalent of a derivative. numpy doesn’t provide a
# tool for calculating partial array differences. Write a program that, given an
# array arr, calculates the partial differences of the array items. Assume that
# the array is numerical.

numbers = range(1, 11) # create an array with 10 numbers from 1 to 10 inclusive
arr = np.array(numbers, copy=True) # place numbers in numpy
np.random.shuffle(arr) # shuffle array so the diffing doesn't only returning ones
# shifts the arrays so we can compare each with then next. See below for an example
# original: [1, 5, 4, 3]
# Calculation:
# [5,  4,  3] <- first part is shifted to right
#  -   -   -
# [1,  5,  4] <- second part is shifted to left
#  =   =   =
# [4, -1, -1]
diff = arr[1:] - arr[:-1]
print(arr[1:], arr[:-1], diff)
