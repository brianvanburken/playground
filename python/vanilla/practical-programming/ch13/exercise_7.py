import time
import random
from exercise_5 import bubble_sort


def built_in(L: list) -> None:
    L.sort()


def print_times(L: list) -> None:
    print(len(L), end='\t')
    for func in (bubble_sort, built_in):
        L_copy = L[:]
        t1 = time.perf_counter()
        func(L_copy)
        t2 = time.perf_counter()
        print("{0:7.1f}".format((t2 - t1) * 1000.), end='\t')
    print()


for list_size in [10, 1000, 2000, 3000, 4000, 5000, 10000]:
    L = list(range(list_size))
    random.shuffle(L)
    print_times(L)
