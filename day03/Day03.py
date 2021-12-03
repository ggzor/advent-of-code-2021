from sys import argv, stdin
from collections import defaultdict
import itertools as it
import operator as op

ns = [[int(c) for c in s.strip()] for s in stdin.readlines()]
cols = len(ns[0])


def most_common_count(nums):
    common = [(0, 0)] * cols

    for i, col in enumerate(map(lambda c: it.groupby(sorted(map(int, c))), zip(*nums))):
        for k, g in col:
            gcount = len(list(g))

            _, ccount = common[i]

            if gcount > ccount:
                common[i] = k, gcount
            elif gcount == ccount:
                common[i] = k, gcount

    return [x[0] for x in common]


as_dec = lambda l: int("".join(map(str, l)), 2)

if argv[1] == "1":
    gamma = most_common_count(ns)
    epsilon = [1 - x for x in gamma]

    print(as_dec(gamma) * as_dec(epsilon))
else:
    oxygen = 0
    scrubber = 0

    rest_most = ns
    rest_least = ns

    for i in range(cols):
        if len(rest_most) > 1:
            most_most_common = most_common_count(rest_most)
            rest_most = list(filter(lambda x: x[i] == most_most_common[i], rest_most))

        if len(rest_least) > 1:
            least_most_common = [1 - x for x in most_common_count(rest_least)]
            rest_least = list(
                filter(lambda x: x[i] == least_most_common[i], rest_least)
            )

    oxygen = as_dec(rest_most[0])
    scrubber = as_dec(rest_least[0])

    print(oxygen * scrubber)
