from sys import argv, stdin
from collections import Counter

ns = [[int(c) for c in s.strip()] for s in stdin.readlines()]
cols = len(ns[0])

bit_counts = lambda nums: [Counter(col) for col in zip(*nums)]
most_common = lambda bitc: [1 if col[1] >= col[0] else 0 for col in bitc]
least_common = lambda mostc: [1 - x for x in mostc]
as_dec = lambda l: int("".join(map(str, l)), 2)

if argv[1] == "1":
    t = most_common(bit_counts(ns))
    gamma = as_dec(t)
    epsilon = as_dec(least_common(t))

    print(gamma * epsilon)
else:
    # Part two
    def go(init, post):
        for i in range(cols):
            if len(init) > 1:
                t = post(most_common(bit_counts(init)))
                init = list(filter(lambda x: x[i] == t[i], init))

        return init[0]

    oxygen = as_dec(go(ns, lambda x: x))
    co2 = as_dec(go(ns, least_common))

    print(oxygen * co2)
