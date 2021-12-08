from functools import reduce
from sys import argv, stdin
from collections import defaultdict

parts = [[[*map(frozenset, p.strip().split(" "))] for p in l.split("|")] for l in stdin]

digit_lens = {2: 1, 3: 7, 4: 4, 7: 8}

if argv[1] == "1":
    total = sum(sum(len(s) in digit_lens.keys() for s in p2) for _, p2 in parts)
    print(total)
else:
    total = 0

    for p1, p2 in parts:
        known_digits = {}
        unknown_digits_by_len = defaultdict(list)

        for s in p1:
            if len(s) in digit_lens.keys():
                known_digits[digit_lens[len(s)]] = s
            else:
                unknown_digits_by_len[len(s)].append(s)

        first = lambda s: next(iter(s))

        all_5 = mids = reduce(frozenset.intersection, unknown_digits_by_len[5])
        all_6 = abfg = reduce(frozenset.intersection, unknown_digits_by_len[6])

        known_segments = {}

        known_segments["a"] = known_digits[7] - known_digits[4]
        known_segments["d"] = mids - abfg
        known_segments["g"] = mids - (known_segments["a"] | known_segments["d"])

        known_digits[5] = abfg | known_segments["d"]
        known_digits[0] = known_digits[8] - known_segments["d"]
        known_digits[3] = mids | known_digits[1]

        known_digits[9] = known_digits[5] | known_digits[7]
        known_segments["e"] = known_digits[8] - known_digits[9]

        known_digits[6] = known_digits[5] | known_segments["e"]

        known_digits[2] = first(set(p1) - set(known_digits.values()))

        digit_mappings = {v: k for k, v in known_digits.items()}

        num = "".join(map(str, (digit_mappings.get(d, 0) for d in p2)))
        total += int(num)

    print(total)
