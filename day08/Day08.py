from functools import reduce
from sys import argv, stdin
import operator as op
from collections import defaultdict

parts = [[[*map(frozenset, p.strip().split(" "))] for p in l.split("|")] for l in stdin]

digit_lens = {2: 1, 3: 7, 4: 4, 7: 8}

if argv[1] == "1":
    total = sum(sum(len(s) in digit_lens.keys() for s in p2) for _, p2 in parts)
    print(total)
else:
    total = 0

    for p1, p2 in parts:
        known = {}
        unknown_by_len = defaultdict(list)

        for s in p1:
            if len(s) in digit_lens.keys():
                known[digit_lens[len(s)]] = s
            else:
                unknown_by_len[len(s)].append(s)

        digits_of_len = lambda l: reduce(op.and_, filter(lambda s: len(s) == l, p1))

        segs_adg = digits_of_len(5)
        segs_abfg = digits_of_len(6)

        seg_d = segs_adg - segs_abfg

        known[0] = known[8] - seg_d
        known[3] = segs_adg | known[1]
        known[5] = segs_abfg | seg_d

        known[2] = segs_adg | (known[8] - known[5])
        known[9] = known[5] | known[1]
        known[6] = known[5] | (known[8] - known[9])

        digit_mappings = {v: k for k, v in known.items()}

        num = "".join(map(str, (digit_mappings[d] for d in p2)))
        total += int(num)

    print(total)
