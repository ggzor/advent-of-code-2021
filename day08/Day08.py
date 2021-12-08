from functools import reduce
from sys import argv, stdin
from collections import defaultdict

digs = [
    *map(set, "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg".split(" "))
]

full = set("abcdefg")

total = 0
for l in stdin.readlines():
    mapping = {}

    p1, p2 = l.split("|")
    p1 = p1.strip().split(" ")
    p2 = p2.strip().split(" ")

    lmap = {2: 1, 4: 4, 3: 7, 7: 8}

    if argv[1] == "1":
        total += sum(len(s) in lmap.keys() for s in p2)
    else:
        # 7 - 8
        # 6 - 0 6 9
        # 5 - 2 3 5
        # 4 - 4
        # 3 - 7
        # 2 - 1

        known = {}
        unknown = defaultdict(list)

        for s in map(lambda s: "".join(sorted(s)), p1):
            found = False
            for k, v in lmap.items():
                if len(s) == k:
                    mapping[s] = str(v)
                    known[v] = set(s)
                    found = True
                    break

            if not found:
                unknown[len(s)].append(set(s))

        final = {}

        ka = list(known[7] - known[1])[0]
        final[ka] = "a"

        all_6 = reduce(set.intersection, unknown[6])
        mids = all_5 = reduce(set.intersection, unknown[5])

        kg = next(iter(all_6 & all_5 - set([ka])))
        final[kg] = "g"

        kd = next(iter(mids - set([ka, kg])))
        final[kd] = "d"

        kb = next(iter(all_6 - mids - known[1]))
        final[kb] = "b"

        kf = next(iter(all_6 - mids - set([kb])))
        final[kf] = "f"

        left = set("abcdefg") - final.keys()

        five_is = frozenset(full - left)
        two_and_three = frozenset(map(frozenset, unknown[5])).difference([five_is])

        s1, s2 = list(two_and_three)
        ke_and_kf = s1.symmetric_difference(s2)
        ke = next(iter(ke_and_kf - set([kf])))
        final[ke] = "e"

        kc = next(iter(full - final.keys()))
        final[kc] = "c"

        for s in map(lambda s: "".join(sorted(s)), p1):
            real = digs.index(set(final[c] for c in s))
            mapping[s] = str(real)

        s = "".join(mapping.get("".join(sorted(v)), "0") for v in p2)
        total += int(s)

print(total)
