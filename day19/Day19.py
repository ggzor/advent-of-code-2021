from sys import argv, stdin
import numpy as np

text = stdin.read()

scanners = {}
for i, ls in enumerate(text.split("\n\n")):
    a = np.array([[*map(int, l.split(","))] for l in ls.strip().splitlines()[1:]])
    scanners[i] = a

perms = [
    [1, 2, 3],
    [-3, 2, 1],
    [-1, 2, -3],
    [3, 2, -1],
    [-1, -2, 3],
    [-3, -2, -1],
    [1, -2, -3],
    [3, -2, 1],
    [-2, 1, 3],
    [-2, -3, 1],
    [-2, -1, -3],
    [-2, 3, -1],
    [2, -1, 3],
    [2, -3, -1],
    [2, 1, -3],
    [2, 3, 1],
    [1, -3, 2],
    [-3, -1, 2],
    [-1, 3, 2],
    [3, 1, 2],
    [1, 3, -2],
    [-3, 1, -2],
    [-1, -3, -2],
    [3, -1, -2],
]
perms = [tuple(l) for l in perms]


def overlap(a, b):
    aset = set(map(tuple, a))
    for p in perms:
        p = np.array(p)
        bs = b[:, abs(p) - 1] * np.sign(p)

        for x in a:
            for y in bs:
                d = y - x
                tb = bs - d

                tbset = set(map(tuple, tb))

                if len(set(aset & tbset)) >= 12:
                    return (True, tb, d)
    return (False, None, None)


found = {}
ds = {0: np.array([0, 0, 0])}

a, *others = [*scanners]
for b in others:
    av = scanners[a]
    bv = scanners[b]

    ok, nbv, db = overlap(av, bv)
    if ok:
        found[a] = av
        found[b] = nbv
        ds[b] = db
        break

tried = set()

while len(found) < len(scanners):
    stop = False
    for a in [*found]:
        if stop:
            break

        for b in scanners.keys() - set(found):
            if (a, b) in tried:
                continue

            if stop:
                break
            av = found[a]
            bv = scanners[b]

            ok, nbv, db = overlap(av, bv)
            tried.add((a, b))
            if ok:
                found[b] = nbv
                ds[b] = db
                stop = True

if argv[1] == "1":
    print(len(set(tuple(x) for a in found.values() for x in a)))
else:
    md = -1e9
    ks = list(ds.keys())
    for i in range(len(ks)):
        for j in range(i + 1, len(ks)):
            s = sum(ds[ks[i]] - ds[ks[j]])
            md = max(md, s)

    print(md)
