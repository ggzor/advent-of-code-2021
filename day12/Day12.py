from sys import argv, stdin
import itertools as it
import operator as op
from collections import Counter, defaultdict

conns = [*map(lambda s: s.strip().split("-"), stdin.readlines())]

m = defaultdict(lambda: defaultdict(bool))

for a, b in conns:
    m[a][b] = True
    m[b][a] = True


paths = []


f = None

if argv[1] == "1":

    def dfs1(cur, seen, path):
        if cur == "end":
            paths.append(path.copy())

        path.append(cur)

        if cur.islower():
            seen.add(cur)

        for n, _ in m[cur].items():
            if not n in seen:
                dfs1(n, seen, path)

        if cur.islower():
            seen.remove(cur)
        path = path[:-1]

    f = dfs1
else:
    singles = defaultdict(int)

    def dfs(cur, seen, path, depth=1):
        global singles

        if cur == "end":
            paths.append(path.copy() + ["end"])
            return

        if cur.islower() and cur not in {"start", "end"}:
            if singles[cur] < 1:
                singles[cur] += 1
            elif singles[cur] < 2 and all(v < 2 for v in singles.values()):
                singles[cur] += 1
            else:
                return
        elif cur.islower():
            seen.add(cur)

        path.append(cur)

        for n, _ in m[cur].items():
            if not n in seen:
                dfs(n, seen, path, depth + 1)

        if singles[cur] >= 1:
            singles[cur] -= 1
        elif cur.islower() and cur not in {"start", "end"}:
            seen.remove(cur)

        path.pop()

    f = dfs


f("start", set(), [])

print(len(paths))
