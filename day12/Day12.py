from sys import argv, stdin
from collections import defaultdict

g = {x for l in stdin if (v := tuple(l.strip().split("-"))) for x in [v, v[::-1]]}


def dfs_one(cur, seen):
    if cur in seen:
        return 0
    if cur == "end":
        return 1

    if cur.islower():
        seen.add(cur)
    r = sum(dfs_one(n, seen) for s, n in g if s == cur)
    if cur.islower():
        seen.remove(cur)
    return r


def dfs_two(cur, start_seen, counters):
    if (
        (cur == "start" and start_seen)
        or sum(c == 2 for c in counters.values()) > 1
        or counters[cur] == 2
    ):
        return 0
    if cur == "end":
        return 1

    if cur.islower():
        counters[cur] += 1
    r = sum(dfs_two(n, True, counters) for s, n in g if s == cur)
    if cur.islower():
        counters[cur] -= 1
    return r


if argv[1] == "1":
    print(dfs_one("start", set()))
else:
    print(dfs_two("start", False, defaultdict(int)))
