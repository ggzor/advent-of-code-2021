from sys import argv, stdin
import itertools as it

text = stdin.read().strip()
lines = text.splitlines()


def parse(n):
    linear = []

    def go(n, path):
        if isinstance(n, int):
            linear.append([n, path])
        else:
            a, b = n
            go(a, path + "L")
            go(b, path + "R")

    go(n, "")
    return linear


snails = [*map(eval, lines)]
snails = [*map(parse, snails)]


def add(a, b):
    return [*[[v, "L" + p] for v, p in a], *[[v, "R" + p] for v, p in b]]


def reduce(n: list):
    explode = None
    for i in range(len(n)):
        av, ap = n[i]
        if len(ap) == 5 and ap[4] == "L" and i + 1 < len(n):
            bv, bp = n[i + 1]
            if bp[:4] == ap[:4]:
                explode = i
                break

    if explode != None:
        left = None
        right = None
        if explode > 0:
            left = n[explode - 1]
        if explode + 2 < len(n):
            right = n[explode + 2]

        av, ad = n.pop(explode)
        bv, _ = n.pop(explode)

        if left != None:
            left[0] += av
        if right != None:
            right[0] += bv

        n.insert(explode, [0, ad[:4]])

        return True

    split = None
    for i in range(len(n)):
        av, ap = n[i]
        if av >= 10:
            split = i
            break

    if split != None:
        av, ap = n.pop(split)
        n[split:split] = [[av // 2, ap + "L"], [av - av // 2, ap + "R"]]
        return True

    return False


def magnitude(n):
    original = {p: v for v, p in n}

    def go(p):
        if p in original:
            return original[p]
        else:
            return 3 * go(p + "L") + 2 * go(p + "R")

    return go("")


if argv[1] == "1":
    cur, *rest = snails

    for r in rest:
        cur = add(cur, r)
        while reduce(cur):
            pass
else:
    cur = None
    mn = -1e9
    for a, b in it.permutations(snails, 2):
        n = add(a, b)
        while reduce(n):
            pass

        nmn = magnitude(n)
        if nmn > mn:
            cur = n
            mn = nmn


print(magnitude(cur))
