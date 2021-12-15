from sys import argv, stdin
from queue import PriorityQueue

text = stdin.read()
lines = text.splitlines()

xs = {(y, x): int(v) for y, l in enumerate(lines) for x, v in enumerate(l)}
w = max(y for y, _ in xs.keys()) + 1
h = max(x for _, x in xs.keys()) + 1

if argv[1] == "2":
    f = lambda x: x if x <= 9 else x - 9
    xs = {
        (y + sy * h, x + sx * w): f(v + sy + sx)
        for (y, x), v in xs.items()
        for sy in range(5)
        for sx in range(5)
    }
    h *= 5
    w *= 5

D = {}
q = PriorityQueue()
seen = set()

D[(0, 0)] = 0
q.put((0, (0, 0)))

while not q.empty():
    d, p = q.get()
    y, x = p
    for dy, dx in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
        np = ny, nx = y + dy, x + dx

        if not np in seen and 0 <= ny < h and 0 <= nx < w:
            old = D.get(np, float("infinity"))
            new = d + xs[np]
            if new < old:
                D[np] = new
                q.put((new, np))
    seen.add(p)

print(D[(h - 1, w - 1)])
