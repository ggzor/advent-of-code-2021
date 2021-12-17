from sys import argv, stdin
import itertools as it

text = stdin.read().strip()

x1, x2, y1, y2 = area = [
    int("".join(v))
    for k, v in it.groupby(text, key=lambda s: s.isnumeric() or s == "-")
    if k
]


def in_area(x, y):
    x1, x2, y1, y2 = area

    return x1 <= x <= x2 and y1 <= y <= y2


def simulate(xv, yv):
    maxx = x = 0
    maxy = y = 0
    hit = in_area(x, y)

    while True:
        x += xv
        y += yv
        xv = xv + (-1 if xv > 0 else (1 if xv < 0 else 0))
        yv -= 1

        hit |= in_area(x, y)

        maxx = max(x, maxx)
        maxy = max(y, maxy)

        if y < y1 or x > x2 or (xv == 0 and x < x1):
            break

    return (hit, maxy)


count = 0
best = -1e9

# Pure guess
max_yv = 300

for xv in range(0, x2 + 1):
    for yv in range(y1 - 1, max_yv):
        hit, maxy = simulate(xv, yv)
        if hit:
            count += 1
            best = max(maxy, best)

if argv[1] == "1":
    print(best)
else:
    print(count)
