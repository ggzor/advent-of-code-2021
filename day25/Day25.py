from sys import stdin
import itertools as it

text = stdin.read()
lines = text.splitlines()

h = len(lines)
w = len(lines[0])

cucumbers = {
    (y, x): v for y, r in enumerate(lines) for x, v in enumerate(r) if v in ">v"
}

for i in it.count(1):
    changed = 0

    new_cucumbers = {}
    for (y, x), v in cucumbers.items():
        if v == ">" and cucumbers.get((y, (x + 1) % w), ".") == ".":
            new_cucumbers[y, (x + 1) % w] = v
            changed += 1
        else:
            new_cucumbers[y, x] = v

    cucumbers = new_cucumbers

    new_cucumbers = {}
    for (y, x), v in cucumbers.items():
        if v == "v" and cucumbers.get(((y + 1) % h, x), ".") == ".":
            new_cucumbers[(y + 1) % h, x] = v
            changed += 1
        else:
            new_cucumbers[y, x] = v

    cucumbers = new_cucumbers

    if changed == 0:
        print(i)
        break
