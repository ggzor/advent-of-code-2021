from sys import argv, stdin

nums = set()
folds = []

for l in map(str.strip, stdin.readlines()):
    if l.startswith("fold"):
        a, b = l[len("fold along") + 1 :].split("=")
        folds.append([a, int(b)])
    elif len(l.strip()) == 0:
        pass
    else:
        a, b = l.split(",")
        nums.add((int(a), int(b)))

w = max(n for dir, n in folds if dir == "x") * 2 + 1
h = max(n for dir, n in folds if dir == "y") * 2 + 1

if argv[1] == "1":
    top = 1
else:
    top = len(folds)

for dir, n in folds[:top]:
    new_nums = set()
    for x, y in nums:
        if dir == "y":
            if y >= n:
                new_nums.add((x, h - y - 1))
            else:
                new_nums.add((x, y))
        else:
            if x >= n:
                new_nums.add((w - x - 1, y))
            else:
                new_nums.add((x, y))
    if dir == "y":
        h = h // 2
    else:
        w = w // 2
    nums = new_nums

if argv[1] == "1":
    print(len(nums))
else:
    mat = [["#" if (x, y) in nums else " " for x in range(w)] for y in range(h)]
    print("\n".join("".join(r) for r in mat))
