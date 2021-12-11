from sys import argv, stdin

nums = [[int(c) for c in l.strip()] for l in stdin.readlines()]

h = len(nums)
w = len(nums[0])

flash = 0

if argv[1] == "1":
    top = 100
else:
    top = 1 * 10 ** 9

for i in range(top):
    for y in range(h):
        for x in range(w):
            nums[y][x] += 1

    seen = set()

    def recurse():
        added = 0
        for y in range(h):
            for x in range(w):
                if (y, x) not in seen and nums[y][x] > 9:
                    seen.add((y, x))
                    added += 1
                    for dy in range(-1, 2):
                        for dx in range(-1, 2):
                            if not (dy == 0 and dx == 0):
                                ny = y + dy
                                nx = x + dx

                                if 0 <= ny < h and 0 <= nx < w:
                                    nums[ny][nx] += 1
        if added > 0:
            recurse()

    recurse()

    temp_flash = 0
    for y in range(h):
        for x in range(w):
            if nums[y][x] > 9:
                flash += 1
                temp_flash += 1
                nums[y][x] = 0

    if temp_flash == 100:
        print(i + 1)
        exit(0)

print(flash)
