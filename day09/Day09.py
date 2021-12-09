from sys import argv, stdin

nums = [[int(c) for c in l.strip()] for l in stdin.readlines()]

h = len(nums)
w = len(nums[0])

s = 0
basins = []

for i in range(len(nums)):
    for j in range(len(nums[0])):
        value = nums[i][j]

        is_low = True
        for dx in range(-1, 2):
            for dy in range(-1, 2):
                if not (dx == 0 and dy == 0):
                    x = j + dx
                    y = i + dy

                    if 0 <= x < w and 0 <= y < h:
                        if nums[y][x] <= value:
                            is_low = False
                            break

        if is_low:
            s += value + 1

            marks = [l.copy() for l in nums]

            def propagate(x, y):
                if marks[y][x] is None:
                    return

                marks[y][x] = None

                for dy in range(-1, 2):
                    for dx in range(-1, 2):
                        if not (dx == 0 and dy == 0) and (dx == 0 or dy == 0):
                            nx = x + dx
                            ny = y + dy

                            if 0 <= nx < w and 0 <= ny < h:
                                if nums[ny][nx] != 9 and nums[ny][nx] > nums[y][x]:
                                    propagate(nx, ny)

            propagate(j, i)

            basins.append(sum(sum(v is None for v in r) for r in marks))

basins.sort()

if argv[1] == "1":
    print(s)
else:
    print(basins[-1] * basins[-2] * basins[-3])
