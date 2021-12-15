from sys import argv, stdin
from queue import PriorityQueue

old_nums = [[int(x) for x in l.strip()] for l in stdin.readlines()]

if argv[1] == "1":
    nums = old_nums
else:

    def f(x):
        if x <= 9:
            return x
        else:
            return x - 9

    nums = []
    for r in old_nums:
        l = []
        l.extend(r)
        for i in range(1, 5):
            l.extend(f(v + i) for v in r)

        nums.append(l)

    temp_nums = []
    temp_nums.extend(nums)
    for i in range(1, 5):
        temp_nums.extend([[f(x + i) for x in r] for r in nums])

    nums = temp_nums

# print("\n".join("".join(map(str, r)) for r in nums))

w = len(nums[0])
h = len(nums)

D = {(y, x): float("inf") for y in range(h) for x in range(w)}
D[(0, 0)] = 0


pq = PriorityQueue()
pq.put((0, (0, 0)))

seen = set()

while not pq.empty():
    (dist, p) = pq.get()
    (y, x) = p
    seen.add(p)

    for dy, dx in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
        ny = y + dy
        nx = x + dx
        neigh = ny, nx

        if 0 <= ny < h and 0 <= nx < w:
            distance = nums[ny][nx]
            if not neigh in seen:
                old_cost = D[neigh]
                new_cost = D[p] + distance
                if new_cost < old_cost:
                    pq.put((new_cost, neigh))
                    D[neigh] = new_cost

print(D[(h - 1, w - 1)])
