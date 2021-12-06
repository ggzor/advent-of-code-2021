from sys import argv, stdin
from collections import Counter, defaultdict

nums = defaultdict(lambda: 0, Counter(map(int, stdin.read().split(","))))

n = 80
if argv[1] == "2":
    n = 256

for x in range(n):
    new = defaultdict(lambda: 0)
    for k, v in list(nums.items()):
        if k == 0:
            new[6] += v
            new[8] = v
        else:
            new[k - 1] += v

    nums = new

print(sum(nums.values()))
