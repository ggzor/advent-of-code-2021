from sys import argv, stdin

nums = [*map(int, stdin.read().split(","))]

if argv[1] == "2":
    f = lambda x: (x * (x + 1)) / 2
else:
    f = lambda x: x

diffs = [int(sum(f(abs(n - i)) for n in nums)) for i in range(max(nums) + 1)]
print(min(diffs))
