from sys import argv, stdin
from collections import Counter

template = stdin.readline().strip()
stdin.readline()

rules = []
for l in stdin.readlines():
    a, b = l.strip().split(" -> ")
    rules.append((a, b))


char_count = Counter(template)
pairs = Counter((a + b for a, b in zip(template, template[1:])))

if argv[1] == "1":
    top = 10
else:
    top = 40

for _ in range(top):
    new_pairs = Counter()
    for k, v in pairs.items():
        for r, s in rules:
            if k == r:
                char_count[s] += v
                new_pairs[r[0] + s] += v
                new_pairs[s + r[1]] += v
                break
    pairs = new_pairs


print(max(char_count.values()) - min(char_count.values()))
