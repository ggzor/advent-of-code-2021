from sys import argv, stdin

ls = stdin.read().splitlines(False)

scores = {")": 3, "]": 57, "}": 1197, ">": 25137}

matching = ["()", "[]", "{}", "<>"]
ps = {}
init = set()
end = set()
for c1, c2 in matching:
    init.add(c1)
    end.add(c2)
    ps[c1] = c2
    ps[c2] = c1

incomplete = []

total = 0
for l in ls:
    stack = [None]
    ending = None
    for c in l:
        if c in init:
            stack.append(c)
        elif c in end and stack[-1] == ps[c]:
            stack.pop()
        else:
            ending = c
            break

    if ending:
        total += scores[ending]
    else:
        incomplete.append([l, stack])


if argv[1] == "1":
    print(total)
else:
    ps2 = ")]}>"

    scores = []
    for _, rem in incomplete:
        result = 0
        for c in rem[1:][::-1]:
            result *= 5
            result += ps2.index(ps[c]) + 1
        scores.append(result)

    scores.sort()
    print(scores[len(scores) // 2])
