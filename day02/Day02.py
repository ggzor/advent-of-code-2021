from sys import argv, stdin
import itertools as it
import operator as op

inst = [(d, int(x)) for d, x in map(lambda l: l.split(" "), stdin.readlines())]

if argv[1] == "1":
    x = y = 0
    for dir, n in inst:
        if dir == "forward":
            x += n
        elif dir == "down":
            y += n
        elif dir == "up":
            y -= n
    print(x * y)
else:
    x = y = aim = 0
    for dir, n in inst:
        if dir == "down":
            aim += n
        elif dir == "up":
            aim -= n
        elif dir == "forward":
            x += n
            y += aim * n
    print(x * y)
