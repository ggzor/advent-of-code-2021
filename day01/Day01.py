from sys import argv, stdin
import operator as op

xs = [int(l) for l in stdin.readlines()]

solve1 = lambda xs: sum(map(op.lt, xs, xs[1:]))

# Part two pre-proc
if argv[1] == "2":
    xs = list(map(lambda *l: sum(l), xs, xs[1:], xs[2:]))

print(solve1(xs))
