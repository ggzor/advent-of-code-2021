from sys import argv, stdin
from collections import defaultdict

text = stdin.read()
lines = text.splitlines()

p1 = int(lines[0].split(" ")[-1])
p2 = int(lines[1].split(" ")[-1])


def adjust(p, amount, target=10):
    p += amount % target
    if p > target:
        return p - target
    else:
        return p


if argv[1] == "1":
    die = 100
    roll_count = 0

    def roll():
        global die
        global roll_count

        roll_count += 1
        die += 1
        if die == 101:
            die = 1
        return die

    ps = [p1, p2]
    scores = [0, 0]
    turn = 0

    while max(scores) < 1000:
        p = adjust(ps[turn], roll() + roll() + roll())
        ps[turn] = p
        scores[turn] += p
        turn = 1 - turn

    print(min(scores) * roll_count)
else:
    universes = {((p1, p2), (0, 0), 0): 1}
    finished = {0: 0, 1: 0}

    while len(universes):
        new_universes = defaultdict(int)

        for (ps, scores, turn), count in universes.items():
            for r1 in [1, 2, 3]:
                for r2 in [1, 2, 3]:
                    for r3 in [1, 2, 3]:
                        p = adjust(ps[turn], r1 + r2 + r3, 10)
                        s = scores[turn] + p

                        nps = list(ps)
                        nps[turn] = p
                        nps = tuple(nps)

                        nscores = list(scores)
                        nscores[turn] = s
                        nscores = tuple(nscores)

                        if max(nscores) >= 21:
                            finished[turn] += count
                        else:
                            new_universes[(nps, nscores, 1 - turn)] += count

        universes = new_universes

    print(max(finished.values()))
