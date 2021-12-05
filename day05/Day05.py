from sys import argv, stdin
import re

NUM_RE = re.compile(r"(\d+),(\d+) -> (\d+),(\d+)")

nums = [
    tuple(int(g) for g in m.groups())
    for l in stdin.readlines()
    if (m := NUM_RE.match(l))
]


BOARD_SIZE = 1000

board = []
for i in range(BOARD_SIZE):
    board.append([0] * BOARD_SIZE)


def hv_lines(l):
    x1, y1, x2, y2 = l
    return x1 == x2 or y1 == y2


def sign(n):
    if n > 0:
        return 1
    elif n < 0:
        return -1
    else:
        return 0


def fill_board(line_criteria):
    for x1, y1, x2, y2 in filter(line_criteria, nums):
        if x1 == x2 and y1 == y2:
            board[y1][x1] += 1
        else:
            stepx = sign(x2 - x1)
            stepy = sign(y2 - y1)

            x = x1
            y = y1

            while not (x == x2 and y == y2):
                board[y][x] += 1

                x += stepx
                y += stepy

            board[y][x] += 1


if argv[1] == "1":
    fill_board(hv_lines)
else:
    # Part two, use all lines
    fill_board(lambda _: True)

print(sum(x > 1 for r in board for x in r))
