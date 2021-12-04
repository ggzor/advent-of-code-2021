from sys import argv, stdin
from collections import defaultdict
import itertools as it
import operator as op

ms = [int(x) for x in input().split(",")]
boards = [
    [[(int(s), False) for s in l.split(" ") if (st := s.strip()) != ""] for l in v]
    for k, v in it.groupby(stdin.read().splitlines(), key=lambda l: l == "")
    if k == False
]

reverse_index = [
    defaultdict(
        lambda: None,
        {value[0]: (i, j) for i, row in enumerate(b) for j, value in enumerate(row)},
    )
    for b in boards
]


def is_winner(board):
    marked_row = any(all(marked for _, marked in row) for row in board)
    marked_column = any(all(marked for _, marked in col) for col in zip(*board))
    return marked_row or marked_column


def score(board):
    return sum(value for row in board for value, marked in row if not marked)


last = -1
winner_board = None
first_completed = False
winners = set()
winners_count = 0


condition = (
    # Part one, stop when one board is completed
    (lambda: first_completed)
    if argv[1] == "1"
    else
    # Part two, use winners_count to determine if we should stop
    (lambda: winners_count == len(boards))
)

for n in ms:
    if condition():
        break

    for board_idx, b, lookup in zip(it.count(), boards, reverse_index):
        if condition():
            break

        coord = lookup[n]
        if coord:
            i, j = coord
            b[i][j] = (n, True)

            if is_winner(b) and board_idx not in winners:
                last = n
                winner_board = b
                winners_count += 1
                first_completed = True
                winners.add(board_idx)

print(last * score(winner_board))
