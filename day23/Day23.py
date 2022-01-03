from sys import stdin
from queue import PriorityQueue
from dataclasses import dataclass, field
import itertools as it

lines = stdin.read().splitlines()

W = len(lines[0])
H = len(lines)

tys = {"A": 1, "B": 10, "C": 100, "D": 1000}
target = {"A": 3, "B": 5, "C": 7, "D": 9}
target_cols = set(target.values())


### Debug
def is_border(y, x):
    return (
        y == 0
        or (y == 1 and (x == 0 or x == W - 1))
        or (y >= 2 and x not in target_cols)
        or y == H - 1
    )


def is_space(y, x):
    return x in [0, 1, W - 2, W - 1] and y >= 3


def pprint_board(b):
    return "\n".join(
        "".join(
            " " if is_space(y, x) else ("#" if is_border(y, x) else b.get((y, x), "."))
            for x in range(W)
        )
        for y in range(H)
    )


### End of Debug

initial_board = {
    (y, x): v for y, r in enumerate(lines) for x, v in enumerate(r) if v in tys
}


def apply_movement(movement, board):
    src, to = movement
    new_board = board.copy()

    del new_board[src]
    new_board[to] = board[src]

    return new_board


def next_movements(board):
    for init, v in board.items():
        y, x = init
        if y >= 2:
            if target[v] != x or any(board[y2, x] != v for y2 in range(y + 1, H - 1)):
                if all(board.get((y2, x), ".") == "." for y2 in range(y - 1, 1, -1)):
                    base_len = y - 1

                    for x2 in range(x - 1, 0, -1):
                        cur = (1, x2)
                        cur_len = base_len + abs(x - x2)
                        if cur not in board:
                            if x2 not in target_cols:
                                yield cur_len, (init, cur)
                            else:
                                if x2 == target[v]:
                                    target_cur = None
                                    target_len = None
                                    for y2 in range(2, H - 1):
                                        new_cur = (y2, x2)
                                        new_len = cur_len + y2 - 1
                                        if new_cur not in board:
                                            target_cur = new_cur
                                            target_len = new_len
                                        else:
                                            break
                                    if target_cur is not None:
                                        if all(
                                            board.get((y2, x2), v) == v
                                            for y2 in range(target_cur[0] + 1, H - 1)
                                        ):
                                            yield target_len, (init, target_cur)
                        else:
                            break

                    for x2 in range(x + 1, W - 1):
                        cur = (1, x2)
                        cur_len = base_len + abs(x - x2)
                        if cur not in board:
                            if x2 not in target_cols:
                                yield cur_len, (init, cur)
                            else:
                                if x2 == target[v]:
                                    target_cur = None
                                    target_len = None
                                    for y2 in range(2, H - 1):
                                        new_cur = (y2, x2)
                                        new_len = cur_len + y2 - 1
                                        if new_cur not in board:
                                            target_cur = new_cur
                                            target_len = new_len
                                        else:
                                            break
                                    if target_cur is not None:
                                        if all(
                                            board.get((y2, x2), v) == v
                                            for y2 in range(target_cur[0] + 1, H - 1)
                                        ):
                                            yield target_len, (init, target_cur)
                        else:
                            break
        elif y == 1:
            for x2 in range(x - 1, 0, -1):
                cur = (1, x2)
                cur_len = abs(x - x2)
                if cur not in board:
                    if x2 == target[v]:
                        target_cur = None
                        target_len = None
                        for y2 in range(2, H - 1):
                            new_cur = (y2, x2)
                            new_len = cur_len + y2 - 1
                            if new_cur not in board:
                                target_cur = new_cur
                                target_len = new_len
                            else:
                                break
                        if target_cur is not None:
                            if all(
                                board.get((y2, x2), v) == v
                                for y2 in range(target_cur[0] + 1, H - 1)
                            ):
                                yield target_len, (init, target_cur)
                else:
                    break

            for x2 in range(x + 1, W - 1):
                cur = (1, x2)
                cur_len = abs(x - x2)
                if cur not in board:
                    if x2 == target[v]:
                        target_cur = None
                        target_len = None
                        for y2 in range(2, H - 1):
                            new_cur = (y2, x2)
                            new_len = cur_len + y2 - 1
                            if new_cur not in board:
                                target_cur = new_cur
                                target_len = new_len
                            else:
                                break
                        if target_cur is not None:
                            if all(
                                board.get((y2, x2), v) == v
                                for y2 in range(target_cur[0] + 1, H - 1)
                            ):
                                yield target_len, (init, target_cur)
                else:
                    break


def is_goal(board):
    return all(y >= 2 and target[v] == x for (y, x), v in board.items())


qitem_counter = it.count()


@dataclass(order=True)
class QItem:
    dist: int
    board: dict = field(compare=False)
    count: int = 0

    def __post_init__(self):
        self.count = next(qitem_counter)

    def __eq__(self, other):
        return self.__hash__() == other.__hash__()

    def __hash__(self):
        return hash(frozenset(self.board.items()))


q = PriorityQueue()
q.put(QItem(0, initial_board))

seen = set()
goal = None

i = 0

while not q.empty():
    cur = q.get()

    if not cur in seen:
        seen.add(cur)

        if is_goal(cur.board):
            goal = cur
            break

        for m_len, m in next_movements(cur.board):
            new = QItem(
                m_len * tys[cur.board[m[0]]] + cur.dist,
                apply_movement(m, cur.board),
            )

            if new not in seen:
                q.put(new)

        i += 1

if goal is not None:
    print(goal.dist)
