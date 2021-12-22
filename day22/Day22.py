from dataclasses import dataclass
from sys import argv, stdin
import re
from typing import List


@dataclass
class Segment:
    start: int
    end: int
    is_empty: bool = False

    def __post_init__(self):
        if self.start > self.end:
            self.is_empty = True

    def __and__(self, other: "Segment") -> "Segment":
        if self.is_empty or other.is_empty:
            return Segment(0, -1)

        a, b = self, other
        if a.end > b.end:
            a, b = b, a

        if a.end < b.start:
            return Segment(0, -1)
        elif a.start >= b.start:
            return a
        else:
            return Segment(max(a.start, b.start), min(a.end, b.end))

    @property
    def size(self):
        if self.is_empty:
            return 0
        else:
            return self.end - self.start + 1


assert (Segment(0, -1) & Segment(5, 10)).is_empty
assert (Segment(0, 5) & Segment(6, 10)).is_empty
assert (Segment(6, 10) & Segment(0, 5)).is_empty
assert (Segment(0, 5) & Segment(-2, 6)) == Segment(0, 5)
assert (Segment(0, 5) & Segment(0, 6)) == Segment(0, 5)
assert (Segment(0, 5) & Segment(0, 6)) == Segment(0, 5)
assert (Segment(0, 4) & Segment(2, 6)) == Segment(2, 4)


@dataclass
class Box:
    xs: Segment
    ys: Segment
    zs: Segment

    @property
    def is_empty(self):
        return self.xs.is_empty or self.ys.is_empty or self.zs.is_empty

    def __and__(self, other: "Box") -> "Box":
        return Box(self.xs & other.xs, self.ys & other.ys, self.zs & other.zs)

    def __sub__(self, other: "Box") -> List["Box"]:
        if (self & other).is_empty:
            return [self]

        left = Box(
            Segment(self.xs.start, other.xs.start - 1),
            self.ys & other.ys,
            self.zs & other.zs,
        )

        right = Box(
            Segment(other.xs.end + 1, self.xs.end),
            self.ys & other.ys,
            self.zs & other.zs,
        )

        front = Box(
            self.xs,
            Segment(self.ys.start, other.ys.start - 1),
            self.zs & other.zs,
        )

        back = Box(
            self.xs,
            Segment(other.ys.end + 1, self.ys.end),
            self.zs & other.zs,
        )

        down = Box(
            self.xs,
            self.ys,
            Segment(self.zs.start, other.zs.start - 1),
        )

        up = Box(
            self.xs,
            self.ys,
            Segment(other.zs.end + 1, self.zs.end),
        )

        return [*filter(lambda x: not x.is_empty, [left, right, front, back, down, up])]

    @property
    def size(self):
        return self.xs.size * self.ys.size * self.zs.size


boxes = []
for l in map(str.strip, stdin):
    ns = [*map(int, re.findall(r"-?\d+", l))]
    ac = l.split(" ")[0]

    boxes.append((ac, Box(Segment(*ns[:2]), Segment(*ns[2:4]), Segment(*ns[4:6]))))

if argv[1] == "1":
    boxes = [
        *map(
            lambda t: (
                t[0],
                t[1] & Box(Segment(-50, 50), Segment(-50, 50), Segment(-50, 50)),
            ),
            boxes,
        )
    ]

first, *remaining = boxes

s = [first[1]]

for ac, box in remaining:
    if ac == "on":
        to_add = [box]
        for b in s:
            to_add = [x for b2 in to_add for x in b2 - b]
        s.extend(to_add)
    elif ac == "off":
        s = [x for b in s for x in b - box]

print(sum(b.size for b in s))
