from sys import argv, stdin
from dataclasses import dataclass
import re
from typing import List, Tuple

"""
This is a special purpose solution. It may or may not work with your input.

This solution parses the input looking for two types of patterns:

- Forced actions: These actions will be run, regardless the condition.
    ```
    if i0 - 14 != z.top():
        z.push(i0 + 14)
    ```

    Corresponds to this ALU code:
    ```
    inp w
    mul x 0
    add x z
    mod x 26
    div z 1
    add x 14
    eql x w
    eql x 0
    mul y 0
    add y 25
    mul y x
    add y 1
    mul z y
    mul y 0
    add y w
    add y 14
    mul y x
    add z y
    ```

- Conditional actions: These actions depend on the stack's top value.
    ```
    if i5 + 12 == z.top():
        z.pop()
    else:
        z.pop()
        z.push(i5 + 5)
    ```

    Corresponds to this ALU code:
    ```
    inp w
    mul x 0
    add x z
    mod x 26
    div z 26
    add x -12
    eql x w
    eql x 0
    mul y 0
    add y 25
    mul y x
    add y 1
    mul z y
    mul y 0
    add y w
    add y 5
    mul y x
    add z y
    ```

The divisor on the fifth line of each ALU code fragment determines which type of
condition it is.

Then, each possible execution path is considered taking into account the stack
(z's value) and the given condition. It branches when it is not a forced condition.

There should be only one execution path that has its stack empty at the end, from
which the maximum possible value is calculated using the conditions that were chosen
for this specific execution path.

"""


def split(lines, items):
    return [
        [
            *[int(m[0]) if (m := re.match(r"-?\d+", w)) else w for w in l.split(" ")],
            *([None] * items),
        ][:items]
        for l in lines
    ]


lines = split(map(str.strip, stdin.readlines()), 3)


@dataclass
class InputSum:
    name: str
    right: int

    def __repr__(self):
        symbol = "+" if self.right >= 0 else "-"
        return f"{self.name} {symbol} {abs(self.right)}"


@dataclass
class StackAction:
    cond: InputSum
    to_push: InputSum
    is_forced: bool

    @property
    def code(self):
        if self.is_forced:
            return f"""\
if {self.cond} != z.top():
    z.push({self.to_push})
"""
        else:
            return f"""\
if {self.cond} == z.top():
    z.pop()
else:
    z.pop()
    z.push({self.to_push})
"""


@dataclass
class CheckEq:
    left: InputSum
    right: InputSum
    value: bool

    def __repr__(self):
        symbol = "==" if self.value else "!="
        return f"{self.left} {symbol} {self.right}"


actions = []

for i in range(14):
    cur = lines[i * 18 : (i + 1) * 18]

    cond = InputSum(f"i{i}", -cur[5][-1])
    push = InputSum(f"i{i}", cur[-3][-1])

    actions.append(StackAction(cond, push, cur[4][-1] == 1))

stacks: List[Tuple[List[CheckEq], List[InputSum]]] = [([], [])]

for c in actions:
    new_stacks = []
    for pcond, ps in stacks:

        if c.is_forced:
            cond = pcond.copy()
            s = ps.copy()

            s.append(c.to_push)

            new_stacks.append((cond, s))
        else:
            cond1 = pcond.copy()
            cond2 = pcond.copy()

            s1 = ps.copy()
            s2 = ps.copy()

            cond1.append(CheckEq(c.cond, s1[-1], True))
            s1.pop()

            cond2.append(CheckEq(c.cond, s2[-1], False))
            s2.pop()
            s2.append(c.to_push)

            new_stacks.append((cond1, s1))
            new_stacks.append((cond2, s2))

    stacks = new_stacks


value_mappings = [0] * 14

maximize = argv[1] == "1"

for cs, s in stacks:
    if len(s) == 0:
        for c in cs:
            l, r = c.left.name, c.right.name
            lv, rv = c.left.right, c.right.right

            if lv > rv:
                l, r = r, l
                lv, rv = rv, lv

            rv -= lv

            r_final = 9 - rv if maximize else 1
            l_final = r_final + rv

            value_mappings[int(r[1:])] = r_final
            value_mappings[int(l[1:])] = l_final

print("".join(map(str, value_mappings)))
