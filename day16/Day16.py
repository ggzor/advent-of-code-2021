from functools import reduce
from sys import argv, stdin
import operator as op

text = stdin.read().strip()
num = "".join(("0000" + bin(int(c, 16))[2:])[-4:] for c in text)


def parse(num):
    version = int(num[:3], 2)
    ty = int(num[3:6], 2)

    if ty == 4:
        i = 6
        bnum = ""
        times = 0
        while True:
            c = num[i]
            bnum += num[i + 1 : i + 5]
            i += 5
            times += 5
            if c == "0":
                break
        return (version, ty, int(bnum, 2)), num[i:]
    else:
        lenty = num[6]
        i = 7
        if lenty == "0":
            sublen = int(num[i : i + 15], 2)
            i += 15
            sub = num[i : i + sublen]

            parts = []
            rem = sub
            while rem != "":
                res, rem = parse(rem)
                parts.append(res)
            return (version, ty, parts), num[i + sublen :]
        else:
            sublen = int(num[i : i + 11], 2)
            i += 11

            parts = []
            rem = num[i:]
            while rem != "" and len(parts) < sublen:
                res, rem = parse(rem)
                parts.append(res)

            return ((version, ty, parts), rem)


res = parse(num)[0]


if argv[1] == "1":

    def version_sum(packet):
        version, _, parts = packet
        if isinstance(parts, list):
            return version + sum(map(version_sum, parts))
        else:
            return version

    print(version_sum(res))

else:

    def eval_packet(packet):
        version, ty, parts = packet

        if ty == 4:
            return parts
        else:
            sub = map(eval_packet, parts)

            if ty == 0:
                return sum(sub)
            elif ty == 1:
                return reduce(op.mul, sub)
            elif ty == 2:
                return reduce(min, sub)
            elif ty == 3:
                return reduce(max, sub)
            elif ty == 5:
                x, y = sub
                return int(x > y)
            elif ty == 6:
                x, y = sub
                return int(x < y)
            elif ty == 7:
                x, y = sub
                return int(x == y)

    print(eval_packet(res))
