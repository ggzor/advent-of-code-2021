#!/usr/bin/env python3

from itertools import dropwhile, takewhile
from pathlib import Path

TEMPLATE = "utils/template.md"
SHORT_HS_SOLUTION = {1, 2, 3, 5, 6, 7, 10}

PROBLEM_NAMES = [
    "Sonar Sweep",
    "Dive!",
    "Binary Diagnostic",
    "Giant Squid",
    "Hydrotermal Venture",
    "Lanternfish",
    "The Treachery of Whales",
    "Seven Segment Search",
    "Smoke Basin",
    "Syntax Scoring",
    "Dumbo Octopus",
    "Passage Pathing",
    "Transparent Origami",
    "Extended Polymerization",
    "Chiton",
    "Packet Decoder",
    "Trick Shot",
    "Snailfish",
    "Beacon Scanner",
    "Trench Map",
    "Dirac Dice",
    "Reactor Reboot",
]

FILE_EXT_LANG = {
    "scala": "Scala",
    "hs": "Haskell",
    "py": "Python",
    "rs": "Rust",
}


def get_haskell_line_ranges(day):
    with open(f"day{day:02}/Day{day:02}.hs") as f:
        lines = list(
            map(
                lambda x: x[0],
                takewhile(
                    lambda x: not x[1].startswith("solutions"),
                    dropwhile(
                        lambda x: len((l := x[1].strip())) == 0
                        or l.startswith("import")
                        or l.startswith("{-"),
                        enumerate(f),
                    ),
                ),
            )
        )

        return lines[0] + 1, lines[-1]


def compute_line_numbers(day, ext):
    if ext == "hs":
        if day in SHORT_HS_SOLUTION:
            start, end = get_haskell_line_ranges(day)
            return f"#L{start}-L{end}"

    return ""


with open(TEMPLATE, "r", encoding="utf8") as f:
    for l in f:
        if l.startswith("<!--TABLE-->"):
            print(
                "|Day|Title|"
                + "|".join("L" + str(i) for i in range(1, len(FILE_EXT_LANG) + 1))
                + "|"
            )
            print("|" + "|".join("---" for _ in range(len(FILE_EXT_LANG) + 2)) + "|")
            for day in range(1, len(PROBLEM_NAMES) + 1):
                print(
                    f"|[{day:02}](https://adventofcode.com/2021/day/{day})|{PROBLEM_NAMES[day - 1]}|",
                    end="",
                )
                for ext, lang in FILE_EXT_LANG.items():
                    target_file = Path(f"./day{day:02}/Day{day:02}.{ext}")
                    if target_file.exists():
                        link = (
                            f"[{lang}]({target_file}{compute_line_numbers(day, ext)})"
                        )
                    else:
                        link = ""

                    print(
                        f"{link}|",
                        end="",
                    )
                print()
        else:
            print(l, end="")
