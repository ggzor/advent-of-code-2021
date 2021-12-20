from sys import argv, stdin
from collections import defaultdict

text = stdin.read()
lines = text.splitlines()

algo = lines[0]
img = defaultdict(
    lambda: ".", {(y, x): v for y, l in enumerate(lines[2:]) for x, v in enumerate(l)}
)


def get_ranges(img):
    return (
        min(map(lambda x: x[0], img)),
        min(map(lambda x: x[1], img)),
        max(map(lambda x: x[0], img)),
        max(map(lambda x: x[1], img)),
    )


count = 0


def get_val(data):
    data = data.replace(".", "0")
    data = data.replace("#", "1")
    data = int(data, 2)
    data = algo[data]
    return data


def step(img):
    global count
    count += 1
    min_y, min_x, max_y, max_x = get_ranges(img)

    default = algo[0] if count % 2 == 1 else get_val(algo[0] * 9)
    new_img = defaultdict(lambda: default)
    for y in range(min_y - 1, max_y + 2):
        for x in range(min_x - 1, max_x + 2):
            data = "".join(
                img[(y + dy, x + dx)] for dy in range(-1, 2) for dx in range(-1, 2)
            )
            data = get_val(data)

            new_img[(y, x)] = data

    return new_img


if argv[1] == "1":
    img = step(step(img))
else:
    for i in range(50):
        img = step(img)

print(sum(v == "#" for v in img.values()))
