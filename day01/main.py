import sys


def main():
    alignments = 0
    angle = 50
    for cmd in map(str.rstrip, sys.stdin):
        if not cmd:
            continue
        dir = 1 if cmd[0] == "R" else -1
        val = int(cmd[1:])
        a = angle + val * dir
        b = angle + dir
        a, b = min(a, b), max(a, b)
        alignments += max(b // 100 - (a + 99) // 100 + 1, 0)
        angle = (angle + val * dir) % 100

    print(alignments)


if __name__ == "__main__":
    main()
