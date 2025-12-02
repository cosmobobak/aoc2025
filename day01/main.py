import sys
import math

def main():
    alignments = 0
    angle = 50
    for cmd in map(str.rstrip, sys.stdin):
        if not cmd: continue
        dir = cmd[0]
        val = int(cmd[1:])
        dir = 1 if dir == 'R' else -1
        # span = range(angle + dir, angle + val * dir + dir, dir)
        # alignments += sum(x%100==0 for x in span)
        a = angle + val * dir
        b = angle + dir
        a, b = min(a, b), max(a, b)
        hundreds = (b + 99) // 100 - a // 100
        alignments += hundreds
        angle = angle + val * dir
        angle %= 100
    print(alignments)

if __name__ == "__main__":
    main()
