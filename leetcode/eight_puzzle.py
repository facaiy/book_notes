#!/busr/bin/env python

import numpy as np


def eight_puzzle():
    res = np.zeros(8)
    eight_puzzle_iter(res, 0)


def eight_puzzle_iter(res, row):
    if row == 8:
        output(res)
    else:
        for col in range(8):
            res[row] = col
            if constrain(res, row):
                eight_puzzle_iter(res, row+1)


def constrain(res, row):
    for m in range(row):
        if res[m] == res[row] or abs(m - row) == abs(res[m] - res[row]):
            return False
    return True


def output(res):
    print(res)


if __name__ == "__main__":
    eight_puzzle()
