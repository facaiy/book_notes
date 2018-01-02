#!/usr/bin/env python


import numpy as np


def sample(seq, k):
    s = np.ones(k) * np.nan

    for idx, e in enumerate(seq):
        if idx < k:
            s[idx] = e
        else:
            new_idx = np.random.randint(0, idx+1)
            if new_idx < k:
                s[new_idx] = e

    return s


if __name__ == "__main__":
    np.random.seed(0)
    seq = [4,5,6,3,4,7,7,4,3,3,2,4,5,5,6,9,5,4,3,45,3,23,44,55,33,5,8]
    k = 5
    print(sample(seq, k))
