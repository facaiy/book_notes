#!/usr/bin/env python

import numpy as np


def find_substr(text, match):
    if not text:
        raise Exception()

    pi = gen_prefix(match)

    k = -1
    for m in range(len(text)):
        while k > -1 and match[k+1] != text[m]:
            k = pi[k]
        if match[k+1] == text[m]:
            k = k + 1
        if k == len(match) - 1:
            start = m - len(match) + 1
            end = m + 1
            print("start: {}, end: {}, find: {}".format(start, end, text[start:end]))
            k = pi[k]


def gen_prefix(match):
    if not match:
        raise Exception()

    pi = np.ones(len(match), dtype=np.int32) * -1

    if len(match) == 1:
        return pi
    else:
        k = -1
        for m in range(1, len(match)):
            while k > -1 and match[k+1] != match[m]:
                k = pi[k]
            if match[k+1] == match[m]:
                k = k + 1
            pi[m] = k
        return pi


if __name__ == "__main__":
    text = "bacbababacaabcbab"
    match = "ababaca"
    print("text: {}\nmatch: {}".format(text, match))
    print("pi\n:{}".format(gen_prefix(match)))
    find_substr(text, match)
