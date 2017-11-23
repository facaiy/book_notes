#!/usr/bin/env python


def find_max_diff(es):
    if len(es) < 2:
        return 0

    min_val = es[0]
    max_diff = 0
    for e in es:
        min_val = min(min_val, e)
        max_diff = max(max_diff, e - min_val)

    return max_diff


if __name__ == "__main__":
    res = find_max_diff([2, 8, 3, 4, 1, 7, 2, 10, 1, 5])
    print("res: {}".format(res))
