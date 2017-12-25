#!/usr/bin/env python


import numpy as np


def sort(array):
    return sort_iter(array)


def sort_iter(array):
    if not array:
        return []
    if len(array) == 1:
        return array.copy()
    elif len(array) == 2:
        return [min(array), max(array)]
    else:
        mid = len(array) // 2
        left = sort_iter(array[:mid])
        right = sort_iter(array[mid:])

        res = []
        l_m, r_m = 0, 0
        l_s, r_s = len(left), len(right)
        while l_m < l_s or r_m < r_s:
            l = left[l_m] if l_m < l_s else np.inf
            r = right[r_m] if r_m < r_s else np.inf
            if l <= r:
                res.append(l)
                l_m += 1
            else:
                res.append(r)
                r_m += 1
        return res


if __name__ == "__main__":
    array = [1]
    print(sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(sort(array))
