#!/usr/bin/env python


import heapq


def sort(array):
    if not array:
        return array

    heap = array.copy()
    heapq.heapify(heap)
    res = []
    while heap:
        res.append(heapq.heappop(heap))
    return res


if __name__ == "__main__":
    array = [1]
    print(sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(sort(array))
