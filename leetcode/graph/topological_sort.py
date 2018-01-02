#!/usr/bin/env python


from functools import reduce

import numpy as np


def sort(adjacency_dict, nodes):
    if not adjacency_dict:
        return []

    assert len(set(nodes)) == len(nodes)

    mask = {n: -1 for n in nodes}
    res = []
    for n in nodes:
        res = iter_find(n, adjacency_dict, mask) + res
    return res


def iter_find(node, adjacency_dict, mask):
    if mask[node] != -1:
        return []

    mask[node] = 1

    res = [iter_find(n, adjacency_dict, mask)
           for n in adjacency_dict.get(node, [])
           if mask[n] == -1]
    if res:
        res = [node] + reduce(lambda x, y: x + y, res[::-1])
    else:
        res = [node]
    return res


if __name__ == "__main__":
    adjacency_dict = {
            "shirt": ["tie", "belt"],
            "tie": ["jacket"],
            "belt": ["jacket"],
            "undershorts": ["pants", "shoes"],
            "pants": ["belt", "shoes"],
            "socks": ["shoes"]}
    nodes = ["shirt", "tie", "jacket", "belt",
             "watch",
             "undershorts", "pants", "shoes",
             "socks"]
    print(sort(adjacency_dict, nodes))
