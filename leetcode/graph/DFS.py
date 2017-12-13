#!/usr/bin/env python

from collections import OrderedDict

import numpy as np


def mark_by_dfs(adjacency_list):
    if not adjacency_list: return None

    mark = {k: [-1, -1] for k in adjacency_list}
    step = 1
    for node_id in adjacency_list:
        step, mark = iter_mark(node_id, adjacency_list, mark, step)
    return step, mark


def iter_mark(node_id, adjacency_list, mask, step):
    if mask[node_id][0] != -1:
        return step, mask

    mask[node_id][0] = step
    step += 1

    neighbors = [n for n in adjacency_list[node_id] if mask[n][0] == -1]
    for n in neighbors:
        step, _ = iter_mark(n, adjacency_list, mask, step)

    mask[node_id][1] = step
    step += 1

    return step, mask


if __name__ == "__main__":
    adjacency_list = OrderedDict([
        ("u", ("v", "x")),
        ("v", ("y")),
        ("y", ("x")),
        ("x", ("v")),
        ("w", ("y", "z")),
        ("z", ("z"))])
    _, mask = mark_by_dfs(adjacency_list)
    print(sorted(mask.items(), key=lambda x: x[0]))
