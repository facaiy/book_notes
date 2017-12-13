#!/usr/bin/env python


import numpy as np


def mark_by_bfs(adjacency_list):
    if not adjacency_list: return None

    mark = np.ones(len(adjacency_list)) * -1
    step = 0
    for node_id in range(len(adjacency_list)):
        step, mark = iter_mark(node_id, adjacency_list, mark, step)
    return step, mark


def iter_mark(node_id, adjacency_list, mark, step):
    if mark[node_id] != -1:
        return step, mark

    queue = [node_id]
    while queue:
        queue_new = []
        for q in queue:
            mark[q] = step
            step += 1
            queue_new = queue_new + adjacency_list[q]
        queue = [q for q in set(queue_new) if mark[q] == -1]
    return step, mark


if __name__ == "__main__":
    from graph import adjacency_list

    _, mark = mark_by_bfs(adjacency_list)
    print(mark)
