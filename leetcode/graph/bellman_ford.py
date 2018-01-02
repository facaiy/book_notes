#!/usr/bin/env python


import numpy as np
import pandas as pd


def find_short(adj_matrix, start_node):
    if adj_matrix.empty: return []

    assert adj_matrix.shape[0] == adj_matrix.shape[1]

    mask = adj_matrix.iloc[:, 0].copy()
    mask[:] = np.inf
    mask[start_node] = 0.0

    edges = np.nonzero(adj_matrix.as_matrix())
    for _ in range(adj_matrix.shape[0]):
        for (c, r) in zip(*edges):
            edge = adj_matrix.index[int(c)], adj_matrix.index[int(r)]
            relax(edge, adj_matrix, mask)

    for (c, r) in zip(*edges):
        edge = adj_matrix.index[int(c)], adj_matrix.index[int(r)]
        if mask[r] > mask[c] + adj_matrix.loc[edge]:
            raise ValueError("Detect: negative cycle in graph.")

    return mask


def relax(edge, adj_matrix, mask):
    from_node, to_node = edge
    mask[to_node] = min(mask[to_node], mask[from_node] + adj_matrix.loc[edge])


# def relax(node, adj_matrix, mask):
#     for from_node in np.where(adj_matrix[:, node] != 0):
#         mask[node] = np.min(mask[node],
#                             mask[from_node] + adj_matrix[from_node, node])


def adj_edges2matrix(edges, nodes):
    if not edges or not nodes:
        return [[]]

    nodes_size = len(nodes)
    zeros = np.zeros((nodes_size, nodes_size))
    adj_matrix = pd.DataFrame(zeros, index=nodes, columns=nodes)

    for node, to_node, weight in edges:
        adj_matrix.loc[node, to_node] = weight

    return adj_matrix


if __name__ == "__main__":
    edges = [("s", "t", 3), ("s", "y", 5),
             ("t", "y", 2), ("t", "x", 6),
             ("x", "z", 2),
             ("z", "x", 7), ("z", "s", 3),
             ("y", "t", 1), ("y", "x", 4), ("y", "z", 6)]
    nodes = list("stxyz")
    adj_matrix = adj_edges2matrix(edges, nodes)
    print(adj_matrix)
    print("res:")
    print(find_short(adj_matrix, "s"))
