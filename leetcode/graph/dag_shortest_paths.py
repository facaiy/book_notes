#!/usr/bin/env python

from functools import reduce


import numpy as np
import pandas as pd


def find_short(adj_matrix, start_node):
    nodes = adj_matrix.index
    nodes_distance = pd.Series(np.ones_like(nodes) * np.inf, index=nodes)
    nodes_distance[start_node] = 0

    for node in sort(adj_matrix):
        neighbors = adj_matrix.loc[node]
        neighbors = neighbors[neighbors != 0]
        for to_node in neighbors.index:
            nodes_distance[to_node] = min(nodes_distance[to_node],
                                          nodes_distance[node] + adj_matrix.loc[node, to_node])
    return nodes_distance


def sort(adj_matrix):
    nodes = adj_matrix.index
    node_mark = pd.Series(np.zeros_like(nodes), index=nodes)
    res = []
    for node in nodes:
        res = iter_find(node, adj_matrix, node_mark) + res
    return res


def iter_find(node, adj_matrix, node_mark):
    if node_mark[node] == -1:
        return []

    node_mark[node] = -1

    edges = adj_matrix.loc[node]
    edges = edges[edges != 0]
    res = []
    for to_node in edges.index:
        if node_mark[to_node] != -1:
            res.append(iter_find(to_node, adj_matrix, node_mark))

    res = reduce(lambda x, y: x+y, res[::-1]) if res else []
    res = [node] + res
    return res


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
    edges = [("r", "s", 5), ("r", "t", 3),
             ("s", "t", 2), ("s", "x", 6),
             ("t", "x", 7), ("t", "y", 4), ("t", "z", 2),
             ("x", "z", 1), ("x", "y", -1),
             ("y", "z", -2)]
    nodes = list("rstxyz")
    adj_matrix = adj_edges2matrix(edges, nodes)
    print(adj_matrix)
    sort_nodes = sort(adj_matrix)
    print(sort_nodes)
    print("res:")
    print(find_short(adj_matrix, "s"))
