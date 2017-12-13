#!/usr/bin/env python


import numpy as np


adjacency_matrix = np.array([
    [0, 1, 0, 0, 1],
    [1, 0, 1, 1, 1],
    [0, 1, 0, 1, 0],
    [0, 1, 1, 0, 1],
    [1, 1, 0, 1, 0]])


def matrix2list(matrix):
    w, h = matrix.shape
    assert w == h

    return {c: np.ravel(np.argwhere(matrix[c] == 1)).tolist() for c in range(w)}


adjacency_list = matrix2list(adjacency_matrix)


if __name__ == "__main__":
    print("matrix:")
    print(adjacency_matrix)
    print("list:")
    print(adjacency_list)
