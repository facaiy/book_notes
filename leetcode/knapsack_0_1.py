#!/usr/bin/env python


import numpy as np


def knapsack(data, max_weight):
    if not data: return None

    rows = max_weight + 1
    cols = len(data[0]) + 1
    dp = np.zeros((rows, cols))

    for w in range(1, rows):
        for n in range(1, cols):
            id_ = n - 1
            if data[0][id_] > w:
                dp[w][n] = dp[w][n-1]
            else:
                weight = data[0][id_]
                value = data[1][id_]
                dp[w][n] = max(dp[w-weight][n-1] + value,
                               dp[w][n-1])

    # print(dp)
    return int(max(dp[-1]))


if __name__ == "__main__":
    data = [[10, 20, 30],    # weight
            [60, 100, 120]]  # value
    print(knapsack(data, 50))
