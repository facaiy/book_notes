#!/usr/bin/env python

def exchange_coin(target):
    assert target > 0

    coins = [50, 25, 10, 5, 1]
    partial_res = 0
    results = [0]
    exchange_coin_iter(partial_res, results, target, coins)
    return results[0]


def exchange_coin_iter(partial_res, results, target, coins):
    if partial_res == target:
        results[0] += 1
        return

    if partial_res > target:
        return

    for m in range(len(coins)):
        partial_res += coins[m]
        exchange_coin_iter(partial_res, results, target, coins[m:])
        partial_res -= coins[m]


if __name__ == "__main__":
    print(exchange_coin(5))
    print(exchange_coin(100))
