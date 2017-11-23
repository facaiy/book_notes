#!/usr/bin/env python3


def greater(array, target):
    # return binary_search_recursion(array, target)
    return greater_iterate(array, target)


def greater_iterate(array, target):
    if not array:
        raise Exception()

    low = 0
    high = len(array) - 1
    while low < high:
        mid = low + (high - low + 1) // 2
        if array[mid] > target:
            high = mid - 1
        else:
            low = mid + 1
    return low


if __name__ == "__main__":
    array = [0, 1, 1, 1, 8, 8, 8, 20, 20, 20, 22]
    print(greater(array, 1))
    print(greater(array, 5))
    print(greater(array, 8))
    print(greater(array, 13))
    print(greater(array, 20))
    print(greater(array, 50))
    print(greater(array, -100))
    print(greater(array, 100))
