#!/usr/bin/env python3


def less(array, target):
    # return less_iterate(array, target)
    return less_recursion(array, 0, len(array)-1, target)


def less_iterate(array, target):
    if not array:
        raise Exception()

    low = 0
    high = len(array) - 1
    while low < high:
        mid = (low + high) // 2
        # print("before: low: {}, high: {}, mid: {}".format(low, high, mid))
        if array[mid] < target:
            low = mid + 1
        else:
            high = mid - 1
        # print("after: low: {}, high: {}, mid: {}".format(low, high, mid))
    return low


def less_recursion(array, low, high, target):
    if low > high: return low

    mid = (low + high) // 2
    if array[mid] < target:
        return less_recursion(array, mid+1, high, target)
    else:
        return less_recursion(array, low, mid-1, target)


if __name__ == "__main__":
    array = [0, 1, 1, 1, 8, 8, 8, 20, 20, 20, 22]
    print(less(array, 1))
    print(less(array, 5))
    print(less(array, 8))
    print(less(array, 13))
    print(less(array, 20))
    print(less(array, 50))
    print(less(array, -100))
    print(less(array, 100))
