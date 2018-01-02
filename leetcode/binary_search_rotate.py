#!/usr/bin/env python3


def binary_search(array, target):
    return binary_search_recursion(array, target)
    # return binary_search_iterate(array, target)


def binary_search_iterate(array, target):
    pass


def binary_search_recursion(array, target):
    if not array:
        return False

    low = 0
    high = len(array) - 1
    mid = low + (high - low) // 2
    # print(array)
    # print("low: {}, high: {}, mid: {}".format(low, high, mid))

    if array[mid] == target:
        return True
    elif array[mid] > target:
        if array[low] < target:
            return binary_search_recursion(array[low+1:mid], target)
        else:
            return binary_search_recursion(array[mid+1:high+1], target)
    else:
        if array[high] >= target:
            return binary_search_recursion(array[mid+1:high+1], target)
        else:
            return binary_search_recursion(array[low+1:mid], target)


if __name__ == "__main__":
    array = [10, 15, 19, 20, 21, 22, 0, 1, 3, 5, 8]
    print(binary_search(array, 1))
    print(binary_search(array, 20))
    print(binary_search(array, 8))
    print(binary_search(array, 13))
    print(binary_search(array, -5))
    print(binary_search(array, 500))
