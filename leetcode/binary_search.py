#!/usr/bin/env python3


def binary_search(array, target):
    # return binary_search_recursion(array, target)
    return binary_search_iterate(array, target)


def binary_search_iterate(array, target):
    if not array:
        return False

    low = 0
    high = len(array) - 1
    while low <= high:
        mid = low + (high - low) // 2
        if array[mid] == target:
            return True
        elif array[mid] > target:
            high = mid - 1
        else:
            low = mid + 1
    return False


def binary_search_recursion(array, target):
    if not array:
        return False

    low = 0
    high = len(array)
    mid = (low + high) // 2

    if array[mid] == target:
        return True
    elif array[mid] > target:
        return binary_search_recursion(array[low:mid], target)
    else:
        return binary_search_recursion(array[mid+1:high], target)


if __name__ == "__main__":
    array = [0, 1, 3, 5, 8, 10, 15, 19, 20, 21, 22]
    print(binary_search(array, 1))
    print(binary_search(array, 20))
    print(binary_search(array, 8))
    print(binary_search(array, 13))
    print(binary_search(array, -5))
    print(binary_search(array, 500))
