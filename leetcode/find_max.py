#!/usr/bin/env python

def find_max(array):
    # return find_max_by_recursion(array)
    # return find_max_by_partition(array)
    return find_k_large_by_partition(array, 3)

def find_max_by_recursion(array):
    array_size = len(array)
    if array_size <= 0:
        return None
    elif array_size == 1:
        return array[0]
    else:
        mid = array_size // 2
        left_max = find_max(array[:mid])
        right_max = find_max(array[mid:])
        return max(left_max, right_max)


def find_max_by_partition(array):
    return find_k_large_by_partition(array, 1)


def find_k_large_by_partition(array, k):
    if not array or len(array) < k or k <= 0:
        return None
    nth = len(array) - k
    find_kth_value(array, 0, len(array)-1, nth)
    return array[nth]


def find_kth_value(array, start, end, k):
    if start >= end:
        return

    mid = partition(array, start, end)

    if mid == k:
        return
    else:
        find_kth_value(array, start, mid-1, k)
        find_kth_value(array, mid+1, end, k)


def partition(array, start, end):
    target = array[end]
    less, greater = start, end
    while less < greater:
        if array[less] >= target:
            greater -= 1
            array[less], array[greater] = array[greater], array[less]
        else:
            less += 1
    array[greater], array[end] = array[end], array[greater]

    return greater


if __name__ == "__main__":
    print(find_max([]))
    print(find_max([2]))
    print(find_max([1, 5, 3, 12, -4, 2, 4, 7, 3]))
