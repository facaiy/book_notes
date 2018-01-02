#!/usr/bin/env python


def quick_sort(array):
    if not array:
        return

    # sort(array, 0, len(array)-1)
    sort3(array, 0, len(array)-1)
    return array
    # return sort2(array)


def sort(array, start, end):
    if end < start:
        return
    elif end == start:
        return

    target = array[end]

    less = start - 1
    for m in range(start, end):
        if array[m] < target:
            less += 1
            array[less], array[m] = array[m], array[less]
    array[end], array[less+1] = array[less+1], array[end]

    sort(array, start, less)
    sort(array, less+2, end)
    return


def sort2(array):
    if not array:
        return []

    target = array[-1]
    less = [x for x in array[:-1] if x < target]
    greater = [x for x in array[:-1] if x >= target]
    return sort2(less) + [target] + sort2(greater)


def sort3(array, start, end):
    if start >= end:
        return

    target = array[end]
    less, greater = start, end
    while less < greater:
        if array[less] >= target:
            greater -= 1
            array[less], array[greater] = array[greater], array[less]
        else:
            less += 1
    array[greater], array[end] = array[end], array[greater]

    sort3(array, start, greater-1)
    sort3(array, greater+1, end)
    return


if __name__ == "__main__":
    array = [1]
    print(quick_sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(quick_sort(array))
