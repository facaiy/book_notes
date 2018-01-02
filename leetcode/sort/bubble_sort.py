#!/usr/bin/env python


def sort(array):
    if not array:
        return array

    if len(array) == 1:
        return array

    for m in range(len(array)-1, -1, -1):
        swap = False
        for n in range(0, m):
            if array[n] > array[n+1]:
                array[n], array[n+1] = array[n+1], array[n]
                swap = True
        if not swap:
            break
        # print("m: ", m, " array: ", array)

    return array


if __name__ == "__main__":
    array = [1]
    print(sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(sort(array))
