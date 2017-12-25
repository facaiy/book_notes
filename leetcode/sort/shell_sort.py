#!/usr/bin/env python


def sort(array):
    if not array:
        return array

    array_s = len(array)

    gap = array_s // 2
    while gap > 0:
        for m in range(gap, array_s):
            tmp = array[m]
            for n in range(m-gap, -1, -gap):
                if array[n] > tmp:
                    array[n+gap] = array[n]
                else:
                    array[n+gap] = tmp
                    break
            else:
                array[n] = tmp
        gap = gap // 2
    return array


if __name__ == '__main__':
    array = [1]
    print(sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(sort(array))
