#!/usr/bin/env python


def sort(array):
    if not array:
        return array

    if len(array) == 1:
        return array

    for m in range(1, len(array)):
        tmp = array[m]
        for prev in range(m-1, -1, -1):
            if array[prev] > tmp:
                array[prev+1] = array[prev]
            else:
                array[prev+1] = tmp
                break
        else:
            array[0] = tmp
        # print('step:', m, ' array: ', array)
    return array


if __name__ == '__main__':
    array = [1]
    print(sort(array))

    array = [25, 1, 8, 2, 5, 9, 25, 3, 2, 16]
    print(sort(array))
