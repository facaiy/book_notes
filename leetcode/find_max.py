#!/usr/bin/env python

def find_max(array):
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


if __name__ == "__main__":
    print(find_max([]))
    print(find_max([2]))
    print(find_max([1, 5, 3, 12, -4, 2, 4, 7, 3]))
