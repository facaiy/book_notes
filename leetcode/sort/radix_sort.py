#!/usr/bin/env python


from functools import reduce


def sort(array):
    # return iter_from_end(array, 0, 10)
    return sort_from_head(array, 0, 10)

def sort_from_head(array, bit, base):
    if not array:
        return []

    if len(array) == 1:
        return array[:]

    assert bit >= 0
    assert base > 0

    buckets = [[] for _ in range(base)]
    div_value = base ** bit
    for e in array:
        k = int((e / div_value) % base)
        buckets[k].append(e)
    buckets = [b for b in buckets if b]
    results = reduce(lambda x, y: x + y, buckets)

    if len(buckets) > 1:
        return sort_from_head(results, bit+1, base)
    else:
        return results


def iter_from_end(array, bit, base):
    if not array:
        return []

    if len(array) == 1:
        return array[:]

    assert bit >= 0
    assert base > 0

    buckets = [[] for _ in range(base)]
    div_value = base ** bit
    for e in array:
        k = int((e / div_value) % base)
        buckets[k].append(e)
    results = [iter(b, bit+1, base) for b in buckets if b]
    results = reduce(lambda x, y: x + y, results)
    return results


if __name__ == "__main__":
    array = [329, 457, 657, 839, 436, 720, 355]
    print(sort(array))
