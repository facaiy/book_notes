#!/usr/bin/env python


def find_2nd_major_vote(array):
    if not array:
        return None

    x, cx = 0, 0
    for a in array:
        if a == x:
            cx += 1
        elif cx == 0:
            x, cx = a, 1
        else:
            cx -= 1

    cx = 0
    for a in array:
        if a == x:
            cx += 1

    if cx >= len(array) // 2:
        return x
    else:
        return None


def find_3nd_major_vote(array):
    if not array:
        return []

    y, cy = 0, 0
    z, cz = 1, 0

    for a in array:
        if a == y:
            cy += 1
        elif a == z:
            cz += 1
        elif cy == 0:
            y, cy = a, 1
        elif cz == 0:
            z, cz = a, 1
        else:
            cy -= 1
            cz -= 1

    assert y != z

    cy, cz = 0, 0
    for a in array:
        if a == y:
            cy += 1
        elif a == z:
            cz += 1

    res = []
    if cy >= len(array) // 3:
        res.append(y)
    if cz >= len(array) // 3:
        res.append(z)

    return res


if __name__ == "__main__":
    print("2nd:")
    array = [1, 2, 2, 0, 2, 2, 2, 5, 2, 8, 2, 9, 2, 3, 2]
    print(find_2nd_major_vote(array))

    print("3nd:")
    array = [1, 2, 3, 0, 2, 3, 2, 5, 3, 8, 3, 9, 2, 3, 2]
    print(find_3nd_major_vote(array))
