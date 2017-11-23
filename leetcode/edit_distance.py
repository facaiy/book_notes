#!/usr/bin/env python


def edit_distance(sa, sb):
    import numpy as np

    sa_s = len(sa) + 1
    sb_s = len(sb) + 1

    # 表，从0个字符开算
    t = np.zeros((sa_s, sb_s))
    for m in range(sa_s):
        t[m, 0] = m
    for n in range(sb_s):
        t[0, n] = n

    for m in range(1, sa_s):
        for n in range(1, sb_s):
            if sa[m-1] == sb[n-1]:  # m是第几个字符，index-1
                t[m, n] = t[m-1, n-1]
            else:
                t[m, n] = min(t[m-1, n-1], t[m-1, n], t[m, n-1]) + 1

    return t[-1, -1]


def edit_distance_mini(sa, sb):
    import numpy as np

    sa_s, sb_s = len(sa) + 1, len(sb) + 1

    t = np.zeros((2, sb_s))
    t[0] = range(sb_s)

    for m in range(1, sa_s):
        t[1, 0] = m
        for n in range(1, sb_s):
            if sa[m-1] == sb[n-1]:
                t[1, n] = t[0, n-1]
            else:
                t[1, n] = min(t[0, n-1], t[0, n], t[1, n-1]) + 1
        t[0] = t[1]

    return t[-1, -1]


def lcs_distance(sa, sb):
    """insertions and deletions only"""
    import numpy as np

    sa_s, sb_s= len(sa)+1, len(sb)+1

    # 表，从0个字符开算
    t = np.zeros((sa_s, sb_s))
    for m in range(sa_s):
        t[m, 0] = m
    for n in range(sb_s):
        t[0, n] = n

    for m in range(1, sa_s):
        for n in range(1, sb_s):
            if sa[m-1] == sb[n-1]:  # m是第几个字符，index-1
                t[m, n] = t[m-1, n-1]
            else:
                t[m, n] = min(t[m-1, n-1] + 2, # replace = del + insert
                              t[m-1, n] + 1,   # del
                              t[m, n-1] + 1)   # insert

    return t[-1, -1]


if __name__ == "__main__":
    res = edit_distance("kitten", "sitting")  #edit dist=3
    print("edit distance: {}".format(res))
    res = edit_distance_mini("kitten", "sitting")  #edit dist=3
    print("edit distance: {}".format(res))

    res = lcs_distance("kitten", "sitting")  #lsc dist=5
    print("lsc distance: {}".format(res))
