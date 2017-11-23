#!/usr/bin/env python


def gen_subset(candidates):
    if not candidates:
        return None

    partial_res = []
    results = []
    gen_subset_iter(partial_res, results, candidates)
    return results


def gen_subset_iter(partial_res, results, candidates):
    results.append(partial_res[:])

    for m in range(len(candidates)):
        partial_res.append(candidates[m])
        gen_subset_iter(partial_res, results, candidates[m+1:])
        partial_res.pop()


if __name__ == "__main__":
    print(gen_subset([1, 2, 3]))
