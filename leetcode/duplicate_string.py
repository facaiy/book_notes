#!/usr/bin/env python


import numpy as np
import unittest


def duplicate_string(string):
    pi = gen_prefix(string)

    start = pi[-1]
    return string + string[start+1:]


def gen_prefix(match):
    if not match:
        raise Exception()

    pi = np.ones(len(match), dtype=np.int32) * -1

    if len(match) == 1:
        return pi
    else:
        k = -1
        for m in range(1, len(match)):
            while k > -1 and match[k+1] != match[m]:
                k = pi[k]
            if match[k+1] == match[m]:
                k += 1
            pi[m] = k
        return pi


class Test(unittest.TestCase):
    def test_basic(self):
        self.assertEqual(duplicate_string("123ab123"), "123ab123ab123")
        self.assertEqual(duplicate_string("a"), "aa")
        self.assertEqual(duplicate_string("aa"), "aaa")
        self.assertEqual(duplicate_string("ab"), "abab")


if __name__ == "__main__":
    unittest.main()
