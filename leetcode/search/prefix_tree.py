#!/usr/bin/env python


class HashImpl(object):
    def __init__(self):
        self._tree = {}

    def insert(self, word):
        if word:
            t = self._tree
            for a in list(word):
                if a in t:
                    t = t[a]
                else:
                    t[a] = {}
                    t = t[a]

    def startsWith(self, word):
        if not word:
            return True

        t = self._tree
        for a in list(word):
            if a in t:
                t = t[a]
            else:
                return False
        return True

    def index(self, word):
        if not word:
            return -1

        t = self._tree
        for idx, a in enumerate(list(word)):
            if a in t:
                t = t[a]
            else:
                return idx
        return len(word) - 1


if __name__ == '__main__':
    tree = HashImpl()
    tree.insert('abc')
    tree.insert('abcd')
    tree.insert('abce')
    tree.insert('bcd')
    print('abcdefg', tree.index('abcdefg'))
    print('bcefg', tree.index('bcefg'))
