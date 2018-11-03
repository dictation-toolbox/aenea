#!/usr/bin/env python
# encoding: utf-8


import os
import sys


def main():
    fail = False

    for fn in sys.argv[1:]:
        fail = os.system('python %s' % fn) != 0 or fail

    if fail:
        sys.exit(-1)


if __name__ == '__main__':
    main()
