#!/usr/bin/env python
# encoding: utf-8

import os
import sys


def main():
    ok = True

    with os.popen('find -type f | egrep ".py$"') as fd:
        python_files = set(fd)
    with open('.nopyflakes') as fd:
        python_files -= set(fd)

    for filename in python_files:
        if os.system('pyflakes %s' % filename) != 0:
            ok = False

    if ok:
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == '__main__':
    main()
