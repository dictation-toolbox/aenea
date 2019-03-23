#!/usr/bin/env python
# encoding: utf-8

import os
import base64
import json


def main():
    token = base64.b64encode(os.urandom(32)).decode('utf-8')

    print('Your randomly generated security token is %s' % token)

    if not append_constant(token, os.path.join('server', 'linux_x11', 'config.py')):
        append_constant(token, os.path.join('server', 'linux_x11', 'config.py.example'))

    if not add_json(token, 'aenea.json'):
        add_json(token, 'aenea.json.example')


def append_constant(token, path):
    if not os.path.exists(path):
        print('%s does not exist; not adding security token to it.' % path)
        return False

    with open(path, 'a') as fd:
        fd.write('SECURITY_TOKEN = \'%s\'\n' % token)
    print('Appended to %s.' % path)
    return True


def add_json(token, path):
    if not os.path.exists(path):
        print('%s does not exist; not adding security token to it.' % path)
        return False

    with open(path) as fd:
        data = json.load(fd)

    data['security_token'] = token

    with open(path, 'w') as fd:
        json.dump(data, fd)

    print('Added to %s.' % path)

    return True


if __name__ == '__main__':
    main()
