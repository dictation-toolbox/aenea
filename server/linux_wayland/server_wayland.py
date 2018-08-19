#!/usr/bin/python
import argparse
import os
import sys
from os.path import join, dirname, realpath

# enable server.core imports by adding the root of the aenea project to path
sys.path.append(realpath(join(dirname(__file__), '../../')))

import config
from server.core import AeneaServer
from evdevImpl import EvdevPlatformRpcs

MAPPINGS = "qwerty, azerty"

def daemonize():
    if os.fork() == 0:
        os.setsid()
        if os.fork() == 0:
            os.chdir('/')
            os.umask(0)
            # Safe upper bound on number of fds we could
            # possibly have opened.
            for fd in range(64):
                try:
                    os.close(fd)
                except OSError:
                    pass
            os.open(os.devnull, os.O_RDWR)
            os.dup2(0, 1)
            os.dup2(0, 2)
        else:
            os._exit(0)
    else:
        os._exit(0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Aenea Linux Wayland Server')
    parser.add_argument('--daemon',
                        action='store_const',
                        const=True,
                        default=False,
                        required=False,
                        help='If provided the server runs in the background.')
    parser.add_argument('--mapping',
                        action = 'store',
                        default="qwerty",
                        required=False,
                        help='If provided the server uses another keyboard mapping than qwerty. Possible mappings are: {}'.format(MAPPINGS))

    arguments = parser.parse_args()

    platform_rpcs = EvdevPlatformRpcs(config,
                                      arguments.mapping)

    if arguments.daemon:
        daemonize()

    server = AeneaServer.from_config(platform_rpcs, config)
    server.serve_forever()
