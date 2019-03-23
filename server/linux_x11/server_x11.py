#!/usr/bin/python

# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (2014) Alex Roper
# Alex Roper <alex@aroper.net>
import argparse
import os
import sys
from os.path import join, dirname, realpath

# enable server.core imports by adding the root of the aenea project to path
sys.path.append(realpath(join(dirname(__file__), '../../')))

import config
from server.core import AeneaServer



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
    parser = argparse.ArgumentParser(description='Aenea Linux X11 Server')
    parser.add_argument(
        '--daemon', action='store_const', const=True, default=False,
        required=False, help='If provided the server runs in the background.')
    parser.add_argument(
        '--input', action='store', type=str, default='xdotool',
        choices=('xdotool', 'libxdo'), required=False, dest='impl',
        help='Aenea Server Input Method.  Providing the default, '
                    '"xdotool" will make the server shell out to the xdotool '
                    'program to emulate input. "libxdo" will cause the server '
                    'to make calls to the xdo library.')

    arguments = parser.parse_args()

    if arguments.impl == 'xdotool':
        from server.linux_x11.x11_xdotool import XdotoolPlatformRpcs
        platform_rpcs = XdotoolPlatformRpcs(config)
    elif arguments.impl == 'libxdo':
        from server.linux_x11.x11_libxdo import XdoPlatformRpcs
        platform_rpcs = XdoPlatformRpcs(security_token=getattr(config, 'SECURITY_TOKEN', None))

    if arguments.daemon:
        daemonize()

    server = AeneaServer.from_config(platform_rpcs, config)
    server.serve_forever()
