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
import os
import sys

import config
from server.core import ServerFactory
from server.linux_x11.x11_xdotool import XdotoolPlatformRpcs

if __name__ == '__main__':
    if '-d' in sys.argv or '--daemon' in sys.argv:
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

    platform_rpcs = XdotoolPlatformRpcs(config)
    server = ServerFactory.from_config(platform_rpcs, config)
    server.serve_forever()
