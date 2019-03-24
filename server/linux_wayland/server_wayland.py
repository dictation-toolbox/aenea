#!/usr/bin/python
import argparse
import os
import sys
from os.path import join, dirname, realpath
import evdev

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


def findKeyEvent():
	devices = [evdev.InputDevice(path) for path in evdev.list_devices()]
	for device in devices:
		cap = device.capabilities();
		key=cap.get(evdev.ecodes.EV_KEY)
		if key is not None:
			try:
				#generate exception if not found
				key.index(evdev.ecodes.KEY_A)
				key.index(evdev.ecodes.KEY_B)
				key.index(evdev.ecodes.KEY_C)
				return device.path
			except:
				pass
	return None

def findMouseEvent():
	devices = [evdev.InputDevice(path) for path in evdev.list_devices()]
	for device in devices:
		cap = device.capabilities();
		rel=cap.get(evdev.ecodes.EV_REL)
		if rel is not None:
			try:
				#generate exception if not found
				rel.index(evdev.ecodes.REL_X)
				rel.index(evdev.ecodes.REL_Y)
				rel.index(evdev.ecodes.REL_WHEEL)
				return device.path
			except:
				pass
	return None


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Aenea Linux Wayland Server')
    parser.add_argument('--daemon',
                        action='store_const',
                        const=True,
                        default=False,
                        required=False,
                        help='If provided the server runs in the background.')
    parser.add_argument('--keyEvent',
                        action='store',
                        default=None,
                        required=False,
                        help='Keyboard event file. Default is autodetect')
    parser.add_argument('--mouseEvent',
                        action='store',
                        default=None,
                        required=False,
                        help='Mouse event file. Default is autodetect')
    parser.add_argument('--mapping',
                        action = 'store',
                        default="qwerty",
                        required=False,
                        help='If provided the server uses another keyboard mapping than qwerty. Possible mappings are: {}'.format(MAPPINGS))
    parser.add_argument('--security_token',
                        action = 'store',
                        default=None,
                        required=False,
                        help='Prevent execution of commands by clicking a link in a browser that POSTs a JSON-RPC to the server.')

    arguments = parser.parse_args()

    if arguments.keyEvent is None:
	    arguments.keyEvent = findKeyEvent()
    if arguments.mouseEvent is None:
	    arguments.mouseEvent = findMouseEvent()

    platform_rpcs = EvdevPlatformRpcs(config,
                                      arguments.mapping,
                                      arguments.keyEvent,
                                      arguments.mouseEvent,
                                      security_token=arguments.securityToken)

    if arguments.daemon:
        daemonize()

    server = AeneaServer.from_config(platform_rpcs, config)
    server.security_token=arguments.securityToken
    server.serve_forever()
