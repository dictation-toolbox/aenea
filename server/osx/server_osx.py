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

import logging
import os
import re
import sys
import time
from os.path import dirname, join, realpath

import applescript
import jsonrpclib
import jsonrpclib.SimpleJSONRPCServer
from Quartz.CoreGraphics import *

import config
# enable server.core imports by adding the root of the aenea project to path
sys.path.append(realpath(join(dirname(__file__), '../../')))
from server.core import AeneaPluginLoader


#logging.basicConfig(level=logging.DEBUG)

server_security_token = None


class PermissionDeniedError(Exception):
    pass


def _compare_security_token(expected, actual):
    if len(expected) != len(actual):
        return False
    result = 0
    for x, y in zip(expected, actual):
        result |= ord(x) ^ ord(y)
    return result == 0

def _check_security_token(expected_security_token, rpc_security_token):
    if expected_security_token is None:
        logging.warn('Server is configured to disable checking security tokens. You can use generate_security_token.py to generate a security token, which you should then add to config.py (client) and aenea.json (server). This message is intentionally spammy and annoying -- you need to fix this.')
        return

    if rpc_security_token is None:
        error_text = 'Client did not send a security token, but server has security token set. To fix, find the client\'s aenea.json and add security_token: "foo", to it, then restart Dragon. You will need to replace foo with the server\'s security token, which you can find in config.py. Or generate a new random one with generate_security_token.py and set it in both client and server.'
        logging.error(error_text)
        raise PermissionDeniedError(error_text)
    elif not _compare_security_token(expected_security_token, rpc_security_token):
        error_text = 'Client sent a security token, but it did not match the server\'s. The server\'s is specified in config.py. The client\'s is specified in aenea.json. Use generate_security_token.py to create a random token.'
        logging.error(error_text)
        raise PermissionDeniedError(error_text)
    else:
        pass


_MOUSE_BUTTONS = {
    'left': 1,
    'middle': 2,
    'right': 3,
    'wheelup': 4,
    'wheeldown': 5
    }


_MOUSE_CLICKS = {
    'click': 'click',
    'down': 'mousedown',
    'up': 'mouseup'
    }


_KEY_PRESSES = {
    'press': '',
    'up': 'up',
    'down': 'down'
    }


_MOUSE_MOVE_COMMANDS = {
    'absolute': 'mousemove',
    'relative': 'mousemove_relative',
    'relative_active': 'mousemove_active'
    }


_SERVER_INFO = {
    'window_manager': 'osx',
    'operating_system': 'darwin',
    'platform': 'darwin',
    'display': 'cocoa',
    'server': 'aenea_reference',
    'server_version': 1
    }


_MOD_TRANSLATION = {
    'alt': 'option',
    'shift': 'shift',
    'control': 'control',
    'super': 'command',
    'command': 'command'
    }

# The key maps are broken up into different sections because AppleScript has
# different semantics for keypress versus keycode

# these are used via keystroke, and need quoting.
_QUOTED_KEY_TRANSLATION = {
    'ampersand': '&',
    'apostrophe': "'",
    'asterisk': '*',
    'at': '@',
    'backslash': '\\\\',
    'backtick': '`',
    'bar': '|',
    'caret': '^',
    'colon': ':',
    'comma': ',',
    'dollar': '$',
    'dot': '.',
    'dquote': '\\\"',
    'equal': '=',
    'exclamation': '!',
    'hash': '#',
    'hyphen': '-',
    'langle': '<',
    'lbrace': '{',
    'lbracket': '[',
    'lparen': '(',
    'minus': '-',
    'percent': '%',
    'plus': '+',
    'question': '?',
    'rangle': '>',
    'rbrace': '}',
    'rbracket': ']',
    'rparen': ')',
    'semicolon': ';',
    'slash': '/',
    'space': ' ',
    'squote': "'",
    'tilde': '~',
    'underscore': '_'
    }

_MODIFIER_KEY_DIRECT = {
    # modifiers
    'win': 'command',
    'shift': 'shift',
    'alt': 'option',
    'ctrl': 'control',
    'rightshift': 'rightshift',
    'rightoption': 'rightoption',
    'rightcontrol': 'rightcontrol',
    'function': 'function'
    }

# from /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/ \
# HIToolbox.framework/Versions/A/Headers/Events.h
# these need applescript "key code " and a number to operate
_KEYCODE_TRANSLATION = {
    # 'apps': 'Menu', ???
    'a': 0,
    's': 1,
    'd': 2,
    'f': 3,
    'h': 4,
    'g': 5,
    'z': 6,
    'x': 7,
    'c': 8,
    'v': 9,
    'b': 11,
    'q': 12,
    'w': 13,
    'e': 14,
    'r': 15,
    'y': 16,
    't': 17,
    '1': 18,
    '2': 19,
    '3': 20,
    '4': 21,
    '6': 22,
    '5': 23,
    'equal': 24,
    '9': 25,
    '7': 26,
    'minus': 27,
    '8': 28,
    '0': 29,
    'rbracket': 30,  # rightbracket
    'o': 31,
    'u': 32,
    'lbracket': 33,  # leftbracket
    'i': 34,
    'p': 35,
    'enter': 36,  # return
    'l': 37,
    'j': 38,
    'quote': 39,
    'k': 40,
    'semicolon': 41,
    'backslash': 42,
    'comma': 43,
    'slash': 44,
    'n': 45,
    'm': 46,
    'period': 47,
    'tab': 48,
    'space': 49,
    'grave': 50,
    'backspace': 51,  # delete
    'escape': 53,
    'capslock': 57,

    'f17': 64,
    'npdecimal': 65,
    'npmultiply': 67,
    'npplus': 69,
    'npclear': 71,
    'volumeup': 72,
    'volumedown': 73,
    'mute': 74,
    'npdivide': 75,
    'npenter': 76,
    'npminus': 78,
    'f18': 79,
    'f19': 80,
    'keypadequals': 81,
    'np0': 82,  # np = numberpad
    'np1': 83,
    'np2': 84,
    'np3': 85,
    'np4': 86,
    'np5': 87,
    'np6': 88,
    'np7': 89,
    'f20': 90,
    'np8': 91,
    'np9': 92,
    'jis_yen': 93,
    'jis_underscore': 94,
    'jis_keypadcomma': 95,
    'f5': 96,
    'f6': 97,
    'f7': 98,
    'f3': 99,
    'f8': 100,
    'f9': 101,
    'jis_eisu': 102,
    'f11': 103,
    'jis_kana': 104,
    'f13': 105,
    'f16': 106,
    'f14': 107,
    'f10': 109,
    'f12': 111,
    'f15': 113,
    'help': 114,
    'home': 115,
    'pgup': 116,  # pageup
    'pageup': 116,  # pageup
    'del': 117,  # forwarddelete
    'f4': 118,
    'end': 119,
    'f2': 120,
    'pgdown': 121,  # pagedown
    'pagedown': 121,  # pagedown
    'f1': 122,
    'left': 123,  # leftarrow
    'right': 124,  # rightarrow
    'down': 125,  # downarrow
    'up': 126  # uparrow
    }


def write_command(message, arguments=' -f -', executable='???'):
    print 'echo \'%s\' | %s %s' % (message, executable, arguments)
    with os.popen('%s %s' % (executable, arguments), 'w') as fd:
        fd.write(message)


def get_active_window():
    '''Returns the window id and title of the active window.'''

    script = applescript.AppleScript('''
        global frontApp, frontAppName, windowTitle

        set windowTitle to ""
        tell application "System Events"
            set frontApp to first application process whose frontmost is true
            set frontAppName to name of frontApp
            tell process frontAppName
                set mainWindow to missing value
                repeat with win in windows
                    if attribute "AXMain" of win is true then
                        set mainWindow to win
                        exit repeat
                    end if
                end repeat
                if mainWindow is missing value then
                    tell application "System Events"
                        set windowTitle to name of first window of process frontAppName
                    end tell
                else
                    tell mainWindow
                        set windowTitle to value of attribute "AXMain"
                    end tell
                end if
            end tell
        end tell

        return {frontAppName, windowTitle}
    ''')
    # window_id isn't really a unique id, instead it's just the app name -- but
    # still useful for automating through applescript
    window_id, window_title = script.run()
    if window_id:
        return window_id.encode('utf-8'), window_title.encode('utf-8')
    else:
        return None, None


def map_window_properties(properties):
    p = {}
    for key in properties:
        short_key = re.match(r".*\('(.*)'\).*", str(key))  # is there a better
        # way to access keys that are instances?
        p[str(short_key.group(1))] = properties[key]
    return p


def get_geometry(window_id=None):
    p = get_window_properties(window_id)
    frame = {'x': p['posn'][0],
            'y': p['posn'][1],
            'width': p['ptsz'][0],
            'height': p['ptsz'][1]}

    return frame  # what to do about screen?


def get_window_properties(window_id=None):
    if window_id is None:
        window_id, _ = get_active_window()

    cmd = '''tell application "System Events" to tell application process "%s"
        try
            get properties of window 1
        on error errmess
            log errmess
        end try
    end tell
    ''' % window_id
    script = applescript.AppleScript(cmd)
    properties = script.run()

    p = map_window_properties(properties)
    return p

def transform_relative_mouse_event(event):
    geo = get_geometry()
    dx, dy = map(int, map(float, event.split()))
    return [('mousemove', '%i %i' % (geo['x'] + dx, geo['y'] + dy))]


def get_context(security_token=None):
    '''return a dictionary of window properties for the currently active
       window. it is fine to include platform specific information, but
       at least include title and executable.'''
    _check_security_token(server_security_token, security_token)

    window_id, window_title = get_active_window()
    properties = get_window_properties(window_id)
    properties['id'] = window_id
    properties['title'] = window_title

    # Types in 'breaking' throw an exception in jsonrpclib, so
    # they need to be converted to strings.
    breaking = [objc.pyobjc_unicode,         # NSString
                applescript.aecodecs.AEType] # AppleEvent

    for key in properties:
        for c in breaking:
            if isinstance(properties[key], c):
                if hasattr(properties[key], 'encode') and \
                   callable(getattr(properties[key], 'encode')):
                    # pyobjc_unicode aren't accepted by 'str()', but need
                    # to be converted or jsonrpclib balks.
                    properties[key] = properties[key].encode('utf-8')
                else:
                    # AEType doesn't respond to 'encode()'
                    properties[key] = str(properties[key])
                break

    logging.debug(properties)

    return properties


def key_press(
    key,
        modifiers=(),
        direction='press',
        count=1,
        count_delay=None,
        security_token=None
        ):
    '''press a key possibly modified by modifiers. direction may be
       'press', 'down', or 'up'. modifiers may contain 'alt', 'shift',
       'control', 'super'. this X11 server also supports 'hyper',
       'meta', and 'flag' (same as super). count is number of times to
       press it. count_delay delay in ms between presses.'''
    _check_security_token(server_security_token, security_token)

    logging.debug(("\nkey = {key} modifiers = {modifiers} " +
                  "direction = {direction} " +
                  "count = {count} count_delay = {count_delay} ").
                  format(modifiers=modifiers,
                         direction=direction,
                         count=count,
                         count_delay=count_delay,
                         key=key))

    if count_delay is None or count < 2:
        delay = ''
    else:
        delay = 'delay %0.2f ' % count_delay

    if modifiers and hasattr(modifiers, 'lower'):
        modifiers = [modifiers]

    modifiers = [_MOD_TRANSLATION.get(mod, mod) for mod in modifiers]
    logging.debug("modifiers = %s" % modifiers)

    key_to_press = _MODIFIER_KEY_DIRECT.get(key.lower(), None)
    if key_to_press:
        if direction == 'down' or direction == 'up':
            command = '{key_to_press} key {direction}'.format(
                key_to_press=key_to_press, direction=direction)

    if not key_to_press:
        key_to_press = _QUOTED_KEY_TRANSLATION.get(key.lower(), None)
        if key_to_press:
            command = 'keystroke "{0}"'.format(key_to_press)
        elif not key_to_press:
            key_to_press = _KEYCODE_TRANSLATION.get(key.lower(), None)
            command = 'key code "{0}"'.format(key_to_press)

    if modifiers:
        elems = map(lambda s: "%s down" % s, modifiers)
        key_command = "%s using {%s} " % (command, ', '.join(elems))
    else:
        key_command = command

    script = applescript.AppleScript('''
    tell application "System Events"
        try
            repeat {count} times
                {key_command}
                {delay}
            end repeat
        on error
            key up {{control, shift, option, command}}
        end try
    end tell
    '''.format(key_command=key_command, count=count, delay=delay))

    script.run()


def write_text(text, paste=False, security_token=None):
    '''send text formatted exactly as written to active window.  will use
       simulate keypress typing for maximum compatibility.'''
    _check_security_token(server_security_token, security_token)

    logging.debug("text = %s" % (text))
    if text:
        script = applescript.AppleScript('''
        tell application "System Events"
          repeat with i from 1 to count characters of "{text}"
            keystroke (character i of "{text}")
            delay 0.0002
          end repeat
        end tell
        '''.format(text=text))
        script.run()


def mouseEvent(type, posx, posy, clickCount=1):
    theEvent = CGEventCreateMouseEvent(
        None, type, (posx, posy), kCGMouseButtonLeft)
    CGEventSetIntegerValueField(theEvent, kCGMouseEventClickState, clickCount)
    CGEventPost(kCGHIDEventTap, theEvent)
    CGEventSetType(theEvent, type)


def mousemove(posx, posy):
    mouseEvent(kCGEventMouseMoved, posx, posy)


def trigger_mouseclick(button, direction, posx, posy, clickCount=1):
    # button: number 1-5, direction {click, up, down}
    click_mapping = {
        1: [kCGEventLeftMouseDown, kCGEventLeftMouseUp],
        2: [kCGEventOtherMouseDown, kCGEventOtherMouseUp],
        3: [kCGEventRightMouseDown, kCGEventRightMouseUp]
        }

    if button == 4 or button == 5:
        yScroll = -10 if button == 5 else 10  # wheeldown -, wheelup +
        theEvent = CGEventCreateScrollWheelEvent(
            None, kCGScrollEventUnitLine, 1, yScroll)

        for _ in xrange(clickCount):
            CGEventPost(kCGHIDEventTap, theEvent)
    elif direction == 'click':
        theEvent = CGEventCreateMouseEvent(
            None, click_mapping[button][0], (posx, posy), kCGMouseButtonLeft)
        for _ in xrange(clickCount):
            CGEventSetType(theEvent, click_mapping[button][0])
            CGEventSetIntegerValueField(
                theEvent, kCGMouseEventClickState, clickCount)
            CGEventPost(kCGHIDEventTap, theEvent)
            CGEventSetType(theEvent, click_mapping[button][1])
            CGEventPost(kCGHIDEventTap, theEvent)
    # else: # up or down


def click_mouse(
        button,
        direction='click',
        count=1,
        count_delay=None,
        security_token=None
        ):
    '''click the mouse button specified. button maybe one of 'right',
       'left', 'middle', 'wheeldown', 'wheelup'.'''
    _check_security_token(server_security_token, security_token)

    logging.debug("button = "+button)
    if count_delay is None or count < 2:
        delay = 0
    else:
        delay = count_delay

    try:
        button = _MOUSE_BUTTONS[button]
    except KeyError:
        button = int(button)

    logging.debug('_MOUSE_CLICKS[direction]' + _MOUSE_CLICKS[direction])

    ourEvent = CGEventCreate(None)
    currentpos = CGEventGetLocation(ourEvent)  # Save current mouse position
    trigger_mouseclick(
        button, _MOUSE_CLICKS[direction],
        int(currentpos.x), int(currentpos.y), count)


def move_mouse(
        x,
        y,
        reference='absolute',
        proportional=False,
        phantom=None,
        security_token=None
        ):
    '''move the mouse to the specified coordinates. reference may be one
    of 'absolute', 'relative', or 'relative_active'. if phantom is not
    None, it is a button as click_mouse. If possible, click that
    location without moving the mouse. If not, the server will move the
    mouse there and click.'''
    _check_security_token(server_security_token, security_token)

    geo = get_geometry()
    if proportional:
        x = geo['width'] * x
        y = geo['height'] * y

    mousemove(x, y)

    if phantom is not None:
        trigger_mouseclick(1, 'click', x, y, 1)


def pause(amount, security_token=None):
    '''pause amount in ms.'''
    _check_security_token(server_security_token, security_token)
    time.sleep(amount / 1000.)


def server_info(security_token=None):
    _check_security_token(server_security_token, security_token)
    return _SERVER_INFO


def list_rpc_commands():
    _RPC_COMMANDS = {
        'get_context': get_context,
        'key_press': key_press,
        'write_text': write_text,
        'click_mouse': click_mouse,
        'move_mouse': move_mouse,
        'server_info': server_info,
        'pause': pause,
        }
    return _RPC_COMMANDS


def multiple_actions(actions, security_token=None):
    '''execute multiple rpc commands, aborting on any error. will not
       return anything ever. actions is an array of objects, possessing
       'method', 'params', and 'optional' keys. See also JSON-RPC
       multicall.  Guaranteed to execute in specified order.'''
    _check_security_token(server_security_token, security_token)

    for (method, parameters, optional) in actions:
        commands = list_rpc_commands()
        if method in commands:
            commands[method](*parameters, **optional)
        else:
            break


def setup_server(host, port, security_token):
    print "started on host = %s port = %s " % (host, port)
    if security_token is None:
        print "A security token is not in use. This allows any link you click in a web browser to execute arbitrary commands."
    server = jsonrpclib.SimpleJSONRPCServer.SimpleJSONRPCServer((host, port))

    for command in list_rpc_commands():
        logging.debug("registered %s", command)
        server.register_function(globals()[command])
    server.register_function(multiple_actions)

    plugins = AeneaPluginLoader(logging.getLogger()).get_plugins(
        getattr(config, 'PLUGIN_PATH', None))
    for plugin in plugins:
        plugin.register_rpcs(server)

    return server


if __name__ == '__main__':
    global server_security_token
    server_security_token = getattr(config, 'SECURITY_TOKEN', None)
    if len(sys.argv) == 2 and sys.argv[-1] == 'getcontext':
        ctx = get_context(security_token=server_security_token)
        try:
            import pprint
            pprint.pprint(ctx)
        except ImportError:
            print ctx
    else:
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
    server = setup_server(config.HOST, config.PORT, server_security_token)
    server.serve_forever()
