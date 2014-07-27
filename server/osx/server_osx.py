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
import time

import jsonrpclib
import jsonrpclib.SimpleJSONRPCServer

import config

# I initially thought to use cliclick (https://github.com/BlueM/cliclick) but found it didn't support the full range of function that xdotool does.  So to match xdotool, I'm using a combination of the python-applescript module as well as some of lower-level cocoa api's.
import applescript
# http://apple.stackexchange.com/questions/36943/how-do-i-automate-a-key-press-in-applescript
# http://en.wikibooks.org/wiki/AppleScript_Programming/System_Events
# https://discussions.apple.com/message/17493621   # working mouse move and click
# http://apple.stackexchange.com/questions/74523/position-windows-via-command-line


# set {width, height, scale} to words of (do shell script "system_profiler SPDisplaysDataType | awk '/Built-In: Yes/{found=1} /Resolution/{width=$2; height=$4} /Retina/{scale=($2 == \"Yes\" ? 2 : 1)} /^ {8}[^ ]+/{if(found) {exit}; scale=1} END{printf \"%d %d %d\\n\", width, height, scale}'")


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
    'window_manager': 'awesome',
    'operating_system': 'darwin',
    'platform': 'darwin',
    'display': 'cocoa',
    'server': 'aenea_reference',
    'server_version': 1
    }


_XPROP_PROPERTIES = {
    '_NET_WM_DESKTOP(CARDINAL)': 'desktop',
    'WM_WINDOW_ROLE(STRING)': 'role',
    '_NET_WM_WINDOW_TYPE(ATOM)': 'type',
    '_NET_WM_PID(CARDINAL)': 'pid',
    'WM_LOCALE_NAME(STRING)': 'locale',
    'WM_CLIENT_MACHINE(STRING)': 'client_machine',
    'WM_NAME(STRING)': 'name'
    }


_MOD_TRANSLATION = {
    'alt': 'alt',
    'shift': 'shift',
    'control': 'control',
    'super': 'command',
    'hyper': 'command', # not sure how to map these others.  probably not important
    'meta': 'command',
    'win': 'command',
    'flag': 'command'
    }

# The key maps are broken up into different sections because AppleScript has different semantics for keypress versus keycode

# these are used via keystroke, and need quoting.
_QUOTED_KEY_TRANSLATION =  {
    'ampersand': '&',
    'apostrophe': "'",
    'asterisk': '*',
    'at': '@',
    'backslash': '\\',
    'backtick': '`',
    'bar': '-',
    'caret': '^',
    'colon': ':',
    'comma': ',',
    'dollar': '$',
    'dot': '.',
    'dquote': '"',
    'equal': '=',
    'exclamation': '!',
    'hash': '#',
    'hyphen': '-',
    'langle': '<',
    'lbrace': '[',
    'lbracket': '{',
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
    'command': 'command',
    'shift': 'shift',
    'option': 'option',
    'control': 'control',
    'rightshift': 'rightshift',
    'rightoption': 'rightoption',
    'rightcontrol': 'rightcontrol',
    'function': 'function'
    }

# from /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h
# these need applescript "key code " and a number to operate
_KEYCODE_TRANSLATION = {
    # 'apps': 'Menu', ???
    # 'win': 'Super_L',
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
    # 'ISO_Section': 10,
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
    'rbracket': 30, # rightbracket
    'o': 31,
    'u': 32,
    'lbracket': 33, # leftbracket
    'i': 34,
    'p': 35,
    'enter': 36, # return
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
    'backspace': 51, # delete
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
    'np0': 82, #  np = numberpad
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
    'pgup': 116, # pageup
    'del': 117, # forwarddelete
    'f4': 118,
    'end': 119,
    'f2': 120,
    'pgdown': 121, # pagedown
    'f1': 122,
    'left': 123, # leftarrow
    'right': 124, # rightarrow
    'down': 125, # downarrow
    'up': 126 # uparrow
    }


def run_command(command, executable='???'):
    command_string = '%s %s' % (executable, command)
    print command_string
    os.system(command_string)


def read_command(command, executable='???'):
    print '%s %s | <server>' % (executable, command)
    with os.popen('%s %s' % (executable, command), 'r') as fd:
        rval = fd.read()
    return rval


def write_command(message, arguments=' -f -', executable='???'):
    print 'echo \'%s\' | %s %s' % (message, executable, arguments)
    with os.popen('%s %s' % (executable, arguments), 'w') as fd:
        fd.write(message)


def get_active_window(_xdotool=None):
    '''Returns the window id and title of the active window.'''
# http://stackoverflow.com/questions/5292204/macosx-get-foremost-window-title
    flush_xdotool(_xdotool)
    window_id = read_command('getactivewindow')
    if window_id:
        window_id = int(window_id)
        window_title = read_command('getwindowname %i' % window_id).strip()
        return window_id, window_title
    else:
        return None, None

#http://stackoverflow.com/questions/3039717/how-to-get-another-application-windows-title-position-and-size-in-mac-os-witho
# https://developer.apple.com/library/mac/samplecode/SonOfGrab/Introduction/Intro.html
# http://stackoverflow.com/questions/6164164/resizing-windows-of-unscriptable-applications-in-applescript  # get the window id?
# https://developer.apple.com/library/mac/documentation/Accessibility/Conceptual/AccessibilityMacOSX/OSXAXModel/OSXAXmodel.html
# http://stackoverflow.com/questions/6836278/api-for-accessing-ui-elements-in-mac-os-x
# http://stackoverflow.com/questions/21069066/move-other-windows-on-mac-os-x-using-accessibility-api

def get_geometry(window_id=None, _xdotool=None):
    flush_xdotool(_xdotool)
    if window_id is None:
        window_id, _ = get_active_window()
    geometry = read_command('getwindowgeometry --shell %i' % window_id)
    geometry = geometry.strip().split('\n')
    geo = dict([val.lower()
               for val in line.split('=')]
               for line in geometry)
    geo = dict((key, int(value)) for (key, value) in geo.iteritems())
    relevant_keys = 'x', 'y', 'width', 'height', 'screen'
    return dict((key, geo[key]) for key in relevant_keys)


def transform_relative_mouse_event(event):
    geo = get_geometry()
    dx, dy = map(int, map(float, event.split()))
    return [('mousemove', '%i %i' % (geo['x'] + dx, geo['y'] + dy))]


def get_context(_xdotool=None):
    '''return a dictionary of window properties for the currently active
       window. it is fine to include platform specific information, but
       at least include title and executable.'''

    flush_xdotool(_xdotool)
    window_id, window_title = get_active_window()
    if window_id is None:
        return {}

    properties = {
        'id': window_id,
        'title': window_title,
        }
    for line in read_command('-id %s' % window_id, 'xprop').split('\n'):
        split = line.split(' = ', 1)
        if len(split) == 2:
            rawkey, value = split
            if split[0] in _XPROP_PROPERTIES:
                property_value = value[1:-1] if '(STRING)' in rawkey else value
                properties[_XPROP_PROPERTIES[rawkey]] = property_value
            elif rawkey == 'WM_CLASS(STRING)':
                window_class_name, window_class = value.split('", "')
                properties['cls_name'] = window_class_name[1:]
                properties['cls'] = window_class[:-1]

    # Sigh...
    properties['executable'] = None
    try:
        proc_command = '/proc/%s/exe' % properties['pid']
        properties['executable'] = os.readlink(proc_command)
    except OSError:
        ps = read_command('%s' % properties['pid'], executable='ps')
        ps = ps.split('\n')[1:]
        if ps:
            try:
                properties['executable'] = ps[0].split()[4]
            except Exception:
                pass

    return properties

def key_press(
    key,
        modifiers=(),
        direction='press',
        count=1,
        count_delay=None,
        _xdotool=None
        ):
    '''press a key possibly modified by modifiers. direction may be
       'press', 'down', or 'up'. modifiers may contain 'alt', 'shift',
       'control', 'super'. this X11 server also supports 'hyper',
       'meta', and 'flag' (same as super). count is number of times to
       press it. count_delay delay in ms between presses.'''

    print "\nkey = {key} modifiers = {modifiers} direction = {direction} count = {count} count_delay = {count_delay} ".format(modifiers=modifiers, direction = direction, count=count, count_delay = count_delay, key=key)
    
    if count_delay is None or count < 2:
        delay = ''
    else:
        delay = 'delay %i ' % (count_delay / 1000.0)

    if modifiers and hasattr(modifiers, 'lower'):
        modifiers = [modifiers]

    modifiers = [_MOD_TRANSLATION.get(mod, mod) for mod in modifiers]
    print "modifiers = %s" % modifiers

    key_to_press = _MODIFIER_KEY_DIRECT.get(key.lower(), None)
    if key_to_press:
        if direction == 'down' or direction == 'up':
            command = '{key_to_press} key {direction}'.format(key_to_press=key_to_press, direction=direction)

    if not key_to_press:
        key_to_press = _QUOTED_KEY_TRANSLATION.get(key.lower(), None)
        if key_to_press:
            command = 'keystroke "{0}"'.format(key_to_press)
        elif not key_to_press:
            key_to_press = _KEYCODE_TRANSLATION.get(key.lower(), None)
            command = 'key code "{0}"'.format(key_to_press)


    if key_to_press == None:
        raise RuntimeError("Don't know how to handle keystroke {0}".format(key))

    if modifiers:
        elems = map(lambda s: "%s down" % s, modifiers)
        key_command = "%s using {%s} " % (command, ', '.join(elems))
    else:
        key_command = command

    print "XXX command = "+command+'  key_command = '+key_command

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

    # if _xdotool is not None:
    #     _xdotool.extend(keys)
    # else:
    #     run_command(delay + ' '.join(keys))


def write_text(text, paste=False, _xdotool=None):
    '''send text formatted exactly as written to active window.  will use pbpaste clipboard to paste the text instead
       of typing it.'''
    # TODO: use pbcopy and pbpaste?
    print "text = %s paste = %s" % (text, paste)
    if text:
        # copy the pasted text to the clipboard
        write_command(text, arguments='', executable='pbcopy')
        # paste
        key_press('v', 'super')


def click_mouse(
        button,
        direction='click',
        count=1,
        count_delay=None,
        _xdotool=None
        ):

    '''click the mouse button specified. button maybe one of 'right',
       'left', 'middle', 'wheeldown', 'wheelup'. This X11 server will
       also accept a number.'''

    if count_delay is None or count < 2:
        delay = ''
    else:
        delay = '--delay %i ' % count_delay

    repeat = '' if count == 1 else '--repeat %i' % count

    try:
        button = _MOUSE_BUTTONS[button]
    except KeyError:
        button = int(button)

    command = ('%s %s %s %s' %
              (_MOUSE_CLICKS[direction], delay, repeat, button))

    if _xdotool is not None:
        _xdotool.append(command)
    else:
        run_command(command)


def move_mouse(
        x,
        y,
        reference='absolute',
        proportional=False,
        phantom=None,
        _xdotool=None
        ):
    '''move the mouse to the specified coordinates. reference may be one
    of 'absolute', 'relative', or 'relative_active'. if phantom is not
    None, it is a button as click_mouse. If possible, click that
    location without moving the mouse. If not, the server will move the
    mouse there and click.'''

    geo = get_geometry()
    if proportional:
        x = geo['width'] * x
        y = geo['height'] * y
    command = _MOUSE_MOVE_COMMANDS[reference]
    if command == 'mousemove_active':
        command = 'mousemove --window %i' % get_active_window()[0]
    commands = ['%s %f %f' % (command, x, y)]
    if phantom is not None:
        commands.append('click %s' % _MOUSE_BUTTONS[phantom])
        commands.append('mousemove restore')
    if _xdotool is not None:
        _xdotool.extend(commands)
    else:
        run_command(' '.join(commands))


def pause(amount, _xdotool=None):
    '''pause amount in ms.'''
    if _xdotool is not None:
        _xdotool.append('sleep %f' % (amount / 1000.))
    else:
        time.sleep(amount / 1000.)


def server_info(_xdotool=None):
    flush_xdotool(_xdotool)
    return _SERVER_INFO


def flush_xdotool(actions):
    if actions:
        run_command(' '.join(actions))
        del actions[:]


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


def multiple_actions(actions):
    '''execute multiple rpc commands, aborting on any error. will not
       return anything ever. actions is an array of objects, possessing
       'method', 'params', and 'optional' keys. See also JSON-RPC
       multicall.  Guaranteed to execute in specified order.'''

    xdotool = []
    for (method, parameters, optional) in actions:
        commands = list_rpc_commands()
        if method in commands:
            commands[method](*parameters, _xdotool=xdotool, **optional)
        else:
            break
    flush_xdotool(xdotool)


def setup_server(host, port):
    server = jsonrpclib.SimpleJSONRPCServer.SimpleJSONRPCServer((host, port))

    for command in list_rpc_commands():
        server.register_function(globals()[command])
    server.register_function(multiple_actions)

    return server


if __name__ == '__main__':
    if len(sys.argv) == 2 and sys.argv[-1] == 'getcontext':
        ctx = get_context()
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
    server = setup_server(config.HOST, config.PORT)
    server.serve_forever()
