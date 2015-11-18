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
import subprocess
import sys
import time
import array
from warnings import warn

import jsonrpclib
import jsonrpclib.SimpleJSONRPCServer

import xdo
import Xlib.display
import psutil

display = Xlib.display.Display()
libxdo = xdo.Xdo()

import config
if not hasattr(config, 'XDOTOOL_DELAY'):
    setattr(config, 'XDOTOOL_DELAY', 0)

import logging.config
import server_logging

try:
    logging_config = server_logging.make_logging_config(config)
    logging.config.dictConfig(logging_config)
except Exception, e:
    print 'Failed to initialize logging support. Error: %s' % e

import logging
logger = logging.getLogger('server')

try:
    import yapsy
    import yapsy.PluginManager
except ImportError, e:
    if hasattr(config, 'PLUGIN_PATH') and config.PLUGIN_PATH is not None:
        logger.warn(
            'Cannot import yapsy; the optional server plugin support won\'t '
            'work. You don\'t need this unless you want to use plugins, which '
            'are not necessary for basic operation. To squelch this message, '
            'don\'t set a PLUGIN_PATH in config.py.'
        )
        yapsy = None

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
    'operating_system': 'linux',
    'platform': 'linux',
    'display': 'X11',
    'server': 'aenea_reference',
    'server_version': 1
    }


# Additional X properties that we'd like to expose via get_context()
_X_PROPERTIES = {
    '_NET_WM_DESKTOP': 'desktop',
    'WM_WINDOW_ROLE': 'role',
    '_NET_WM_WINDOW_TYPE': 'type',
    '_NET_WM_PID': 'pid',
    'WM_LOCALE_NAME': 'locale',
    'WM_CLIENT_MACHINE': 'client_machine',
    'WM_NAME': 'name'
}


# compute {atom_name: atom_value} map once to save us from having to query X
# for this data repeatedly.
_X_ATOMS = {name: display.intern_atom(name) for name in _X_PROPERTIES}


_MOD_TRANSLATION = {
    'alt': 'Alt_L',
    'shift': 'Shift_L',
    'control': 'Control_L',
    'super': 'Super_L',
    'hyper': 'Hyper_L',
    'meta': 'Meta_L',
    'win': 'Super_L',
    'flag': 'Super_L',
    }


_KEY_TRANSLATION = {
    'ampersand': 'ampersand',
    'apostrophe': 'apostrophe',
    'apps': 'Menu',
    'asterisk': 'asterisk',
    'at': 'at',
    'backslash': 'backslash',
    'backspace': 'BackSpace',
    'backtick': 'grave',
    'bar': 'bar',
    'caret': 'asciicircum',
    'colon': 'colon',
    'comma': 'comma',
    'del': 'Delete',
    'delete': 'Delete',
    'dollar': 'dollar',
    'dot': 'period',
    'dquote': 'quotedbl',
    'enter': 'Return',
    'equal': 'equal',
    'exclamation': 'exclam',
    'hash': 'numbersign',
    'hyphen': 'minus',
    'langle': 'less',
    'lbrace': 'braceleft',
    'lbracket': 'bracketleft',
    'lparen': 'parenleft',
    'minus': 'minus',
    'npadd': 'KP_Add',
    'npdec': 'KP_Decimal',
    'npdiv': 'KP_Divide',
    'npmul': 'KP_Multiply',
    'percent': 'percent',
    'pgdown': 'Next',
    'pgup': 'Prior',
    'plus': 'plus',
    'question': 'question',
    'rangle': 'greater',
    'rbrace': 'braceright',
    'rbracket': 'bracketright',
    'rparen': 'parenright',
    'semicolon': 'semicolon',
    'shift': 'Shift_L',
    'slash': 'slash',
    'space': 'space',
    'squote': 'apostrophe',
    'tilde': 'asciitilde',
    'underscore': 'underscore',
    'win': 'Super_L',
    }


def update_key_translation(translation):
    caps_keys = [
        'left',
        'right',
        'up',
        'down',
        'home',
        'end',
        'tab',
        'insert',
        'escape'
        ]
    caps_keys.extend('f%i' % i for i in xrange(1, 13))
    for key in caps_keys:
        translation[key] = key[0].upper() + key[1:]
    for index in xrange(10):
        translation['np%i' % index] = 'KP_%i' % index
    for c in range(ord('a'), ord('z')) + range(ord('0'), ord('9')):
        translation[chr(c)] = chr(c)
        translation[chr(c).upper()] = chr(c).upper()
update_key_translation(_KEY_TRANSLATION)


def run_command(command, executable='xdotool'):
    command_string = '%s %s' % (executable, command)
    logger.debug(command_string)
    os.system(command_string)


def read_command(command, executable='xdotool'):
    logger.debug('%s %s | <server>' % (executable, command))
    with os.popen('%s %s' % (executable, command), 'r') as fd:
        rval = fd.read()
    return rval


def write_command(message, arguments='type --file -', executable='xdotool'):
    logger.debug('echo \'%s\' | %s %s' % (message, executable, arguments))
    with os.popen('%s %s' % (executable, arguments), 'w') as fd:
        fd.write(message)


def get_active_window(_xdotool=None):
    '''Returns the window id and title of the active window.'''

    flush_xdotool(_xdotool)
    window_id = read_command('getactivewindow')
    if window_id:
        window_id = int(window_id)
        window_title = read_command('getwindowname %i' % window_id).strip()
        return window_id, window_title
    else:
        return None, None


def get_geometry(window_id=None):
    """
    Get locations and size information of a window
    :param int window_id:
    :return: Window location and size info. Example:
      {
        'y': 154,
        'x': 81,
        'screen': None,
        'width': 1116,
        'height': 458
      }
    :rtype: dict
    """
    if window_id is None:
        window_id = libxdo.get_focused_window_sane()
    window_location = libxdo.get_window_location(window_id)
    window_size = libxdo.get_window_size(window_id)
    return {
        'x': int(window_location.x),
        'y': int(window_location.y),
        'screen': window_location.screen.display,
        'height': int(window_size.height),
        'width': int(window_size.width),
    }


def transform_relative_mouse_event(event):
    geo = get_geometry()
    dx, dy = map(int, map(float, event.split()))
    return [('mousemove', '%i %i' % (geo['x'] + dx, geo['y'] + dy))]


def get_context():
    """
    Query the system for context information.  This data is typically passed
    back to the aenea client so that it may use it in Dragonfly grammars.
    Specifically, this data will be used when Dragonfly's grammars perform
    context matching to decide which grammars should be activated.
    :return: various properties related to the current active window
      All possible return keys are demonstrated below:

      {
          'id': 1234,             # window id
          'cls_name': 'foo',      # WM_CLASS name
          'cls': 'bar',           # WM_CLASS
          'title': 'baz',         # WM_NAME
          'client_machine': 'me', # WM_CLIENT_MACHINE
          'name': 'baz',          # WM_NAME,
          'locale': 'x',          # WM_LOCALE_NAME
          'pid': 123,             # _NET_WM_PID
          'type': '543',          # x11 atom for _NET_WM_WINDOW_TYPE
          'role': '...',          # WM_WINDOW_ROLE
          'desktop': '0',         # _NET_WM_DESKTOP
      }
    :rtype dict
    """
    try:
        window_id = libxdo.get_focused_window_sane()
        window = display.create_resource_object('window', window_id)
    except Exception as error:
        logger.error('failed to get active window error=%s', error)
        return {}

    properties = {'id': window_id}

    window_class = window.get_wm_class()
    if window_class is not None:
        properties['cls_name'] = window_class[1]
        properties['cls'] = window_class[0]

    window_title = window.get_wm_name()
    if window_title is not None:
        properties['title'] = window_title

    # get additional window properties via xlib.  if the window does not have
    # the property then omit it from <properties>.
    for atom_name, atom in _X_ATOMS.items():
        queried_property = window.get_full_property(atom, 0)
        if queried_property is not None:
            value = queried_property.value
            if type(value) == array.array:
                value = value.tolist()
                if not len(value):
                    continue
                value = value[0]
            properties[_X_PROPERTIES[atom_name]] = str(value)

    # get process related context info.  if we cannot get this information
    # then omit it from <properties>.
    try:
        pid = libxdo.get_pid_window(window_id)
        properties['pid'] = pid
        process = psutil.Process(pid)

        try:
            properties['executable'] = process.exe()
        except (psutil.NoSuchProcess, psutil.AccessDenied) as e:
            pass
        try:
            properties['cmdline'] = process.cmdline()
        except (psutil.NoSuchProcess, psutil.AccessDenied) as e:
            pass
    except Exception as e:
        pass

    return properties


def key_press(key=None, modifiers=(), direction='press', count=1,
              count_delay=None, _xdotool=None):
    """
    Press a key possibly modified by modifiers. direction may be 'press',
    'down', or 'up'. modifiers may contain 'alt', 'shift', 'control', 'super'.
    This X11 server also supports 'hyper', 'meta', and 'flag' (same as super).
    Count is number of times to press it. count_delay delay in ms between
    presses.
    :param str key: Key to press. Valid values: _KEY_TRANSLATION.keys()
    :param modifiers: Key modifiers. Valid values: _MOD_TRANSLATION.keys()
    :type modifiers: list of str
    :param str direction: Direction of key press.  Valid values:
      _KEY_PRESSES.keys()
    :param int count: Number of times to perform this key press.
    :param int count_delay: Delay between repeated keystrokes in milliseconds.
    :param _xdotool: Deprecated.  Here for backwards compatibility only!
    :return: This function always returns None
    """
    assert key is not None

    if _xdotool is not None:
        warn('_xdotool parameter deprecated', DeprecationWarning)

    delay_millis = 0 if count_delay is None or count == 1 else count_delay
    delay_micros = delay_millis * 1000
    modifiers = [_MOD_TRANSLATION.get(mod, mod) for mod in modifiers]
    key = _KEY_TRANSLATION.get(key, key)

    # TODO: We can distill this entire loop down to a single libxdo function
    # call when we figure out how to properly user charcode_t entities from
    # libxdo.
    for _ in range(0, count):
        # modifiers down
        libxdo.send_keysequence_window_down(
            0, '+'.join(modifiers), delay_micros)

        if direction == 'press':
            libxdo.send_keysequence_window(0, key, delay_micros)
        elif direction == 'up':
            libxdo.send_keysequence_window_up(0, key, delay_micros)
        elif direction == 'down':
            libxdo.send_keysequence_window_down(0, key, delay_micros)

        # modifiers up
        libxdo.send_keysequence_window_up(
            0, '+'.join(reversed(modifiers)), delay_micros)

        time.sleep(delay_millis / 1000)  # emulate xdotool sleep


def write_text(text, paste=False, _xdotool=None):
    """
    Send text formatted exactly as written to active window. If paste
    is True, will use X11 PRIMARY clipboard to paste the text instead
    of typing it. See config.ENABLE_XSEL documentation for more
    information on this.
    :param str text: Text to send to the current active window.
    :param bool paste: If True, text will be written to the current window
     using xsel and a middle click.
    :param _xdotool: Deprecated.
    :return: This function always returns None
    """

    # Workaround for https://github.com/jordansissel/xdotool/pull/29
    if text:
        if paste and config.ENABLE_XSEL:
            # swap primary and secondary X11 clipboards so we can
            # restore after paste
            run_command('-x', executable='xsel')

            # copy the pasted text to the clipboard
            write_command(text, arguments='-i', executable='xsel')

            # paste by simulating midde click
            # TODO: can we do this even in programs that don't have a
            #     middle click?
            #     if not, we may need a blacklist of buggy programs.
            click_mouse(2, _xdotool=_xdotool)

            # nuke the text we selected
            run_command('-c', executable='xsel')

            # restore the previous clipboard contents
            run_command('-x', executable='xsel')
        else:
            libxdo.enter_text_window(0, text, config.XDOTOOL_DELAY*1000)


def click_mouse(button, direction='click', count=1, count_delay=None,
                _xdotool=None):
    """
    Click the mouse button specified at the current location.
    :param button: Mouse button to click. One of _MOUSE_BUTTONS.keys()
      or an int corresponding to a libxdo mouse button code.
    :type button: str or int
    :param str direction: Direction of 'up', 'down', 'click'  One of
      _MOUSE_CLICKS.keys().
    :param int count: Number of times to repeat this click.
    :param int count_delay: Delay (in milliseconds) between mouse clicks.
    :param _xdotool: Deprecated.
    :return: This function always returns None
    """
    if _xdotool is not None:
        warn('_xdotool parameter deprecated', DeprecationWarning)

    delay_millis = 0 if count_delay is None or count < 2 else count_delay

    if button in _MOUSE_BUTTONS:
        button = _MOUSE_BUTTONS[button]
    else:
        try:
            button = int(button)
        except ValueError:
            raise ValueError('invalid "button" parameter: "%s"' % button)

    for _ in range(0, count):
        if direction == 'click':
            libxdo.click_window(0, button)
        elif direction == 'down':
            libxdo.mouse_down(0, button)
        elif direction == 'up':
            libxdo.mouse_up(0, direction)
        else:
            raise ValueError('invalid "direction" parameter: "%s"' % direction)
        time.sleep(delay_millis / 1000)


def move_mouse(x, y, reference='absolute', proportional=False, phantom=None,
               _xdotool=None):
    """
    Move the mouse to the specified coordinates. reference may be one
    of 'absolute', 'relative', or 'relative_active'. if phantom is not
    None, it is a button as click_mouse. If possible, click that
    location without moving the mouse. If not, the server will move the
    mouse there and click. Currently, phantom only works with absolute
    moves. Negative coordinates are allowed for all references; in the
    case of absolute they will be clamped to 0.
    :param x:
    :param y:
    :param reference:
    :param proportional:
    :param phantom:
    :param _xdotool:
    :return:
    """
    geo = get_geometry()
    if proportional:
        x = geo['width'] * x
        y = geo['height'] * y
    command = _MOUSE_MOVE_COMMANDS[reference]
    if command == 'mousemove_active':
        command = 'mousemove --window %i' % get_active_window()[0]

    if x <= 0 or y <= 0:
        commands = ['%s -- %f %f' % (command, x, y)]
    else:
        commands = ['%s %f %f' % (command, x, y)]
    if phantom is not None:
        commands.append('click %s' % _MOUSE_BUTTONS[phantom])
        commands.append('mousemove restore')

    # To avoid headaches down the road with argparse, we don't chain commands
    # if we need to use -- since it would block future flags from being
    # interpreted.
    if _xdotool is not None and x >= 0 and y >= 0:
        _xdotool.extend(commands)
    else:
        flush_xdotool(_xdotool)
        run_command(' '.join(commands))


def pause(amount, _xdotool=None):
    '''pause amount in ms.'''
    if _xdotool is not None:
        _xdotool.append('sleep %f' % (amount / 1000.))
    else:
        time.sleep(amount / 1000.)


def notify(message):
    '''Send a message to the notification daemon via notify-send.'''
    try:
        subprocess.Popen(['notify-send', message])
    except Exception as e:
        logger.warn('failed to start notify-send process: %s' % e)


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
        'notify' : notify
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
            # JSON-RPC forbids specifying both optional and parameters.
            # Since multiple_actions is trying to mimic something like
            # Multicall except with sequential ordering and abort,
            # we enforce it here.
            assert not (parameters and optional)

            commands[method](*parameters, _xdotool=xdotool, **optional)
        else:
            break
    flush_xdotool(xdotool)


def setup_server(host, port):
    server = jsonrpclib.SimpleJSONRPCServer.SimpleJSONRPCServer((host, port))

    for command in list_rpc_commands():
        server.register_function(globals()[command])
    server.register_function(multiple_actions)

    if (yapsy is not None and
       hasattr(config, 'PLUGIN_PATH') and
       config.PLUGIN_PATH is not None):
        plugin_manager = yapsy.PluginManager.PluginManager()
        plugin_manager.setPluginPlaces(config.PLUGIN_PATH)
        plugin_manager.collectPlugins()
        for plugin_info in plugin_manager.getAllPlugins():
            logger.info('Loading plugin "%s"' % plugin_info.name)
            plugin_manager.activatePluginByName(plugin_info.name)
            plugin_info.plugin_object.register_rpcs(server)

    return server


if __name__ == '__main__':
    if len(sys.argv) == 2 and sys.argv[-1] == 'getcontext':
        ctx = get_context()
        try:
            import pprint
            pprint.pprint(ctx)
        except ImportError, e:
            logger.error('failed to pretty print context. error:%s' % e)

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
