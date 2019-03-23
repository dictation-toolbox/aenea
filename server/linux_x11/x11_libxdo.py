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

import subprocess
import time
import array
import xdo
import Xlib.display
import psutil

from server.core import AbstractAeneaPlatformRpcs

_MOUSE_BUTTONS = {
    'left': 1,
    'middle': 2,
    'right': 3,
    'wheelup': 4,
    'wheeldown': 5
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
    'equals': 'equal',
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


class XdoPlatformRpcs(AbstractAeneaPlatformRpcs):
    """
    Aenea RPC implementation that uses low level C bindings to the xdo library.
    """
    def __init__(self, xdo_delay=0, display=None, security_token=None, **kwargs):
        """
        :param int xdo_delay: Default pause between keystrokes.
        :param str display: reserved for future use.
        :param security_token: Static token that must be the same between
         client and server.
        :param kwargs:
        """
        super(XdoPlatformRpcs, self).__init__(**kwargs)

        self.display = Xlib.display.Display(display)
        self.libxdo = xdo.Xdo(display)

        self.xdotool_delay = xdo_delay

        # compute and cache {atom_name: atom_value} dict once to save us from
        # having to repeatedly query X for this data.
        self.x_atoms = {
            name: self.display.intern_atom(name) for name in _X_PROPERTIES
        }

    def server_info(self, security_token=None):
        self._check_security_token(security_token)
        return {
            'window_manager': 'idk',
            'operating_system': 'linux',
            'platform': 'linux',
            'display': 'X11',
            'server': 'x11_libxdo',
            'server_version': 1
        }

    def get_context(self, security_token=None):
        self._check_security_token(security_token)
        try:
            window_id = self.libxdo.get_focused_window_sane()
            window = self.display.create_resource_object('window', window_id)
        except Exception as error:
            self.logger.error('failed to get active window error=%s', error)
            return {}

        properties = {'id': window_id}

        window_class = window.get_wm_class()
        if window_class is not None:
            properties['cls_name'] = window_class[0]
            properties['cls'] = window_class[1]

        window_title = window.get_wm_name()
        if window_title is not None:
            properties['title'] = window_title

        # get additional window properties via xlib.  if the window does not
        # have the property then omit it from <properties>.
        for atom_name, atom in self.x_atoms.items():
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
            pid = self.libxdo.get_pid_window(window_id)
            properties['pid'] = pid
            process = psutil.Process(pid)

            try:
                properties['executable'] = process.exe()
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
            try:
                properties['cmdline'] = process.cmdline()
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        except Exception as e:
            self.logger.error('failed to get process context: %s' % e)

        return properties

    def key_press(self, key=None, modifiers=(), direction='press', count=1,
                  count_delay=None, security_token=None):
        self._check_security_token(security_token)
        assert key is not None

        delay_millis = 0 if count_delay is None or count == 1 else count_delay
        delay_micros = delay_millis * 1000
        modifiers = [_MOD_TRANSLATION.get(mod, mod) for mod in modifiers]
        key = _KEY_TRANSLATION.get(key, key)

        # TODO: We can distill this entire loop down to a single libxdo function
        # call when we figure out how to properly user charcode_t entities from
        # libxdo.
        for _ in range(0, count):
            # modifiers down
            self.libxdo.send_keysequence_window_down(
                    0, '+'.join(modifiers), delay_micros)

            if direction == 'press':
                self.libxdo.send_keysequence_window(0, key, delay_micros)
            elif direction == 'up':
                self.libxdo.send_keysequence_window_up(0, key, delay_micros)
            elif direction == 'down':
                self.libxdo.send_keysequence_window_down(0, key, delay_micros)

            # modifiers up
            self.libxdo.send_keysequence_window_up(
                    0, '+'.join(reversed(modifiers)), delay_micros)

            time.sleep(delay_millis / 1000)  # emulate xdotool sleep

    def write_text(self, text, security_token=None):
        self._check_security_token(security_token)
        self.libxdo.enter_text_window(0, text, self.xdotool_delay*1000)

    def click_mouse(self, button, direction='click', count=1, count_delay=None, security_token=None):
        self._check_security_token(security_token)
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
                self.libxdo.click_window(0, button)
            elif direction == 'down':
                self.libxdo.mouse_down(0, button)
            elif direction == 'up':
                self.libxdo.mouse_up(0, button)
            else:
                raise ValueError(
                    'invalid "direction" parameter: "%s"' % direction)

            time.sleep(delay_millis / 1000)

    def _get_geometry(self, window_id=None, security_token=None):
        self._check_security_token(security_token)
        if window_id is None:
            window_id = self.libxdo.get_focused_window_sane()
        window_location = self.libxdo.get_window_location(window_id)
        window_size = self.libxdo.get_window_size(window_id)
        return {
            'x': int(window_location.x),
            'y': int(window_location.y),
            'screen': window_location.screen.display,
            'height': int(window_size.height),
            'width': int(window_size.width),
        }

    def move_mouse(self, x, y, reference='absolute', proportional=False,
                   phantom=None, security_token=None):
        self._check_security_token(security_token)
        original_location = self.libxdo.get_mouse_location()
        geo = self._get_geometry()

        if proportional:
            x = int(geo['width'] * x)
            y = int(geo['height'] * y)

        if reference == 'absolute':
            x = x if x > 0 else x
            y = y if y > 0 else y
            self.libxdo.move_mouse(x, y)
        elif reference == 'relative_active':
            window_location = self.libxdo.get_window_location(
                    self.libxdo.get_active_window())
            self.libxdo.move_mouse(window_location.x + x, window_location.y + y)
        elif reference == 'relative':
            self.libxdo.move_mouse_relative(x, y)
        else:
            raise ValueError('invalid "reference" parameter "%s"' % reference)

        if phantom is not None:

            self.libxdo.click_window(0, _MOUSE_BUTTONS[phantom])
            self.libxdo.move_mouse(original_location.x, original_location.y)

    def notify(self, message, security_token=None):
        self._check_security_token(security_token)
        try:
            subprocess.Popen(['notify-send', message])
        except Exception as e:
            self.logger.warn('failed to start notify-send process: %s' % e)

