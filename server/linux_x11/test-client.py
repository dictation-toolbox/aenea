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

import config

import jsonrpclib

def _add_security_token(token, *a, **kw):
    # Cannot use both positional and keyword arguments
    # (according to JSON-RPC spec.)
    assert not (a and kw)

    # Add the security token.
    if token is not None:
        if a:
            a = a + (token,)
        else:
            kw = kw.copy()
            kw['security_token'] = token
    return a, kw

class Proxy(object):
    def __init__(self, host, port, security_token):
        self.server = jsonrpclib.Server('http://%s:%i' % (host, port))
        self.security_token = security_token

    def execute_batch(self, batch):
        self.server.multiple_actions(batch, self.security_token)

    def __getattr__(self, key):
        def call(*a, **kw):
            # Call the server function with the security token.
            a, kw = _add_security_token(self.security_token, *a, **kw)
            return getattr(self.server, key)(*a, **kw)
        return call


class BatchProxy(object):
    def __init__(self, security_token):
        self._commands = []
        self.security_token = security_token

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
                a, kw = _add_security_token(self.security_token, *a, **kw)
                self._commands.append((key, a, kw))
        return call


def test_key_press(distobj):
    '''Should write aABBB, taking a second between B's.'''
    distobj.key_press(key='a')
    distobj.key_press(key='a', modifiers=['shift'])
    distobj.key_press(key='shift', direction='down')
    distobj.key_press(key='b', count_delay=100, count=3)
    distobj.key_press(key='shift', direction='up')


def test_write_text(distobj):
    '''Should write Hello world!'''
    distobj.write_text(text='Hello world!')


def test_click_mouse(distobj):
    '''should double left click, then wheel up twice, then right click.'''
    distobj.click_mouse(button='left', count=2)
    distobj.click_mouse(button='wheelup', count=2)
    distobj.click_mouse(button='right')


def test_move_mouse(distobj):
    '''Should move mouse to absolute upper left, then middle of screen,
       then middle of active window, then click the upper left and
       restore position, then up 50 from that position.
       One second pause between events. To be clear, the mouse should end
       50 pixels up from the middle of the active window.'''
    distobj.move_mouse(x=0, y=0)
    distobj.pause(amount=100)
    distobj.move_mouse(x=0.5, y=0.5, proportional=True)
    distobj.pause(amount=100)
    distobj.move_mouse(
        x=0.5,
        y=0.5,
        proportional=True,
        reference='relative_active'
        )
    distobj.pause(amount=100)
    distobj.move_mouse(x=0, y=0, phantom='left')
    distobj.pause(amount=100)
    distobj.move_mouse(x=0, y=50, reference='relative')
    distobj.pause(amount=100)


def test_mouse_drag(distobj):
    '''Should left click upper left and drag to center.'''
    distobj.move_mouse(x=0, y=0, proportional=True)
    distobj.click_mouse(button='left', direction='down')
    distobj.move_mouse(x=1, y=1, proportional=True)
    distobj.click_mouse(button='left', direction='up')


def test_pause(distobj):
    '''Should pause five seconds.'''
    distobj.pause(amount=500)


def test_multiple_actions(distobj):
    batch = BatchProxy(distobj.security_token)
    all_tests(batch)
    distobj.execute_batch(batch._commands)


def all_tests(distobj):
    test_key_press(distobj)
    test_write_text(distobj)
    test_click_mouse(distobj)
    test_move_mouse(distobj)
    test_mouse_drag(distobj)
    test_pause(distobj)
    distobj.get_context()


def main():
    security_token = getattr(config, "SECURITY_TOKEN", None)
    communication = Proxy(config.HOST, config.PORT, security_token)
    all_tests(communication)
    test_multiple_actions(communication)
    print 'Get context returns:'
    print communication.get_context()

if __name__ == '__main__':
    main()
