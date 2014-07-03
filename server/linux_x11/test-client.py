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


class Proxy(object):
    def __init__(self, host, port):
        self.server = jsonrpclib.Server('http://%s:%i' % (host, port))

    def execute_batch(self, batch):
        self.server.multiple_actions(batch)


class BatchProxy(object):
    def __init__(self):
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
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
    batch = BatchProxy()
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
    communication = Proxy(config.HOST, config.PORT)
    all_tests(communication.server)
    test_multiple_actions(communication)
    print 'Get context returns:'
    print communication.server.get_context()

if __name__ == '__main__':
    main()
