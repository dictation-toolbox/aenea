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

import unittest
import mock

from aenea.proxy_actions import *


class TestActions(unittest.TestCase):
    @mock.patch('aenea.communications.server')
    def test_key(self, comm):
        ProxyKey('H').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': []})])

        ProxyKey('H, e').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': []}),
                                               ('key_press', (), {'key': 'e', 'count': 1, 'modifiers': []})])

        ProxyKey('Super_R').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'count': 1, 'modifiers': [], 'key': 'Super_R'})])

        ProxyKey('c-home').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'count': 1, 'modifiers': ['control'], 'key': 'home'})])

        ProxyKey('c-home:2').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': ['control']})])

        ProxyKey('c-home:2/5').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': ['control']}),
                                               ('pause', (), {'amount': 0.05})])

        ProxyKey('c-home/1:2/5').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'count_delay': 0.01, 'modifiers': ['control']}),
                                               ('pause', (), {'amount': 0.05})])

        ProxyKey('home').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 1, 'modifiers': []})])

        ProxyKey('home:2').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': []})])

        ProxyKey('home:0').execute()
        comm.execute_batch.assert_called_with([])

    @mock.patch('aenea.communications.server')
    def test_key_multiple_modifiers(self, comm):
        ProxyKey('scawh-H').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': ['shift', 'control', 'alt', 'super', 'hyper']})])

    @mock.patch('aenea.communications.server')
    def test_key_manual(self, comm):
        ProxyKey('a:up').execute()
        comm.execute_batch.assert_called_with([('key_press', (), {'key': 'a', 'direction': 'up', 'modifiers': []})])

    @mock.patch('aenea.communications.server')
    def test_text(self, comm):
        ProxyText('Hello world!').execute()
        comm.write_text.assert_called_with(text='Hello world!')

    @mock.patch('aenea.communications.server')
    def test_mouse_move(self, comm):
        ProxyMouse('[3, 5]').execute()
        comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'})])

        ProxyMouse('<7 9>').execute()
        comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 7.0, 'y': 9.0, 'proportional': False, 'reference': 'relative'})])

        ProxyMouse('(3, 5)').execute()
        comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'relative_active'})])

        ProxyMouse(','.join(['[3 5]'] * 3)).execute()
        comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'}),
                                               ('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'}),
                                               ('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'})])

    @mock.patch('aenea.communications.server')
    def test_mouse_click(self, comm):
        ProxyMouse('left').execute()
        comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'left', 'count': 1, 'count_delay': None, 'direction': 'click'})])

        ProxyMouse('right').execute()
        comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'right', 'count': 1, 'count_delay': None, 'direction': 'click'})])

        ProxyMouse('wheelup:5').execute()
        comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'wheelup', 'count': 5, 'count_delay': None, 'direction': 'click'})])

        ProxyMouse('wheeldown:5/9').execute()
        comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'wheeldown', 'count': 5, 'direction': 'click', 'count_delay': 0.09})])

    @mock.patch('aenea.communications.server')
    def test_drag(self, comm):
        ProxyMouse('middle:up/5').execute()
        comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'middle', 'direction': 'up', 'count_delay': 0.05, 'count': 1})])

    @mock.patch('aenea.communications.server')
    def test_phantom_click(self, comm):
        ProxyMousePhantomClick('(78, 114), left').execute()
        comm.execute_batch.assert_called_with([('move_mouse', (), {'y': 114.0, 'x': 78.0, 'phantom': 'left', 'reference': 'relative_active', 'proportional': False})])

if __name__ == '__main__':
    unittest.main()
