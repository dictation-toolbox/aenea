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

import mock
import unittest
import threading

import util.communications

import server_x11

HOST = '127.0.0.1'
PORT = 58382


class test_server_x11_info(unittest.TestCase):
    def get_context(self, return_value):
        comm = util.communications.Proxy(HOST, self.port)
        self.assertEqual(comm.server.get_context(), return_value)
        self.server.shutdown()

    @mock.patch('server_x11.get_context')
    def test_get_context(self, get_context):
        self.port = PORT
        return_value = {'test_successful': 'Yep'}
        get_context.return_value = return_value
        get_context.__name__ = 'get_context'
        self.server = server_x11.setup_server(HOST, self.port)
        test_thread = threading.Thread(
            target=self.get_context,
            args=[return_value]
            )
        test_thread.start()
        self.server.serve_forever()
        test_thread.join()
        get_context.assert_called_with()


class test_server_x11_actions(unittest.TestCase):
    def setUp(self):
        self.server = None
        self.test_thread = None
        self.port = None

    def run_request_thread(self, client):
        self.server = server_x11.setup_server(HOST, self.port)
        test_thread = threading.Thread(target=client)
        test_thread.start()
        self.server.serve_forever()
        test_thread.join()

    def key_press(self, comm):
        comm.key_press(key='a')
        comm.key_press(key='a', modifiers=['shift'])
        comm.key_press(key='shift', direction='down')
        comm.key_press(key='b', count_delay=100, count=3)
        comm.key_press(key='shift', direction='up')

    @mock.patch('server_x11.run_command')
    def test_key_press(self, run_command):
        self.port = PORT + 1
        commands = []
        run_command.side_effect = lambda cmd: commands.append(cmd)
        self.run_request_thread(self.single_request_client(self.key_press))
        self.assertEqual(commands, [
            'key a',
            'keydown Shift_L key a keyup Shift_L',
            'keydown Shift_L',
            '--delay 100 key b key b key b',
            'keyup Shift_L'
            ])

    def write_text(self, comm):
        comm.write_text(text='Hello world!')

    @mock.patch('server_x11.write_command')
    def test_write_text(self, write_command):
        self.port = PORT + 2
        self.run_request_thread(self.single_request_client(self.write_text))
        write_command.assert_called_with(
            'Hello world!',
            arguments='type --file - --delay 0'
            )

    def click_mouse(self, comm):
        comm.click_mouse(button='left', count=2)
        comm.click_mouse(button='wheelup', count=2)
        comm.click_mouse(button='right')
        comm.click_mouse(button='right', count=5, count_delay=70)
        comm.click_mouse(button='middle', count_delay=7)

    @mock.patch('server_x11.run_command')
    def test_click_mouse(self, run_command):
        self.port = PORT + 3
        commands = [
            mock.call('click  --repeat 2 1'),
            mock.call('click  --repeat 2 4'),
            mock.call('click   3'),
            mock.call('click --delay 70  --repeat 5 3'),
            mock.call('click   2')
            ]
        self.run_request_thread(self.single_request_client(self.click_mouse))
        self.assertEqual(commands, run_command.mock_calls)

    def move_mouse(self, comm):
        comm.move_mouse(x=0, y=0)
        comm.pause(amount=100)
        comm.move_mouse(x=0.5, y=0.5, proportional=True)
        comm.pause(amount=100)
        comm.move_mouse(x=75, y=45, reference='relative_active')
        comm.pause(amount=100)
        comm.move_mouse(x=0, y=0, phantom='left')
        comm.pause(amount=100)
        comm.move_mouse(x=0, y=50, reference='relative')
        comm.pause(amount=100)

    @mock.patch('server_x11.get_active_window')
    @mock.patch('time.sleep')
    @mock.patch('server_x11.get_geometry')
    @mock.patch('server_x11.run_command')
    def test_move_mouse(self, run_command, geo, wait, get_active_window):
        self.port = PORT + 4
        geo.return_value = {
            'x': 50,
            'y': 50,
            'width': 100,
            'height': 100,
            'screen': 0
            }
        get_active_window.return_value = 103, 'test'
        commands = [
            mock.call('mousemove 0.000000 0.000000'),
            mock.call('mousemove 50.000000 50.000000'),
            mock.call('mousemove --window 103 75.000000 45.000000'),
            mock.call('mousemove 0.000000 0.000000 click 1 mousemove restore'),
            mock.call('mousemove_relative 0.000000 50.000000')
            ]
        self.run_request_thread(self.single_request_client(self.move_mouse))
        self.assertEqual(commands, run_command.mock_calls)
        self.assertEqual([mock.call(0.1)] * 5, wait.mock_calls)

    def drag_mouse(self, comm):
        comm.move_mouse(x=0, y=0, proportional=True)
        comm.click_mouse(button='left', direction='down')
        comm.move_mouse(x=1, y=1, proportional=True)
        comm.click_mouse(button='left', direction='up')

    @mock.patch('server_x11.get_active_window')
    @mock.patch('server_x11.get_geometry')
    @mock.patch('server_x11.run_command')
    def test_drag_mouse(self, run_command, geo, get_active_window):
        self.port = PORT + 5
        geo.return_value = {
            'x': 50,
            'y': 50,
            'width': 100,
            'height': 100,
            'screen': 0
            }
        get_active_window.return_value = 103, 'test'
        commands = [
            mock.call('mousemove 0.000000 0.000000'),
            mock.call('mousedown   1'),
            mock.call('mousemove 100.000000 100.000000'),
            mock.call('mouseup   1')
            ]
        self.run_request_thread(self.single_request_client(self.drag_mouse))
        self.assertEqual(commands, run_command.mock_calls)

    def pause(self, comm):
        comm.pause(amount=500)

    def single_request_client(self, test_suite):
        def worker():
            test_suite(util.communications.Proxy(HOST, self.port).server)
            self.server.shutdown()
        return worker

    @mock.patch('time.sleep')
    def test_pause(self, wait):
        self.port = PORT + 6
        self.run_request_thread(self.single_request_client(self.pause))
        wait.assert_called_with(0.5)

    def multiple_actions(self):
        batch = util.communications.BatchProxy()
        self.key_press(batch)
        self.write_text(batch)
        self.click_mouse(batch)
        self.pause(batch)
        self.write_text(batch)
        self.key_press(batch)
        self.click_mouse(batch)
        self.pause(batch)
        proxy = util.communications.Proxy(HOST, self.port)
        proxy.execute_batch(batch._commands)
        self.server.shutdown()

    @mock.patch('server_x11.write_command')
    @mock.patch('server_x11.flush_xdotool')
    @mock.patch('server_x11.run_command')
    def test_multiple_actions(self, run_command, flush, write_command):
        calls = []

        def mock_flush(actions):
            '''Mock has issues with the del [:].'''
            if actions:
                calls.append(actions[:])
            del actions[:]

        flush.side_effect = mock_flush
        self.port = PORT + 7
        self.server = server_x11.setup_server(HOST, self.port)

        test_thread = threading.Thread(target=self.multiple_actions)
        test_thread.start()
        self.server.serve_forever()
        test_thread.join()

        # No easy way to test interleaving, so we rely on shape of flushes
        # to check proper happens-before.
        self.assertEqual(
            write_command.mock_calls,
            [mock.call(
                'Hello world!',
                arguments='type --file - --delay 0'
                )] * 2
            )

        step1 = [
            'key a',
            'keydown Shift_L',
            'key a',
            'keyup Shift_L',
            'keydown Shift_L',
            'key b',
            'key b',
            'key b',
            'keyup Shift_L'
            ]

        step2 = [
            'click  --repeat 2 1',
            'click  --repeat 2 4',
            'click   3',
            'click --delay 70  --repeat 5 3',
            'click   2',
            'sleep 0.500000'
            ]

        self.assertEqual(calls, [step1, step2, step1 + step2])


if __name__ == '__main__':
    unittest.main()
