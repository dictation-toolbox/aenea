import itertools
import sys
import unittest
import mock

import proxy_actions
from proxy_actions import *

import communications

class TestActions(unittest.TestCase):
  @mock.patch("proxy_actions.communication")
  def test_key(self, comm):
    ProxyKey("H").execute()
    comm.execute_batch.assert_called_with([('key_press', ('H',), {'count': 1, 'modifiers': []})])

    ProxyKey("H, e").execute()
    comm.execute_batch.assert_called_with([('key_press', ('H',), {'count': 1, 'modifiers': []}),
                                           ('key_press', ('e',), {'count': 1, 'modifiers': []})])

    ProxyKey("c-home").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 1, 'modifiers': ['control']})])

    ProxyKey("c-home:2").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 2, 'modifiers': ['control']})])

    ProxyKey("c-home:2/5").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 2, 'modifiers': ['control']}),
                                           ('pause', (0.05,), {})])

    ProxyKey("c-home/1:2/5").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 2, 'count_delay': 0.01, 'modifiers': ['control']}),
                                           ('pause', (0.05,), {})])

    ProxyKey("home").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 1, 'modifiers': []})])

    ProxyKey("home:2").execute()
    comm.execute_batch.assert_called_with([('key_press', ('home',), {'count': 2, 'modifiers': []})])

    ProxyKey("home:0").execute()
    comm.execute_batch.assert_called_with([])

  @mock.patch("proxy_actions.communication")
  def test_key_multiple_modifiers(self, comm):
    ProxyKey("scawh-H").execute()
    comm.execute_batch.assert_called_with([('key_press', ('H',), {'count': 1, 'modifiers': ['shift', 'control', 'alt', 'super', 'hyper']})])

  @mock.patch("proxy_actions.communication")
  def test_key_manual(self, comm):
    ProxyKey("a:up").execute()
    comm.execute_batch.assert_called_with([('key_press', ('a',), {'direction': 'up', 'modifiers': []})])

  @mock.patch("proxy_actions.communication")
  def test_text(self, comm):
    ProxyText("Hello world!").execute()
    comm.server.write_text.assert_called_with("Hello world!")

  @mock.patch("proxy_actions.communication")
  def test_mouse_move(self, comm):
    ProxyMouse("[3 5]").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (3.0, 5.0), {'proportional': False, 'reference': 'absolute'})])

    ProxyMouse("<7 9>").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (7.0, 9.0), {'proportional': False, 'reference': 'relative'})])

    ProxyMouse("(3 5)").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (3.0, 5.0), {'proportional': False, 'reference': 'relative_active'})])

    ProxyMouse(",".join(["[3 5]"] * 3)).execute()
    comm.execute_batch.assert_called_with([('move_mouse', (3.0, 5.0), {'proportional': False, 'reference': 'absolute'}), ('move_mouse', (3.0, 5.0), {'proportional': False, 'reference': 'absolute'}), ('move_mouse', (3.0, 5.0), {'proportional': False, 'reference': 'absolute'})])

  @mock.patch("proxy_actions.communication")
  def test_mouse_click(self, comm):
    ProxyMouse("left").execute()
    comm.execute_batch.assert_called_with([('click_mouse', ('left',), {'count': 1, 'count_delay': 0})])

    ProxyMouse("right").execute()
    comm.execute_batch.assert_called_with([('click_mouse', ('right',), {'count': 1, 'count_delay': 0})])

    ProxyMouse("wheelup:5").execute()
    comm.execute_batch.assert_called_with([('click_mouse', ('wheelup',), {'count': 5, 'count_delay': 0})])

    ProxyMouse("wheeldown:5/9").execute()
    comm.execute_batch.assert_called_with([('click_mouse', ('wheeldown',), {'count': 5, 'count_delay': 0.09})])

  @mock.patch("proxy_actions.communication")
  def test_drag(self, comm):
    ProxyMouse("middle:up/5").execute()
    comm.execute_batch.assert_called_with([('click_mouse', ('middle',), {'direction': 'up'})])

if __name__ == '__main__':
  unittest.main()
