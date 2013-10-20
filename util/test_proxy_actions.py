import itertools
import sys
import unittest
import mock

import util.proxy_actions
from util.proxy_actions import *

import util.communications

class TestActions(unittest.TestCase):
  @mock.patch("util.proxy_actions.communication")
  def test_key(self, comm):
    ProxyKey("H").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': []})])

    ProxyKey("H, e").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': []}),
                                           ('key_press', (), {'key': 'e', 'count': 1, 'modifiers': []})])

    ProxyKey("c-home").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'count': 1, 'modifiers': ['control'], 'key': 'home'})])

    ProxyKey("c-home:2").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': ['control']})])

    ProxyKey("c-home:2/5").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': ['control']}),
                                           ('pause', (), {'amount': 0.05})])

    ProxyKey("c-home/1:2/5").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'count_delay': 0.01, 'modifiers': ['control']}),
                                           ('pause', (), {'amount': 0.05})])

    ProxyKey("home").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 1, 'modifiers': []})])

    ProxyKey("home:2").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'home', 'count': 2, 'modifiers': []})])

    ProxyKey("home:0").execute()
    comm.execute_batch.assert_called_with([])

  @mock.patch("util.proxy_actions.communication")
  def test_key_multiple_modifiers(self, comm):
    ProxyKey("scawh-H").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'H', 'count': 1, 'modifiers': ['shift', 'control', 'alt', 'super', 'hyper']})])

  @mock.patch("util.proxy_actions.communication")
  def test_key_manual(self, comm):
    ProxyKey("a:up").execute()
    comm.execute_batch.assert_called_with([('key_press', (), {'key': 'a', 'direction': 'up', 'modifiers': []})])

  @mock.patch("util.proxy_actions.communication")
  def test_text(self, comm):
    ProxyText("Hello world!").execute()
    comm.server.write_text.assert_called_with(text="Hello world!")

  @mock.patch("util.proxy_actions.communication")
  def test_mouse_move(self, comm):
    ProxyMouse("[3, 5]").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'})])

    ProxyMouse("<7 9>").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 7.0, 'y': 9.0, 'proportional': False, 'reference': 'relative'})])

    ProxyMouse("(3, 5)").execute()
    comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'relative_active'})])

    ProxyMouse(",".join(["[3 5]"] * 3)).execute()
    comm.execute_batch.assert_called_with([('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'}),
                                           ('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'}),
                                           ('move_mouse', (), {'x': 3.0, 'y': 5.0, 'proportional': False, 'reference': 'absolute'})])

  @mock.patch("util.proxy_actions.communication")
  def test_mouse_click(self, comm):
    ProxyMouse("left").execute()
    comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'left', 'count': 1, 'count_delay': None, 'direction': 'click'})])

    ProxyMouse("right").execute()
    comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'right', 'count': 1, 'count_delay': None, 'direction': 'click'})])

    ProxyMouse("wheelup:5").execute()
    comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'wheelup', 'count': 5, 'count_delay': None, 'direction': 'click'})])

    ProxyMouse("wheeldown:5/9").execute()
    comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'wheeldown', 'count': 5, 'direction': 'click', 'count_delay': 0.09})])

  @mock.patch("util.proxy_actions.communication")
  def test_drag(self, comm):
    ProxyMouse("middle:up/5").execute()
    comm.execute_batch.assert_called_with([('click_mouse', (), {'button': 'middle', 'direction': 'up', 'count_delay': 0.05, 'count': 1})])

if __name__ == '__main__':
  unittest.main()
