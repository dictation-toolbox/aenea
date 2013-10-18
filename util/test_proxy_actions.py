import itertools
import sys
import unittest
import mock

import proxy_actions
from proxy_actions import *

import comsat

from functools import partial

class MockComsat(object):
  raw = []
  def __init__(self):
    self.proxy = mock.MagicMock()
    self.proxy.callExecute = mock.MagicMock(return_value=None, side_effect=self.raw.extend)

  def __enter__(self):
    return self

  def __exit__(self, a, b, c):
    pass

  def getRPCProxy(self):
    return self.proxy
comsat.ComSat = MockComsat

class TestActions(unittest.TestCase):
  def get_events(self, proxy, spec):
    del MockComsat.raw[:]
    key = proxy(spec)
    key.execute()
    return MockComsat.raw

  def test_key(self):
    parse = partial(self.get_events, ProxyKey)
    self.assertEqual(parse("H"), [("key", "H")])
    self.assertEqual(parse("H, e"), [("key", "H"), ("key", "e")])

    self.assertEqual(parse("c-home"), [("keydown", "Control_L"), ("key", "home"), ("keyup", "Control_L")])
    self.assertEqual(parse("c-home:2"), [("keydown", "Control_L"), ("key", "home"), ("key", "home"), ("keyup", "Control_L")])
    self.assertEqual(parse("c-home:2/5"), [("keydown", "Control_L"), ("key", "home"), ("key", "home"), ("keyup", "Control_L"), ("sleep", "5")])
    self.assertEqual(parse("c-home/1:2/5"), [("keydown", "Control_L"), ("key", "home"), ("sleep", "1"), ("key", "home"), ("keyup", "Control_L"), ("sleep", "5")])
    self.assertEqual(parse("c-home/1:2/5"), [("keydown", "Control_L"), ("key", "home"), ("sleep", "1"), ("key", "home"), ("keyup", "Control_L"), ("sleep", "5")])
    self.assertEqual(parse("home"), [("key", "home")])
    self.assertEqual(parse("home:2"), [("key", "home")] * 2)

    self.assertEqual(parse("home:0"), [])
  
  def test_key_multiple_modifiers(self):
    parse = partial(self.get_events, ProxyKey)
    keys = parse("scaw-H")
    mods = ("Shift_L", "Control_L", "Alt_L", "Super_L")
    self.assertEqual(keys[4], ("key", "H"))
    self.assertItemsEqual(keys[:4], [("keydown", mod) for mod in mods])
    self.assertItemsEqual(keys[-4:], [("keyup", mod) for mod in mods])
    mod_up, mod_down = [[x for x in mod_string]
                         for mod_string in (keys[:4], keys[-4:])]
    self.assertEqual(zip(*mod_up)[1], zip(*list(reversed(mod_down)))[1])

  def test_key_manual(self):
    parse = partial(self.get_events, ProxyKey)
    self.assertEqual(parse("a:up"), [("keyup", "a")])

  def test_text(self):
    parse = partial(self.get_events, ProxyText)
    self.assertEqual(parse("Hello world!"), [("type", "Hello world!")])

  def test_mouse_move(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("[3 5]"), [("mousemove", "%f %f" % (3, 5))])
    self.assertEqual(parse("<7 9>"), [("mousemove_relative", "%f %f" % (7, 9))])
    self.assertEqual(parse("(3 5)"), [("mousemove_active", "%f %f" % (3, 5))])

    self.assertEqual(parse(",".join(["[3 5]"] * 3)), [("mousemove", "%f %f" % (3, 5))] * 3)

  def test_mouse_click(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("left"), [("click", "1")])
    self.assertEqual(parse("1"), [("click", "1")])
    self.assertEqual(parse("right"), [("click", "3")])

    self.assertEqual(parse("wheelup:5"), [("click", "4")] * 5)
    self.assertEqual(parse("wheelup:5/9"), [("click", "4"), ("sleep", "%f" % 0.9)] * 5)

  def test_mouse_drag(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("middle:up"), [("mouseup", "2")])

if __name__ == '__main__':
  unittest.main()
