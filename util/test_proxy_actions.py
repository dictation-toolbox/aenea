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
    self.proxy.callEvents = mock.MagicMock(return_value=None, side_effect=self.raw.extend)
    self.proxy.callText = mock.MagicMock(return_value=None, side_effect=lambda text: self.raw.append("type " + text))

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
    self.assertEqual(parse("H"), ["key H"])
    self.assertEqual(parse("H, e"), ["key H", "key e"])

    self.assertEqual(parse("c-Home"), ["keydown Control_L", "key Home", "keyup Control_L"])
    self.assertEqual(parse("c-Home:2"), ["keydown Control_L", "key Home", "key Home", "keyup Control_L"])
    self.assertEqual(parse("c-Home:2/5"), ["keydown Control_L", "key Home", "key Home", "keyup Control_L", "sleep 5"])
    self.assertEqual(parse("c-Home/1:2/5"), ["keydown Control_L", "key Home", "sleep 1", "key Home", "keyup Control_L", "sleep 5"])
    self.assertEqual(parse("c-home/1:2/5"), ["keydown Control_L", "key Home", "sleep 1", "key Home", "keyup Control_L", "sleep 5"])
    self.assertEqual(parse("Home"), ["key Home"])
    self.assertEqual(parse("Home:2"), ["key Home"] * 2)

    self.assertEqual(parse("Home:0"), [])
  
  def test_key_multiple_modifiers(self):
    parse = partial(self.get_events, ProxyKey)
    keys = parse("scaw-H")
    mods = ("Shift_L", "Control_L", "Super_L", "Alt_L")
    self.assertEqual(keys[4], "key H")
    self.assertItemsEqual(keys[:4], ["keydown %s" % mod for mod in mods])
    self.assertItemsEqual(keys[-4:], ["keyup %s" % mod for mod in mods])
    mod_up, mod_down = [[x.split()[1] for x in mod_string]
                         for mod_string in (keys[:4], keys[-4:])]
    self.assertEqual(mod_up, list(reversed(mod_down)))

  def test_key_manual(self):
    parse = partial(self.get_events, ProxyKey)
    self.assertEqual(parse("a:up"), ["keyup a"])

  def test_key_windows(self):
    parse = partial(self.get_events, ProxyKey)
    sequence = ["keydown Super_L", "key Delete", "keyup Super_L", "key greater", "key Left",
                "key F9", "key KP_7"]
    self.assertEqual(parse("w-del, rangle, left, f9, np7"), sequence)

  def test_text(self):
    parse = partial(self.get_events, ProxyText)
    self.assertEqual(parse("Hello world!"), ["type Hello world!"])

  def test_mouse_move(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("[3 5]"), ["mousemove %f %f" % (3, 5)])
    self.assertEqual(parse("<7 9>"), ["mousemove_relative %f %f" % (7, 9)])
    self.assertEqual(parse("(3 5)"), ["mousemove_active %f %f" % (3, 5)])

    self.assertEqual(parse(",".join(["[3 5]"] * 3)), ["mousemove %f %f" % (3, 5)] * 3)

  def test_mouse_click(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("left"), ["click 1"])
    self.assertEqual(parse("1"), ["click 1"])
    self.assertEqual(parse("right"), ["click 3"])

    self.assertEqual(parse("wheelup:5"), ["click 4"] * 5)
    self.assertEqual(parse("wheelup:5/9"), ["click 4", "sleep %f" % 0.9] * 5)

  def test_mouse_drag(self):
    parse = partial(self.get_events, ProxyMouse)
    self.assertEqual(parse("middle:up"), ["mouseup 2"])

if __name__ == '__main__':
  unittest.main()
