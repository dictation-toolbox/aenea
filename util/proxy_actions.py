"""performs black magic on the dragonfly actions objects to force them to
   forward their actions to a remote server."""

import comsat
import types

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

from constants import LINUX_KEY_SYMBOLS

import proxy
communications = proxy.communications

class ProxyBase(object):
  pass

################################################################################
# Key

def _get_key_symbols():
  try:
    with open("keys.txt") as keyfile:
      return [line.strip() for line in keyfile]
  except:
    with open("C:\\NatLink\\NatLink\\MacroSystem\\keys.txt") as keyfile:
      return [line.strip() for line in keyfile]

_modifier_keys = {
        "a": "Alt_L",
        "c": "Control_L",
        "s": "Shift_L",
        "w": "Super_L",
        "h": "Hyper_L",
        "m": "Meta_L"
        }

def _make_key_parser():
  from pyparsing import Optional, Literal, Word, Group, Keyword, StringStart, StringEnd, And, Or
  digits = "0123456789"
  modifier_keywords = Word("".join(_modifier_keys))
  key_symbols = Or([Keyword(symbol) for symbol in _get_key_symbols()])
  pause_clause = Optional(Literal("/") + Word("." + digits))
  modifier_clause = Optional(modifier_keywords + Literal("-"))
  key_hold_clause = Literal(":") + Or([Keyword(d) for d in ("up", "down")])
  keypress_clause = Group(Group(pause_clause) + Group(Optional(Literal(":") +
                                                         Word(digits))))

  return (StringStart() + Group(modifier_clause) + Group(key_symbols) +
          Group(key_hold_clause | keypress_clause) + Group(pause_clause) +
          StringEnd())

class ProxyKey(ProxyBase, dragonfly.DynStrActionBase):
  """As Dragonfly's Key except the valid modifiers are a, c, s for alt,
     control and shift respectively, w indicates super and h
     indicates hyper."""
     
  _parser = _make_key_parser()

  def _parse_spec(self, spec):
    def handle_pause(pause_spec):
      if pause_spec:
        _, sleeptime = pause_spec
        return ["sleep %i" % int(sleeptime)]
      else:
        return []
     
    actions = []
    for key in spec.split(","):
      modifier_part, key_part, command_part, outer_pause_part = \
          self._parser.parseString(key.strip())

      modifiers = ([_modifier_keys[c] for c in modifier_part[0]]
                   if modifier_part else [])
      key = key_part[0]
      if key.isalpha() and key.isupper():
        key = key.lower()
        modifiers.append("Shift_L")
      outer_pause = handle_pause(outer_pause_part)

      # regular keypress event
      if len(command_part) == 1:
        ((pause_part, repeat_part),) = command_part

        repeat = int(repeat_part[1]) if repeat_part else 1
        if not repeat:
          continue
        keypress = ["key " + key]
        current_actions = keypress + (handle_pause(pause_part) + keypress) * (repeat - 1)
      # manual keypress event
      else:
        (_, direction) = command_part

        current_actions = ["key%s %s" % (direction, key)]

      actions += ([("keydown %s" % modifier) for modifier in modifiers] +
                  current_actions +
                  [("keyup %s" % modifier) for modifier in reversed(modifiers)] +
                  handle_pause(outer_pause_part))
    return actions

  def _execute_events(self, events):
    with communications as proxy:
      proxy.callEvents(events)


################################################################################
# Text

class ProxyText(ProxyBase, dragonfly.DynStrActionBase):
  def _parse_spec(self, spec):
    return spec

  def _execute_events(self, events):
    with communications as proxy:
      proxy.callEvents(["text " + events])

################################################################################
# Mouse

class ProxyMouse(ProxyBase, dragonfly.DynStrActionBase):
  def _parse_spec(self, spec):
    events = []
    for item in spec.split(","):
      item = item.strip()
      if item[0] in "[(<":
        # it is a movement
        item, x, y = ([item[0]] + [float(x.strip()) for x in item[1:-1].split() if x.strip()])
        command = {"[":"mousemove",
                   "<":"mousemove_relative",
                   "(":"mousemove_active"}[item[0]]
        events.append("%s %f %f" % (command, x, y))
      else:
        pause = 0
        repeat = 1
        drag = None

        if "/" in item:
          item, pause = item.split("/")
          pause = int(pause) * 0.1

        key = item
        if ":" in item:
          key, item = item.split(":")
          if item in ("up", "down"):
            drag = item
          else:
            repeat = int(item)

        key = {"left":1, "middle":2, "right":3, "wheelup":4, "wheeldown":5}.get(key, key)
        key = int(key)

        if drag:
          events.append("mouse%s %i" % (drag, key))
        else:
          single = ["click %i" % key]
          if pause:
            single.append("sleep %f" % pause)
          events.extend(single * repeat)

    return events

  def _execute_events(self, events):
    with communications as proxy:
      proxy.callEvents(events)
