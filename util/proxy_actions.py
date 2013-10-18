"""performs black magic on the dragonfly actions objects to force them to
   forward their actions to a remote server."""

import types
import pyparsing
import jsonrpclib

import communications
import config

communication = communications.Proxy(config.HOST, config.PORT)

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

class ProxyBase(object):
  pass
def _get_key_symbols():
  try:
    with open("keys.txt") as keyfile:
      return [line.strip() for line in keyfile]
  except Exception:
    with open("C:\\NatLink\\NatLink\\MacroSystem\\keys.txt") as keyfile:
      return [line.strip() for line in keyfile]

_modifier_keys = {
        "a": "alt",
        "c": "control",
        "s": "shift",
        "w": "super",
        "h": "hyper",
        "m": "meta"
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
    proxy = communications.BatchProxy()
    for key in spec.split(","):
      modifier_part, key_part, command_part, outer_pause_part = \
          self._parser.parseString(key.strip())

      modifiers = ([_modifier_keys[c] for c in modifier_part[0]]
                   if modifier_part else [])
      key = key_part[0]

      # regular keypress event
      if len(command_part) == 1:
        ((pause_part, repeat_part),) = command_part

        repeat = int(repeat_part[1]) if repeat_part else 1
        if not repeat:
          continue
        proxy.key_press(key, modifiers=modifiers, count=repeat)
      # manual keypress event
      else:
        (_, direction) = command_part
        proxy.key_press(key, modifiers=modifiers, direction=direction)

      if outer_pause_part:
        proxy.pause(outer_pause_part[0])

    return proxy._commands

  def _execute_events(self, commands):
    communication.execute_batch(commands)

################################################################################
# Text

class ProxyText(ProxyBase, dragonfly.DynStrActionBase):
  def _parse_spec(self, spec):
    return spec

  def _execute_events(self, events):
    communication.server.write_text(events)

################################################################################
# Mouse

class ProxyMouse(ProxyBase, dragonfly.DynStrActionBase):
  def _parse_spec(self, spec):
    proxy = communications.BatchProxy()
    for item in spec.split(","):
      item = item.strip()
      if item[0] in "[(<":
        # it is a movement
        item, x, y = ([item[0]] + [float(x.strip()) for x in item[1:-1].split() if x.strip()])
        reference = {"[":"absolute",
                     "<":"relative",
                     "(":"relative_active"}[item[0]]
        proxy.move_mouse(x, y, reference=reference, proportional=("." in item[1:-1]))
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

        if drag:
          proxy.click_mouse(key, direction=drag)
        else:
          proxy.click_mouse(key, count=repeat, count_delay=pause)

    return proxy._commands

  def _execute_events(self, commands):
    communication.execute_batch(commands)

################################################################################
# click without moving mouse

class ProxyMousePhantomClick(ProxyMouse):
  """specification is similar to that for mouse except you should only
     specify one move as more events may behave strangely.
     the intended usage is as these examples,
       "(55 274), 1"         # left click once at those coordinates
       "<9 222>, 1:2/10"     # left double-click at those coordinates
       "1:down, [1 1], 1:up" # drag what is there to the upper left corner
     """

  def _parse_spec(self, spec):
    commands = ProxyMouse._parse_spec(self, spec)
    move, click = commands
    move[2]["phantom"] = click[1][0]
    print "bees", click[1][0]
    print move
    return [move]

################################################################################
# do nothing

class NoAction(dragonfly.ActionBase):
  def execute(self):
    pass

################################################################################
# take a different action depending on which context is currently active.
# only works with proxy contexts.

class ProxyContextAction(dragonfly.ActionBase):
  def __init__(self, default=None, actions=[]):
    self.actions = actions
    self.default = default

  def add_context(self, context, action):
    self.actions.append((context, action))

  def execute(self):
    for (context, action) in self.actions:
      if context.matches(None, None, None):
        return action.execute()
    else:
      return self.default.execute()

__all__ = ["ProxyKey", "ProxyText", "ProxyMouse", "NoAction", "ProxyMousePhantomClick", "ProxyContextAction"]
