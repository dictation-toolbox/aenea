"""performs black magic on the dragonfly actions objects to force them to
   forward their actions to a remote server."""

import comsat
import types
import pyparsing

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

import proxy
communications = proxy.communications

class ProxyBase(object):
  pass

################################################################################
# Key

# mapping from windows key names to the equivalent linux symbols (where different). 
WINDOWS_MAPPING = {"pgup":"Prior", "pgdown":"Next", "backspace":"BackSpace",
                   "del":"Delete", "backtick":"grave", "caret":"asciicircum",
                   "dot":"period", "dquote":"quotedbl", "exclamation":"exclam",
                   "hash":"numbersign", "hyphen":"minus", "squote":"apostrophe",
                   "tilde":"asciitilde", "langle":"less", "rangle":"greater",
                   "lbracket":"bracketleft", "rbracket":"bracketright",
                   "lparen":"parenleft", "rparen":"parenright",
                   "lbrace":"braceleft", "rbrace":"braceright", "apps":"Menu",
                   "win":"Super_L", "npadd":"KP_Add", "npdec":"KP_Decimal",
                   "npdiv":"KP_Divide", "npmul":"KP_Multiply"}
for key in (["left", "right", "up", "down", "home", "end", "tab", "insert",
             "escape"] + ["f%i" % i for i in xrange(1, 13)]):
  WINDOWS_MAPPING[key] = key[0].upper() + key[1:]
for index in xrange(10):
  WINDOWS_MAPPING["np%i" % index] = "KP_%i" % index

def _get_key_symbols():
  try:
    with open("keys.txt") as keyfile:
      return [line.strip() for line in keyfile] + list(WINDOWS_MAPPING)
  except:
    with open("C:\\NatLink\\NatLink\\MacroSystem\\keys.txt") as keyfile:
      return [line.strip() for line in keyfile] + list(WINDOWS_MAPPING)

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
  _text_clause = (pyparsing.Literal("[") + pyparsing.Word(pyparsing.alphanums) +
                  pyparsing.Literal("]"))

  def _parse_spec(self, spec):
    def handle_pause(pause_spec):
      if pause_spec:
        _, sleeptime = pause_spec
        return ["sleep %i" % int(sleeptime)]
      else:
        return []
     
    actions = []
    for key in spec.split(","):
      try:
        text_parse = self._text_clause.parseString(key.strip())
        if len(text_parse) == 3:
          actions.append("type " + text_parse[1])
          continue
      except pyparsing.ParseException:
        pass

      modifier_part, key_part, command_part, outer_pause_part = \
          self._parser.parseString(key.strip())

      modifiers = ([_modifier_keys[c] for c in modifier_part[0]]
                   if modifier_part else [])
      key = WINDOWS_MAPPING.get(key_part[0], key_part[0])
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
      proxy.callText(events)

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
    return ProxyMouse._parse_spec(self, spec) + ["mousemove restore"]

################################################################################
# do nothing

class NoAction(dragonfly.ActionBase):
  def execute(self):
    pass

__all__ = ["ProxyKey", "ProxyText", "ProxyMouse", "NoAction", "ProxyMousePhantomClick"]
