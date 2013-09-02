from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition)
import natlink, os
from comsat import ComSat

from raul import SelfChoice, ALPHANUMERIC

grammar_context = AppContext(executable="notepad")
grammar = Grammar("nato", context=grammar_context)

class MetaKey(CompoundRule):
  spec = "<modifiers> <key>"

  modifier_keys = {"control":"Control_L", "altar":"Alt_L",
                   "shift":"Shift_L", "vorpal":"Hyper_L",
                   "zach":"Super_R"}
  modifier = SelfChoice("modifier", modifier_keys)

  extras = [Repetition(modifier, name="modifiers", max=4),
            SelfChoice("key", ALPHANUMERIC)]
            
  def _process_recognition(self, note, extras):
    modifiers = set(map(self.modifier_keys.get, map(str, extras["modifiers"])))
    with ComSat() as connection:
      actions = ([("keydown %s" % mod) for mod in modifiers] +
                 ["key " + ALPHANUMERIC[str(extras["key"])]] +
                 [("keyup %s" % mod) for mod in modifiers])
      connection.getRPCProxy().callRaw(actions)

class UpperKeypress(CompoundRule):
  spec = "upper <key>"

  letter = SelfChoice("key", ALPHANUMERIC)
  extras = [letter]

  def _process_recognition(self, node, extras):
    key = ALPHANUMERIC[str(extras["key"])]
    with ComSat() as connection:
      connection.getRPCProxy().callText(key.upper())

class Keypress(CompoundRule):
  spec = "<key>"

  letter = SelfChoice("key", ALPHANUMERIC)
  extras = [letter]

  def _process_recognition(self, node, extras):
    key = ALPHANUMERIC[str(extras["key"])]
    with ComSat() as connection:
      connection.getRPCProxy().callText(key)

class SpellingBee(CompoundRule):
  spec = "letters <letters>"

  letter = SelfChoice("letter", ALPHANUMERIC)
  extras = [Repetition(letter, name="letters", max=50)]

  def _process_recognition(self, node, extras):
    written = "".join(map(ALPHANUMERIC.get, extras["letters"]))
    with ComSat() as connection:
      connection.getRPCProxy().callText(written)

grammar.add_rule(SpellingBee())
grammar.add_rule(MetaKey())
grammar.add_rule(Keypress())
grammar.add_rule(UpperKeypress())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
