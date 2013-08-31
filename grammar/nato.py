from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition)
import natlink, os
from comsat import ComSat

from raul import SelfChoice, ALPHANUMERIC

grammar_context = AppContext(executable="notepad")
grammar = Grammar("nato", context=grammar_context)

#class MetaKey(CompoundRule

class SpellingBee(CompoundRule):
  spec = "letters <letters>"

  letter = SelfChoice("letter", ALPHANUMERIC)
  extras = [Repetition(letter, name="letters", max=50)]

  def _process_recognition(self, node, extras):
    written = "".join(map(ALPHANUMERIC.get, extras["letters"]))
    with ComSat() as connection:
      connection.getRPCProxy().callText(written)

grammar.add_rule(SpellingBee())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
