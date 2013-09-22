from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition)
import natlink, os
from comsat import ComSat

from raul import SelfChoice

grammar_context = AppContext(executable="notepad")
grammar = Grammar("favorites", context=grammar_context)

# {"spoken form":"written form"}
from personal import FAVORITES

class Favorites(CompoundRule):
  spec = "fave  <key>"

  extras = [SelfChoice("key", FAVORITES)]

  def _process_recognition(self, node, extras):
    value = FAVORITES[str(extras["key"])]
    with ComSat() as connection:
      connection.getRPCProxy().callText(value)

grammar.add_rule(Favorites())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
