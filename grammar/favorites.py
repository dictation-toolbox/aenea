from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition)
import natlink, os
from comsat import ComSat

from raul import SelfChoice

grammar_context = AppContext(executable="notepad")
grammar = Grammar("favorites", context=grammar_context)

favorites = {"dir":
    {"downloads":"/home/alexr/Downloads",
     "home":"/home/alexr",
     "projects":"/home/alexr/projects",
     "media":"/home/alexr/media",
     "media static":"/home/alexr/media/static",
     "media cash":"/home/alexr/media/cache",
     "expire":"/home/alexr/expire",
     "norgan":"/home/alexr/norgan",
     "business fake bio":"/home/alexr/business/fakebio",
     "norgan keys":"/home/alexr/norgan/keys"}}

class Favorites(CompoundRule):
  spec = "fave [kind] [key]"

  extras = [SelfChoice("kind", favorites),
            SelfChoice("key", favorites["dir"])]

  def _process_recognition(self, node, extras):
    value = favorites[extras["kind"]][extras["key"]]

    with ComSat() as connection:
      connection.getRPCProxy().callText(value)

grammar.add_rule(Favorites())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
