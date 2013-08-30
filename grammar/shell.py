from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os
from comsat import ComSat

from raul import SelfChoice

grammar_context = AppContext(executable="notepad")
grammar = Grammar("shell", context=grammar_context)

class ListDirectoryContents(CompoundRule):
  spec = "lists <flags>"
  flags = {"LH":"-lh", "L":"-l", "LHT":"-lht", "":"", "LHT head":"-lht | head"}
  extras = [SelfChoice("flags", flags)]

  def _process_recognition(self, node, extras):
    flags = self.flags[str(extras.get("flags", ""))]
    with ComSat() as connection:
      connection.getRPCProxy().callText("ls " + flags)

grammar.add_rule(ListDirectoryContents())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
