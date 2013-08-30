from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context)
import natlink, os
vim = __import__("_vim")
from comsat import ComSat

from raul import SelfChoice

class ShellContext(Context):
  def __init__(self):
    self._str = "ShellContext"

  def matches(self, executable, title, handle):
    with ComSat() as cs:
      print cs.getRPCProxy().callGetState()["in_terminal"]
      return cs.getRPCProxy().callGetState()["in_terminal"]

grammar_context = (AppContext(executable="notepad") & ShellContext()) & (~vim.VimContext())
grammar = Grammar("shell", context=grammar_context)

class ListDirectoryContents(CompoundRule):
  spec = "lists <flags>"
  flags = {"LH":"-lh"}#, "L":"-l", "LHT":"-lht", "":"", "LHT head":"-lht | head"}
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
