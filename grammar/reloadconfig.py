from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os, aenea
from comsat import ComSat

grammar = Grammar("reload_configuration", context=aenea.global_context)

class ReloadConfiguration(CompoundRule):
  spec = "reload aenea configuration"
  extras = []

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callReloadConfiguration()
      aenea.reload_aenea_configuration()

grammar.add_rule(ReloadConfiguration())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
