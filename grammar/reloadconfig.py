from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os
from comsat import ComSat

grammar_context = AppContext(executable="notepad")
grammar = Grammar("reload_configuration", context=grammar_context)

class ReloadConfiguration(CompoundRule):
  spec = "reload aenea configuration"
  extras = []
  print "Reloading stopgap."

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callReloadConfiguration()
      for name in os.listdir("E:\\aenea\\grammar"):
        if name.endswith(".py"):
          with open("E:\\aenea\\grammar\\%s" % name) as infd:
            with open("C:\\NatLink\\NatLink\\MacroSystem\\_%s" % name, "w") as outfd:
              outfd.write(infd.read())
      for name in os.listdir("E:\\aenea\\util"):
        if name.endswith(".py") or name.endswith(".txt"):
          with open("E:\\aenea\\util\\%s" % name) as infd:
            with open("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, "w") as outfd:
              outfd.write(infd.read())

grammar.add_rule(ReloadConfiguration())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
