from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os

from comsat import ComSat

from raul import SelfChoice, processDictation, NUMBERS as numbers


grammar_context = AppContext(executable="notepad")
grammar = Grammar("vim", context=grammar_context)

class VimCommand(CompoundRule):
  spec = "vim <cmd>"
  cmd = ["save", "save and quick", "quit bang", "quit"]
  extras = [SelfChoice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callKeys("Escape")
      cmd = {"save":"w", "save and quit":"wq", "quit":"q", "quit bang":"q!"}[str(extras["cmd"])]
      rpc.callText(":%s\n" % cmd)

class VIMRule4(CompoundRule):
  spec = "<cmd> <cmd2>"
  cmd = {"Grab":"y", "Inc.":"y", "Dell":"d"}
  extras = [Choice("cmd", cmd), Choice("cmd2", cmd)]

  def _process_recognition(self, node, extras):
    if extras["cmd"] == extras["cmd2"]:
      with ComSat() as cs:
        cs.getRPCProxy().callText("%s%s" % ((extras["cmd"],) * 2))

class VIMRule3point5(CompoundRule):
  spec = "<cmd> word"
  cmd = {"Grab":"y", "Inc.":"y", "Dell":"d"}
  extras = [Choice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callText("%sw" % extras["cmd"])
        
class VIMRule3(CompoundRule):
  spec = "<cmd> rest"
  cmd = {"Grab":"y", "Inc.":"y", "Dell":"d"}
  extras = [Choice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callText("%s$" % extras["cmd"])

class VIMRule2(CompoundRule):
  cmd = {"Grab":"y", "Inc.":"y", "Dell":"d"}
  extras = [Choice("cmd", cmd), Choice("number", numbers)]
  spec = "<cmd> <number>"

  def _process_recognition(self, node, extras):
    number = int(numbers[str(extras["number"])])
    with ComSat() as cs:
      cs.getRPCProxy().callText("%s%i%s" % (extras["cmd"], number, extras["cmd"]))

class VIMRule(CompoundRule):
  spec = "<number> <cmd>"
  extras = [Choice("cmd", {"J":"j", "K":"k", "H":"h", "L":"l", "P":"p",
                           "word":"w", "Oscar":"o",
                           "up Oscar":"O"}), Dictation("number", numbers)]

  def _process_recognition(self, node, extras):
    number = int(processDictation(numbers[str(extras["number"])]))
    with ComSat() as cs:
      cs.getRPCProxy().callText("%i%s" % (number, extras["cmd"]))

grammar.add_rule(VIMRule())
grammar.add_rule(VimCommand())
grammar.add_rule(VIMRule2())
grammar.add_rule(VIMRule3())
grammar.add_rule(VIMRule3point5())
grammar.add_rule(VIMRule4())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
