from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context)
import natlink, os

from comsat import ComSat

from raul import SelfChoice, processDictation, NUMBERS as numbers

class VimContext(Context):
  def __init__(self):
    self._str = "VimContext"

  def matches(self, executable, title, handle):
    with ComSat() as cs:
      active_title = cs.getRPCProxy().callGetState()["active_title"]
      if active_title:
        active_title = active_title.strip().lower()
      else:
        return False

      return (active_title.startswith("vim ") or active_title.endswith(" vim") or
              " vim " in active_title)

grammar_context = AppContext(executable="notepad") & VimContext()
grammar = Grammar("vim", context=grammar_context)

class EasyMotion(CompoundRule):
  spec = "<command> [<end>] [<inout>]"
  command = SelfChoice("command", ["leap", "jump"])
  end = SelfChoice("end", ["start", "end"])
  inout = SelfChoice("inout", ["in", "out"])

  extras = [command, end, inout]

  def _process_recognition(self, node, extras):
    command = str(extras["command"])
    location = str(extras.get("end", "start"))
    inout = str(extras.get("inout", "in"))

    shortcut = {("leap", "start"):"b",
                ("leap", "end"):"ge",
                ("jump", "start"):"w",
                ("jump", "end"):"e"}[(command, location)]

    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callKeys("Escape backslash backslash " + shortcut)

class VimSearch(CompoundRule):
  spec = "vim <cmd> [<number>]"
  cmd = {"query":"/", "query back":"?",
         "search":"/", "search back":"?"}
  extras = [SelfChoice("cmd", cmd), SelfChoice("number", numbers)]

  def _process_recognition(self, node, extras):
    number = int(numbers.get(str(extras.get("number", "one")), "1"))
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callKeys("Escape")
      cmd = self.cmd[str(extras["cmd"])]
      rpc.callText("%i%s" % (number, cmd))
      if "search" in str(extras["cmd"]):
        rpc.callKeys(["Return"])

class VimCommand(CompoundRule):
  spec = "vim <cmd>"
  cmd = {"write":"w", "write and quit":"wq", "quit bang":"q!", "quit":"q",
         "undo":"u", "redo":":redo"}
  extras = [SelfChoice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callKeys("Escape")
      cmd = self.cmd[str(extras["cmd"])]
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
  cmd = {"Grab":"y", "Inc.":"y", "Dell":"d", "back dell":"d a"}
  extras = [SelfChoice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callModifiedKeys("%s w" % self.cmd[str(extras["cmd"])])
        
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
                   "word":"w", "Oscar":"o", "go":"G",
                   "up Oscar":"O"}), Dictation("number", numbers)]

  def _process_recognition(self, node, extras):
    number = int(processDictation(numbers[str(extras["number"])]))
    with ComSat() as cs:
      cs.getRPCProxy().callText("%i%s" % (number, extras["cmd"]))

grammar.add_rule(EasyMotion())
grammar.add_rule(VIMRule())
grammar.add_rule(VimCommand())
grammar.add_rule(VIMRule2())
grammar.add_rule(VIMRule3())
grammar.add_rule(VIMRule3point5())
grammar.add_rule(VIMRule4())
grammar.add_rule(VimSearch())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
