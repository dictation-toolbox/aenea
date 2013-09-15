from dragonfly import (Grammar, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, MappingRule, IntegerRef, Pause)
import natlink, os, time

from proxy_nicknames import Key, Text, AppRegexContext

from comsat import ComSat

from raul import SelfChoice, processDictation, NUMBERS as numbers

import aenea

LEADER_KEY = "comma"

leader = Key(LEADER_KEY)
escape = Key("Escape")
escape_leader = escape + Pause("30") + leader

vim_context = aenea.global_context & AppRegexContext(name="(?i)^.* vim$")

command_t_context = AppRegexContext(name="^GoToFile.*$") & vim_context
fugitive_index_context = AppRegexContext(name="^index.*\.git.*$") & vim_context

grammar = Grammar("vim", context=vim_context)

class EasyMotion(MappingRule):
  mapping = {"easy jump [start] [<place>]":escape_leader + leader + Key("W") + Text("%(place)s"),
             "easy jump end [<place>]":escape_leader + leader + Key("E") + Text("%(place)s"),
             "easy hop [start] [<place>]":escape_leader + leader + Key("w") + Text("%(place)s"),
             "easy hop end [<place>]":escape_leader + leader + Key("e") + Text("%(place)s"),
             "easy leap [start] [<place>]":escape_leader + leader + Key("B") + Text("%(place)s"),
             "easy leap end [<place>]":escape_leader + leader + Key("g, E") + Text("%(place)s"),
             "easy bounce [start] [<place>]":escape_leader + leader + Key("b") + Text("%(place)s"),
             "easy bounce end [<place>]":escape_leader + leader + Key("g, e") + Text("%(place)s")}
  extras = [Dictation("place")]
  default = {"place":""}

# i guess if you write a vim plugin you get to name it but i can't claim to understand these two...
class LustyJuggler(MappingRule):
  mapping = {"jug | juggle":escape_leader + Text("lj"),
             "(jug | juggle) <n>":escape_leader + Key("l, j, %(n)d") + Pause("20") + Key("Return") + Pause("20") + Key("i")}
  extras = [IntegerRef("n", 0, 10)]

class LustyExplorer(MappingRule):
  mapping = {"rusty":escape_leader + Key("l, r"),
             "rusty absolute":escape_leader + Key("r, f"),
             "rusty <name>":escape_leader + Key("l, r") + Text("%(name)s"),
             "rusty absolute <name>":escape_leader + Key("l, f") + Text("%(name)s")}
  extras = [Dictation("name")]

class CommandT(MappingRule):
  mapping = {"command tea":escape_leader + Key("t"),
             "command tea [<text>]":escape_leader + Key("t") + Pause("20") + Text("%(text)s\n"),
             "command tea buffer":escape + Text(":CommandTBuffer\n"),
             "command tea buffer [<text>]":escape_leader + Key("t") + Pause("20") + Text("%(text)s\n"),
             "command tea (tags | tag)":escape + Text(":CommandTTag\n"),
             "command tea jump":escape + Text(":CommandTJump\n")}
  extras = [Dictation("text")]

class Fugitive(MappingRule):
  mapping = {"git status":escape + Text(":Gstatus\n"),
             "git commit":escape + Text(":Gcommit\n"),
             "git diff":escape + Text(":Gdiff\n"),
             "git move":escape + Text(":Gmove\n"),
             "git remove":escape + Text(":Gremove\n")}

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
    if "search" in str(extras["cmd"]):
      Text("i").execute()

class VimCommand(CompoundRule):
  spec = "vim <cmd>"
  cmd = {"write":"w", "write and quit":"wq", "quit bang":"q!", "quit":"q",
         "undo":"u", "redo":":redo", "[buf] close":"bd"}
  extras = [SelfChoice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callKeys("Escape")
      cmd = self.cmd[str(extras["cmd"])]
      rpc.callText(":%s\n" % cmd)
    if cmd == "w":
      time.sleep(0.2) # vim does not seem to notice the keystroke unless it occurs after the save is complete
      Key("i").execute()

class GoCommand(MappingRule):
  mapping = {"<n> go":Key("Escape") + Text("%(n)dGi")}
  extras = [IntegerRef("n", 1, 1000)] #  for longer files i can just use manual keystroke commands

grammar.add_rule(EasyMotion())
grammar.add_rule(VimCommand())
grammar.add_rule(VimSearch())
grammar.add_rule(GoCommand())
grammar.add_rule(LustyJuggler())
grammar.add_rule(LustyExplorer())
grammar.add_rule(CommandT())
grammar.add_rule(Fugitive())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
