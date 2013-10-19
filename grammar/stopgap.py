from dragonfly import Grammar, Key, CompoundRule, MappingRule
import dragonfly

from raul import SelfChoice

import config

if config.PLATFORM == "proxy":
  import aenea
  grammar = Grammar("stopgap", context=aenea.global_context)
  from proxy_contexts import *
  from proxy_nicknames import *
else:
  grammar = Grammar("stopgap")

class MouseClick(MappingRule):
  mapping = {
      "click [left]":Mouse("left"),
      "click middle":Mouse("middle"),
      "click right":Mouse("right"),
    }

class QuadCommand(CompoundRule):
  spec = "zip <xcoord> <ycoord>"
  cmd = ["zero", "one", "two", "too", "to", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
  extras = [SelfChoice("xcoord", cmd), SelfChoice("ycoord", cmd)]

  def _process_recognition(self, node, extras):
    x = extras["xcoord"]
    y = extras["ycoord"]
    x = numbers.get(x, x)
    y = numbers.get(y, y)
    x = 1280 * int(x) / 10
    y = 800 * int(y) / 10
    Mouse("[%s, %s]" % (x, y)).execute()

class LaunchBrowser(MappingRule):
  mapping = {
      "chrome browsing":Key("h-f"),
      "chrome login":Key("h-a"),
      "chrome social":Key("h-s"),
      "chrome google":Key("h-g"),
      "chrome secure":Key("h-b"),
    }

grammar.add_rule(LaunchBrowser())
grammar.add_rule(QuadCommand())
grammar.add_rule(MouseClick())

#---------------------------------------------------------------------------
# Load the grammar instance and define how to unload it.

grammar.load()

# Unload function which will be called by natlink at unload time.
def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
