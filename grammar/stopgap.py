from dragonfly import CompoundRule, Grammar, Key, MappingRule, Integer

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
  extras = [Integer("xcoord", min=0, max=11), Integer("ycoord", min=0, max=11)]

  def _process_recognition(self, node, extras):
    x = extras["xcoord"]
    y = extras["ycoord"]
    xres, yres = config.SCREEN_RESOLUTION
    x = xres * int(x) / 10
    y = yres * int(y) / 10
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
