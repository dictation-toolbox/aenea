from dragonfly import (Grammar, Key, CompoundRule, Choice, Dictation, List, Optional, Literal, RuleRef)
import dragonfly
import natlink, os

from proxy_contexts import *

from proxy_actions import *

from proxy_nicknames import *

from comsat import ComSat

from raul import SelfChoice, processDictation, NUMBERS as numbers

#---------------------------------------------------------------------------
# Create this module's grammar and the context under which it'll be active.

grammar_context = dragonfly.AppContext(executable="notepad") & (~AppRegexContext(".*VirtualBox.*"))
grammar = Grammar("notepad_example", context=grammar_context)

class MouseClick(CompoundRule):
  spec = "click [<cmd>]"
  cmd = ["one", "two", "to", "too", "three"]
  extras = [SelfChoice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    button = str(extras.get("cmd", 1))
    button = int(numbers.get(button, button))
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callClick(button)

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
    with ComSat() as cs:
      rpc = cs.getRPCProxy()
      rpc.callMouse(x, y)
      if "zap" in extras:
        rpc.callClick(1)

class TranslateSpecial(CompoundRule):
  spec = "<cmd>"
  # say: law raw slaw sraw claw craw
  cmd = {"syn":"a", "inns":"i", "vim replace":"R", "termie":"Super_L Return", "chrome browsing":"Hyper_L f",
        "chrome login":"Hyper_L a", "chrome social":"Hyper_L s", "chrome google":"Hyper_L g",
        "chrome secure":"Hyper_L"}
  extras = [Choice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callKeyStack(str(extras["cmd"]).split())

class Translate(CompoundRule):
  spec = "<cmd>"
 
  cmd = {"hash bang shell":"#!/bin/sh",
         "hash bang bash":"#!/bin/sh", "hash bang python":"#!/usr/bin/python"}

  extras = [SelfChoice("cmd", cmd)]
 
  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callText(self.cmd[str(extras["cmd"])])

grammar.add_rule(Translate())
grammar.add_rule(TranslateSpecial())
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
