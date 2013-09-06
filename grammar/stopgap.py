from dragonfly import (Grammar, Key, CompoundRule, Choice, Dictation, List, Optional, Literal, RuleRef)
import dragonfly
import natlink, os

from proxy_contexts import *

from proxy_actions import *

from proxy_nicknames import *

from comsat import ComSat
import actions

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
  cmd = {"syn":"a", "inns":"i", "vim replace":"R", "termie":"Super_L Return"}
  extras = [Choice("cmd", cmd)]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callKeyStack(str(extras["cmd"]).split())

class Translate(CompoundRule):
  spec = "<cmd>"
 
  cmd = {"push":"(", "pop":")", "push push":"((", "pop pop":"))", "slot":"[",
         "straw":"]", "claw":"{", "draw":"}",
         "law raw":"()", "slot straw":"[]", "claw draw":"{}",
         "dick tia":'":"', "singh dick tia":"':'",
         "hash bang shell":"#!/bin/sh",
         "hash bang bash":"#!/bin/sh", "hash bang python":"#!/usr/bin/python"}

  extras = [SelfChoice("cmd", cmd)]
 
  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callText(self.cmd[str(extras["cmd"])])

class Capitalization(CompoundRule):
  spec = "<cmd> <name>"
  cmd = {"camel":"c", "resolution":"R", "steadily":"s", ".word":".", "score":"_", "capscore":"C_",
         "up score":"U_", "jumble":"jumble", "twitter":"twitter", "twit shout":"twitshout",
         "path":"path", "abs path":"abs path", "foma":"foma"}
  extras = [Choice("cmd", cmd), Dictation("name")]
      
  def _process_recognition(self, node, extras):
    name = processDictation(extras["name"]).split()
    cmd = extras["cmd"]
    if cmd == "c":
      var = ''.join([name[0]] + [x.capitalize() for x in name[1:]])
    elif cmd == "s":
      var = ''.join([x.capitalize() for x in name])
    elif cmd == ".":
      var = '.'.join(name).lower()
    elif cmd == "_":
      var = '_'.join(name).lower()
    elif cmd == "path":
      var = '/'.join(name)
    elif cmd == "abs path":
      var = '/' + '/'.join(name)
    elif cmd == "C_":
      var = '_'.join([x.capitalize() for x in name])
    elif cmd == "U_":
      var = '_'.join(name).upper()
    elif cmd == "R":
      var = '::'.join(name)
    elif cmd == "jumble":
      var = "".join(name)
    elif cmd == "foma":
      var = " ".join(word.split("\\")[0] for word in name)
    elif cmd == "twitter":
      var = "".join(name).lower()
    elif cmd == "twitshout":
      var = "".join(name).upper()
    with ComSat() as cs:
      cs.getRPCProxy().callText(var)

class LetMeTalk(CompoundRule):
  spec = "little <text>"
  extras = [Dictation("text")]

  def _process_recognition(self, node, extras):
    with ComSat() as cs:
      cs.getRPCProxy().callText(processDictation(extras["text"]))

grammar.add_rule(Translate())
grammar.add_rule(TranslateSpecial())
grammar.add_rule(LetMeTalk()) 
grammar.add_rule(QuadCommand())
grammar.add_rule(MouseClick())
grammar.add_rule(Capitalization())

#---------------------------------------------------------------------------
# Load the grammar instance and define how to unload it.

grammar.load()

# Unload function which will be called by natlink at unload time.
def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
