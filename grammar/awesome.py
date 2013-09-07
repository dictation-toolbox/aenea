from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition)
import natlink, os
from comsat import ComSat

from raul import SelfChoice, NUMBERS

grammar_context = AppContext(executable="notepad")
grammar = Grammar("awesome", context=grammar_context)

awesome = "Super_L"
control = "Control_L"
shift = "Shift_L"
altar = "Alt_L"
vorpal = "Hyper_L"

identifiers = ["1", "one", "to", "two", "too", "3", "three", "4",
               "for", "fore", "four", "5", "five", "6", "six", "7", "seven", "8",
               "9", "nine"]

class Basics(CompoundRule):
  spec = "(whim | notion | ion) <command>"

  commands = {"screen":[awesome, control, "k"],
              "up":[awesome, "k"],
              "down":[awesome, "j"],
              "left":[awesome, shift, "k"],
              "right":[awesome, shift, "j"],
              "change screen":[awesome, "o"],
              "close client":[awesome, shift, "c"],
              "snap":[awesome, control, "Return"],
              "full":[awesome, "m"]
              }

  extras = [SelfChoice("command", commands)]

  def _process_recognition(self, node, extras):
    command = self.commands[str(extras["command"])]
    with ComSat() as connection:
      connection.getRPCProxy().callKeyStack(" ".join(command))

class Numeric(CompoundRule):
  spec = "(whim | notion | ion) [<command>] <identifier>"

  commands = {"work":[],
              "move":[shift, control],
              "tag":[shift, control],
              "tag marked":[shift],
              "move marked":[shift]}

  extras = [SelfChoice("command", commands),
            SelfChoice("identifier", identifiers)]

  def _process_recognition(self, node, extras):
    command = self.commands[str(extras.get("command", "work"))]
    identifier = NUMBERS.get(extras["identifier"])
    with ComSat() as connection:
      connection.getRPCProxy().callKeyStack(" ".join([awesome] + command + [identifier]))

grammar.add_rule(Basics())
grammar.add_rule(Numeric())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
