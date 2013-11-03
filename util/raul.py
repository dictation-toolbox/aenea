from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os

def SelfChoice(name, ch):
  return Choice(name, dict(zip(ch, ch)))

LETTERS = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel",
           "indigo", "juliet", "kilo", "lima", "mike", "november", "oscar",
           "poppa", "quiche", "romeo", "sierra", "tango", "uniform",
           "victor", "whiskey", "x-ray", "yankee", "zulu"]
LETTERS = dict(zip(LETTERS, (chr(ord("a") + i) for i in range(26))))
temp = {}
for (spoken, written) in LETTERS.iteritems():
  temp[spoken] = written
  temp["upper " + spoken] = written.upper()
LETTERS = temp

DIGITS = ["zero", "one", "to", "3", "for", "5", "6", "7", "8", "nine"]
DIGITS = dict(zip(DIGITS, (chr(ord("0") + i) for i in range(10))))
DIGITS["niner"] = "9"

ALPHANUMERIC = LETTERS.copy()
ALPHANUMERIC.update(DIGITS)

ALPHANUMERIC_EXTENDED = ALPHANUMERIC.copy()

ALPHANUMERIC_EXTENDED["enter"] = "enter"
ALPHANUMERIC_EXTENDED["comma"] = ","
