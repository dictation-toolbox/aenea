from dragonfly import (AppContext, Grammar, Dictation, IntegerRef, MappingRule,
                       Key, Text, IntegerRef, Dictation)

import aenea
import config

if config.PLATFORM == "proxy":
  from proxy_nicknames import *
  chromium_context = (AppContext(cls_name="chromium", cls="chromium") &
                      aenea.global_context)
else:
  chromium_context = (AppContext(executable="chrome") |
                      AppContext(executable="chromium"))

chromium_grammar = Grammar("chromium", context=chromium_context)

class ChromiumRule(MappingRule):
  mapping = {
      "close [<n>] ( frame | frames )":         Key("c-w:%(n)d"),
      "open frame":                             Key("c-t"),
      "open window":                            Key("c-n"),
      "reopen [<n>] ( frame | frames )":        Key("cs-t:%(n)d"),
      "[ go to ] frame [<n>]":                  Key("c-%(n)d"),
      "frame left [<n>]":                       Key("cs-tab:%(n)d"),
      "frame right [<n>]":                      Key("c-tab:%(n)d"),
      "search [<text>]":                        Key("c-k") + Text("%(text)s"),
      "find [<text>]":                          Key("c-f") + Text("%(text)s"),
      "history":                                Key("c-h"),
      "reload":                                 Key("c-r"),
      "next [<n>]":                             Key("c-g:%(n)d"),
      "previous [<n>]":                         Key("cs-g:%(n)d"),
      "back [<n>]":                             Key("a-left:%(n)d"),
      "forward [<n>]":                          Key("a-right:%(n)d"),
      }

  extras = [IntegerRef("n", 1, 10), Dictation("text")]
  defaults = {"n":1, "text":""}

chromium_grammar.add_rule(ChromiumRule())

chromium_grammar.load()

def unload():
  global chromium_grammar 
  if chromium_grammar:
    chromium_grammar.unload()
  chromium_grammar = None
