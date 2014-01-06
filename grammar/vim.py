from dragonfly import (
    AppContext,
    Dictation,
    Grammar,
    IntegerRef,
    Key,
    MappingRule,
    Pause,
    RuleRef,
    Text,
  )

import config

from raul import LETTERS

import aenea

LEADER_KEY = "comma"

leader = Key(LEADER_KEY)
escape = Key("escape")
escape_leader = escape + Pause("30") + leader

if config.PLATFORM == "proxy":
  from proxy_nicknames import Key, Text, AppContext
  vim_context = AppContext(match="regex", title="(?i).*VIM.*")
  command_t_context = AppContext(match="regex", title="^GoToFile.*$") & vim_context
  fugitive_index_context = AppContext(match="regex", title="^index.*\.git.*$") & vim_context
  global_context = vim_context & aenea.global_context
else:
  vim_context = AppContext(title="VIM")
  command_t_context = AppContext(title="GoToFile") & vim_context
  fugitive_index_context = AppContext(title="index") & AppContext(".git") & vim_context
  global_context = vim_context

grammar = Grammar("vim", context=global_context)

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
             "(jug | juggle) <n>":escape_leader + Key("l, j, %(n)d") + Pause("20") + Key("enter") + Pause("20") + Key("i")}
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

class VimCommand(MappingRule):
  mapping = {
      "vim query [<text>]":escape + Text("/%(text)s"),
      "vim query back [<text>]":escape + Text("?%(text)s"),
      "vim search":escape + Text("/\n"),
      "vim search back":escape + Text("?\n"),

      "vim write":escape + Text(":w\n"),
      "vim write and quit":escape + Text(":wq\n"),
      "vim quit bang":escape + Text(":q!\n"),
      "vim quit":escape + Text(":q\n"),
      "vim undo [<number>]":escape + Text("%(number)du"),
      "vim redo":escape + Text(":redo\n"),
      "vim [buf] close":escape + Text(":bd\n"),
      "vim [buf] close bang":escape + Text(":bd!\n"),
      "<number> go":escape + Text("%(number)dG"),
    }
  extras = [IntegerRef("number", 1, 1000), Dictation("text")]
  defaults = {"text":"", "number":1}

class MacroCommand(MappingRule):
  mapping = {
      "rec ripple <LetterMapping>":Key("q, %(LetterMapping)s"),
      "end ripple":Key("q"),
#      "[<count>] ripple <LetterMapping>":Text("%(count)d@%(LetterMapping)s"),
    }
  extras = [RuleRef(MappingRule(mapping=LETTERS, name="maplet"), name="LetterMapping")]

grammar.add_rule(EasyMotion())
grammar.add_rule(VimCommand())
grammar.add_rule(LustyJuggler())
grammar.add_rule(LustyExplorer())
grammar.add_rule(CommandT())
grammar.add_rule(Fugitive())
grammar.add_rule(MacroCommand())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
