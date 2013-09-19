# Dragonfly module for controlling vim on Linux modelessly. Intended as eventual
# replacement for the vim module.

# For those who to deem vim without modes heresy, i implore you to consider the
# harsh realities of controlling it by voice. The magic of modal editing comes
# from the limited number of keys on the keyboard. while voice lacks this
# restriction, it imposes a much greater latency, making frequent manual switching
# extremely inconvenient, particularly when you consider the higher error rate.

try:
    import pkg_resources

    pkg_resources.require("dragonfly >= 0.6.5beta1.dev-r99")
except ImportError:
    pass

import aenea
import raul

from dragonfly import *
from proxy_nicknames import *

LEADER_KEY = "comma"
LINE_COMMAND_KEY = "colon"

leader = Key(LEADER_KEY)
line_command_key = Key(LINE_COMMAND_KEY)
escape = Key("Escape")
escape_leader = escape + Pause("30") + leader

vim_context = aenea.global_context & AppRegexContext(name="(?i)^.* vim$")

command_t_context = AppRegexContext(name="^GoToFile.*$") & vim_context
fugitive_index_context = AppRegexContext(name="^index.*\.git.*$") & vim_context

grammar = Grammar("verbal_emacs", context=vim_context)

class NumericDelegateRule(CompoundRule):
  def value(self, node):
    delegates = node.children[0].children[0].children
    value = delegates[1].value()
    if delegates[0].value() is not None:
      value = Text("%s" % delegates[0].value()) + value
    return value

class PrimitiveMotion(MappingRule):
  mapping = {
    "up":Text("k"),
    "down":Text("j"),
    "left":Text("h"),
    "right":Text("l"),

    "lope":Text("b"),
    "yope":Text("w"),
    "elope":Text("ge"),
    "e yope":Text("e"),

    "lopert":Text("B"),
    "yopert":Text("W"),
    "elopert":Text("gE"),
    "eyopert":Text("E"),
    
    "apla":Text("{"),
    "anla":Text("}"),
    "sapla":Text("("),
    "sanla":Text(")"),

    "care":Text("^"),
    "dollin ":Text("$"),

    "screecare":Text("g^"),
    "screedoll":Text("g$"),

    "scree up":Text("gk"),
    "scree down":Text("gj"),

    "wynac":Text("G"),

    "wynac top":Text("H"),
    "wynac toe":Text("L"),

    # CamelCaseMotion plugin
    "calalope":Text(",b"),
    "calayope":Text(",w"),
    "end calayope":Text(",e"),
    "inner calalope":Text("i,b"),
    "inner calayope":Text("i,w"),
    "inner end calayope":Text("i,e"),
  }

#    "phytic":Text("f"),
#    "fitton":Text("F"),

#    "pre phytic":Text("t"),
#    "pre fitton":Text("T"),
#"tect":Text("%%"),
#"matu":Text("M"),

  for (spoken_object, command_object) in (("(lope | yope)", "w"),
                                          ("(lopert | yopert)", "W")):
    for (spoken_modifier, command_modifier) in (("inner", "i"),
                                                ("outer", "a")):
      mapping["%s %s" % (spoken_modifier, spoken_object)] = Text(command_modifier + command_object)

class PrimitiveOperator(MappingRule):
  mapping = {
    "dell":Text("d"),
    "chaos":Text("c"),
    "yank":Text("y"),
    "swap case":Text("g~"),
    "uppercase":Text("gU"),
    "lowercase":Text("gu"),
    "external filter":Text("!"),
    "external format":Text("="),
    "format text":Text("gq"),
    "rotate thirteen":Text("g?"),
    "indent left":Text("<"),
    "indent right":Text(">"),
    "define fold":Text("zf"),
  }

class KeyInsertion(MappingRule):
  mapping = {"space":Text(" ")}

class Operator(NumericDelegateRule):
  spec = "[<count>] <operator>"
  extras = [IntegerRef("count", 1, 1000), RuleRef(PrimitiveOperator(), name="operator")]

class Motion(NumericDelegateRule):
  spec = "[<count>] <motion>"
  extras = [IntegerRef("count", 1, 1000), RuleRef(PrimitiveMotion(), name="motion")]

class OperatorApplication(CompoundRule):
  spec = "[<operator>] <motion>"
  extras = [RuleRef(Operator(name="a"), name="operator"), RuleRef(Motion(name="b"), name="motion")]

  def value(self, node):
    children = node.children[0].children[0].children
    if children[0].value() is not None:
      return children[0].value() + children[1].value()
    else:
      return children[1].value()

class PrimitiveInsertion(CompoundRule):
  spec = "<insertion>"
  extras = [Alternative([
      RuleRef(KeyInsertion()),
#      SymbolInsertion(),
#      NestInsertion(),
#      VariableInsertion(),
#      PythonInsertion(),
    ], name="insertion")]

  def value(self, node):
    children = node.children[0].children[0].children
    return children[0].value()

class InsertModeEntry(MappingRule):
  mapping = {
    "inns":Key("i"),
    "syn":Key("a"),
  }

class Insertion(CompoundRule):
  spec = "[<mode_switch>] <insertions>"
  extras = [Repetition(RuleRef(PrimitiveInsertion()), max=15, name="insertions"),
            RuleRef(InsertModeEntry(), name="mode_switch")]

  def value(self, node):
    children = node.children[0].children[0].children
    accumulate = children[0].value()
    if accumulate is None:
      accumulate = Key("a")
    for child in children[1].value():
      accumulate = accumulate + child
    return accumulate

class VimCommand(CompoundRule):
  spec = ("<app>")
  extras = [RuleRef(Insertion(), name="app")]

  def _process_recognition(self, node, extras):
    extras["app"].execute()

grammar.add_rule(VimCommand())

#grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
