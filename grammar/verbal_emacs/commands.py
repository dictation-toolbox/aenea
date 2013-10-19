from dragonfly import MappingRule, Alternative, RuleRef, CompoundRule
from proxy_nicknames import Text, Key, NoAction

from verbal_emacs.common import NumericDelegateRule, ruleDigitalInteger, ruleLetterMapping
from verbal_emacs.operators import ruleOperatorApplication

class PrimitiveCommand(MappingRule):
  mapping = {
    "vim scratch":Key("X"),
    "vim chuck":Key("x"),
    "vim undo":Key("u"),
    "plap":Key("P"),
    "plop":Key("p"),
    "ditto":Text("."),
  }
rulePrimitiveCommand = RuleRef(PrimitiveCommand(), name="PrimitiveCommand")

class Command(CompoundRule):
  spec = "[<count>] [reg <LetterMapping>] <command>"
  extras = [Alternative([ruleOperatorApplication,
                         rulePrimitiveCommand,
                        ], name="command"),
            ruleDigitalInteger[3],
            ruleLetterMapping]

  def value(self, node):
    delegates = node.children[0].children[0].children
    value = delegates[-1].value()
    prefix = ""
    if delegates[0].value() is not None:
      prefix += str(delegates[0].value())
    if delegates[1].value() is not None:
      prefix += '"' + delegates[1].value()[1]
    if prefix:
      value = Text(prefix) + value
    # TODO: ugly hack; should fix the grammar or generalize.
    if "chaos" in zip(*node.results)[0]:
      return [("c", value), ("i", (NoAction(),) * 2)]
    else:
      return [("c", value)]
ruleCommand = RuleRef(Command(), name="Command")
