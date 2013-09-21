from dragonfly import MappingRule, Alternative, RuleRef
from aenea import DigitalInteger
from proxy_nicknames import Text, Key

from verbal_emacs.common import NumericDelegateRule
from verbal_emacs.operators import OperatorApplication

class PrimitiveCommand(MappingRule):
  mapping = {
    "vim scratch":Key("X"),
    "vim chuck":Key("x"),
    "vim undo":Key("u"),
    "plap":Key("P"),
    "plop":Key("p"),
    "megaditto":Text("."),
  }

class Command(NumericDelegateRule):
  spec = "[<count>] <command>"
  extras = [Alternative([RuleRef(OperatorApplication()),
                         RuleRef(PrimitiveCommand()),
                        ], name="command"),
            DigitalInteger("count", 1, 4)]

  def value(self, node):
    rval = "c", NumericDelegateRule.value(self, node)
    return rval
