from dragonfly import MappingRule, RuleRef, CompoundRule
from verbal_emacs.motions import Motion
from aenea import DigitalInteger
from verbal_emacs.common import NumericDelegateRule
from proxy_nicknames import Text

class PrimitiveOperator(MappingRule):
  mapping = {
    "relo":Text(""),
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

class Operator(NumericDelegateRule):
  spec = "[<count>] <operator>"
  extras = [DigitalInteger("count", 1, 4),
            RuleRef(PrimitiveOperator(), name="operator")]

class OperatorApplication(CompoundRule):
  spec = "[<operator>] <motion>"
  extras = [RuleRef(Operator(name="a"), name="operator"),
            RuleRef(Motion(name="b"), name="motion")]

  def value(self, node):
    children = node.children[0].children[0].children
    return_value = children[1].value()
    if children[0].value() is not None:
      return_value = children[0].value() + return_value
    return return_value

