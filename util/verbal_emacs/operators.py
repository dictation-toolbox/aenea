from dragonfly import MappingRule, RuleRef, CompoundRule
from aenea import DigitalInteger
from proxy_nicknames import Text

from verbal_emacs.common import NumericDelegateRule
from verbal_emacs.motions import ruleMotion

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
rulePrimitiveOperator = RuleRef(PrimitiveOperator(), name="PrimitiveOperator")

class Operator(NumericDelegateRule):
  spec = "[<count>] <PrimitiveOperator>"
  extras = [DigitalInteger("count", 1, 4),
            rulePrimitiveOperator]
ruleOperator = RuleRef(Operator(), name="Operator")

class OperatorApplication(CompoundRule):
  spec = "[<Operator>] <Motion>"
  extras = [ruleOperator, ruleMotion]

  def value(self, node):
    children = node.children[0].children[0].children
    return_value = children[1].value()
    if children[0].value() is not None:
      return_value = children[0].value() + return_value
    return return_value
ruleOperatorApplication = RuleRef(OperatorApplication(), name="OperatorApplication")
