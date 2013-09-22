from dragonfly import MappingRule, RuleRef, CompoundRule, Alternative
from proxy_nicknames import Text

from verbal_emacs.common import NumericDelegateRule, ruleDigitalInteger
from verbal_emacs.motions import ruleMotion

_OPERATORS = {
    "relo":"",
    "dell":"d",
    "chaos":"c",
    "yank":"y",
    "swap case":"g~",
    "uppercase":"gU",
    "lowercase":"gu",
    "external filter":"!",
    "external format":"=",
    "format text":"gq",
    "rotate thirteen":"g?",
    "indent left":"<",
    "indent right":">",
    "define fold":"zf",
  }

class PrimitiveOperator(MappingRule):
  mapping = dict((key, Text(value)) for (key, value) in _OPERATORS.iteritems())

rulePrimitiveOperator = RuleRef(PrimitiveOperator(), name="PrimitiveOperator")

class Operator(NumericDelegateRule):
  spec = "[<count>] <PrimitiveOperator>"
  extras = [ruleDigitalInteger[3],
            rulePrimitiveOperator]
ruleOperator = RuleRef(Operator(), name="Operator")

class OperatorApplicationMotion(CompoundRule):
  spec = "[<Operator>] <Motion>"
  extras = [ruleOperator, ruleMotion]

  def value(self, node):
    children = node.children[0].children[0].children
    return_value = children[1].value()
    if children[0].value() is not None:
      return_value = children[0].value() + return_value
    return return_value
ruleOperatorApplicationMotion = RuleRef(OperatorApplicationMotion(), name="OperatorApplicationMotion")

class OperatorSelfApplication(MappingRule):
  mapping = dict(("%s [<count>] %s" % (key, key), Text("%s%%(count)d%s" % (value, value)))
                 for (key, value) in _OPERATORS.iteritems())
  extras = [ruleDigitalInteger[3]]
  defaults = {"count":1}
ruleOperatorSelfApplication = RuleRef(OperatorSelfApplication(), name="OperatorSelfApplication")

ruleOperatorApplication = Alternative([ruleOperatorApplicationMotion,
                                       ruleOperatorSelfApplication],
                                      name="OperatorApplication")
