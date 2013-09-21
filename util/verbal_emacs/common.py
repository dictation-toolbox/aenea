from dragonfly import CompoundRule, MappingRule, RuleRef
from proxy_nicknames import Text
from aenea import DigitalInteger
from raul import LETTERS

class NumericDelegateRule(CompoundRule):
  def value(self, node):
    delegates = node.children[0].children[0].children
    value = delegates[-1].value()
    if delegates[0].value() is not None:
      value = Text("%s" % delegates[0].value()) + value
    return value

class _DigitalIntegerFetcher(object):
  def __init__(self):
    self.cached = {}

  def __getitem__(self, length):
    if length not in self.cached:
      self.cached[length] = DigitalInteger("count", 1, length)
    return self.cached[length]
ruleDigitalInteger = _DigitalIntegerFetcher()

class LetterMapping(MappingRule):
  mapping = LETTERS
ruleLetterMapping = RuleRef(LetterMapping(), name="LetterMapping")
