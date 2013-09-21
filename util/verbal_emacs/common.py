from dragonfly import CompoundRule
from proxy_nicknames import Text

class NumericDelegateRule(CompoundRule):
  def value(self, node):
    delegates = node.children[0].children[0].children
    value = delegates[1].value()
    if delegates[0].value() is not None:
      value = Text("%s" % delegates[0].value()) + value
    return value
