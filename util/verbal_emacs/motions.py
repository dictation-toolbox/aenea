from dragonfly import MappingRule, CompoundRule, Alternative, RuleRef
from proxy_nicknames import Text, Key
from aenea import DigitalInteger
from raul import LETTERS

from verbal_emacs.common import NumericDelegateRule

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
    "doll":Text("$"),

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
rulePrimitiveMotion = PrimitiveMotion()

class ParameterizedMotion(CompoundRule):
  spec = "<motion> <parameter>"
  extras = [RuleRef(MappingRule(mapping=LETTERS, name="pra"), name="parameter"),
            RuleRef(MappingRule(name="prb", mapping={
                      "find char":"f",
                      "find char rev":"F",
                      "till char":"t",
                      "till char rev":"T"}), name="motion")]

  def value(self, node):
    children = node.children[0].children[0].children
    return Text(children[0].value() + children[1].value())
ruleParameterizedMotion = ParameterizedMotion()

class Motion(NumericDelegateRule):
  spec = "[<count>] <motion>"
  extras = [DigitalInteger("count", 1, 4),
            Alternative([
                RuleRef(rulePrimitiveMotion),
                RuleRef(ruleParameterizedMotion)], name="motion")]
ruleMotion = Motion()
