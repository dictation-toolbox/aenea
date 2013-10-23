from dragonfly import Alternative, CompoundRule, MappingRule, RuleRef
from proxy_nicknames import Text, Key
from raul import LETTERS

from verbal_emacs.common import (
    NumericDelegateRule,
    ruleDigitalInteger,
    ruleLetterMapping
  )
from verbal_emacs.config import LEADER

class PrimitiveMotion(MappingRule):
  mapping = {
    "up":Text("k"),
    "down":Text("j"),
    "left":Text("h"),
    "right":Text("l"),

    "lope":Text("b"),
    "yope":Text("w"),
    "elope":Text("ge"),
    "iyope":Text("e"),

    "lopert":Text("B"),
    "yopert":Text("W"),
    "elopert":Text("gE"),
    "eyopert":Text("E"),

    "apla":Text("{"),
    "anla":Text("}"),
    "sapla":Text("("),
    "sanla":Text(")"),

    "care":Text("^"),
    "hard care":Text("0"),
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

    # EasyMotion
    "easy lope":Key("%s:2, b" % LEADER),
    "easy yope":Key("%s:2, w" % LEADER),
    "easy elope":Key("%s:2, g, e" % LEADER),
    "easy iyope":Key("%s:2, e" % LEADER),

    "easy lopert":Key("%s:2, B" % LEADER),
    "easy yopert":Key("%s:2, W" % LEADER),
    "easy elopert":Key("%s:2, g, E" % LEADER),
    "easy eyopert":Key("%s:2, E" % LEADER),
  }

  for (spoken_object, command_object) in (("(lope | yope)", "w"),
                                          ("(lopert | yopert)", "W")):
    for (spoken_modifier, command_modifier) in (("inner", "i"),
                                                ("outer", "a")):
      mapping["%s %s" % (spoken_modifier, spoken_object)] = Text(command_modifier + command_object)
rulePrimitiveMotion = RuleRef(PrimitiveMotion(), name="PrimitiveMotion")

class UncountedMotion(MappingRule):
  mapping = {
    "tect":Text("%%"),
    "matu":Text("M"),
  }
ruleUncountedMotion = RuleRef(UncountedMotion(), name="UncountedMotion")

class MotionParameterMotion(MappingRule):
  mapping = {
    "phytic":"f",
    "fitton":"F",
    "pre phytic":"t",
    "pre fitton":"T",
  }
ruleMotionParameterMotion = RuleRef(MotionParameterMotion(), name="MotionParameterMotion")

class ParameterizedMotion(CompoundRule):
  spec = "<MotionParameterMotion> <LetterMapping>"
  extras = [ruleLetterMapping, ruleMotionParameterMotion]

  def value(self, node):
    children = node.children[0].children[0].children
    return Text(children[0].value() + children[1].value())
ruleParameterizedMotion = RuleRef(ParameterizedMotion(), name="ParameterizedMotion")

class CountedMotion(NumericDelegateRule):
  spec = "[<count>] <motion>"
  extras = [ruleDigitalInteger[3],
            Alternative([
                rulePrimitiveMotion,
                ruleParameterizedMotion], name="motion")]
ruleCountedMotion = RuleRef(CountedMotion(), name="CountedMotion")

class Motion(CompoundRule):
  spec = "<motion>"
  extras = [Alternative([ruleCountedMotion, ruleUncountedMotion], name="motion")]
  def value(self, node):
    return node.children[0].children[0].children[0].value()
ruleMotion = RuleRef(Motion(), name="Motion")
