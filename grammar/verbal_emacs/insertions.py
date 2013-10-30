from dragonfly import (
    MappingRule,
    CompoundRule,
    Dictation,
    RuleRef,
    Repetition,
    Alternative
  )

from aenea import Nested
from proxy_nicknames import Key, Text
from raul import DIGITS, LETTERS

from verbal_emacs.identifiers import ruleIdentifierInsertion, ruleInsertModeEntry
from verbal_emacs.common import ruleDigitalInteger

class KeyInsertion(MappingRule):
  mapping = {
    "ace [<count>]":        Key("space:%(count)d"),
    "tab [<count>]":        Key("tab:%(count)d"),
    "slap [<count>]":       Key("enter:%(count)d"),
    "chuck [<count>]":      Key("del:%(count)d"),
    "scratch [<count>]":    Key("backspace:%(count)d"),
    "ack":                  Key("escape"),
  }
  extras = [ruleDigitalInteger[3]]
  defaults = {"count":1}
ruleKeyInsertion = RuleRef(KeyInsertion(), name="KeyInsertion")

class SymbolInsertion(MappingRule):
  mapping = {
    "amp":        Key("ampersand"),
    "star":       Key("asterisk"),
    "at sign":    Key("at"),
    "back ash":   Key("backslash"),
    "backtick":   Key("backtick"),
    "bar":        Key("bar"),
    "hat":        Key("caret"),
    "yeah":       Key("colon"),
    "drip":       Key("comma"),
    "dollar":     Key("dollar"),
    "dot":        Key("dot"),
    "quote":      Key("dquote"),
    "eek":        Key("equal"),
    "bang":       Key("exclamation"),
    "pound":      Key("hash"),
    "hyph":       Key("hyphen"),
    "percent":    Key("percent"),
    "cross":      Key("plus"),
    "quest":      Key("question"),
    "ash":        Key("slash"),
    "smote":      Key("squote"),
    "tilde":      Key("tilde"),
    "rail":       Key("underscore"),
    "push":       Key("lparen"),
    "pop":        Key("rparen"),
    "semi":       Key("semicolon"),
  }
ruleSymbolInsertion = RuleRef(SymbolInsertion(), name="SymbolInsertion")

class NestedInsertion(MappingRule):
  mapping = {
    "circle":           Nested("()"),
    "square":           Nested("[]"),
    "box":              Nested("{}"),
    "diamond":          Nested("<>"),
    "nest quote":       Nested("\"\""),
    "nest smote":       Nested("''"),
  }
ruleNestedInsertion = RuleRef(NestedInsertion(), name="NestedInsertion")

class SpellingInsertion(MappingRule):
  mapping = dict(("dig " + key, value) for (key, value) in DIGITS.iteritems())
  mapping.update(LETTERS)

  def value(self, node):
    return Text(MappingRule.value(self, node))
ruleSpellingInsertion = RuleRef(SpellingInsertion(), name="SpellingInsertion")

class PythonInsertion(MappingRule):
  mapping = {
    "private":          Nested("____"),
    "dub dock string":  Nested('""""""'),
    "dock string":      Nested("''''''"),
    "values":           Text("values"),
    "get atter":        Text("getattr"),
    "set atter":        Text("setattr"),
    "has atter":        Text("hasattr"),
    "print":            Text("print"),
    "if test":          Text("if "),
    "elif":             Text("elif "),
    "else":             Text("else"),
    "deaf":             Text("def "),
    "log and":          Text("and "),
    "log or":           Text("or "),
    "log not":          Text("not "),
    "not":              Text("not "),
    "for loop":         Text("for "),
    "bit ore":          Text("| "),
    "bit and":          Text("& "),
    "bit ex or":        Text("^ "),
    "times":            Text("* "),
    "divided":          Text("/ "),
    "plus":             Text("+ "),
    "minus":            Text("- "),
    "plus equal":       Text("+= "),
    "minus equal":      Text("-= "),
    "times equal":      Text("*= "),
    "divided equal":    Text("/= "),
    "mod equal":        Text("%%= "),
    "as name":          Text("as "),
    "in":               Text("in "),
    "is":               Text("is "),
    "while":            Text("while "),
    "class":            Text("class "),
    "with context":     Text("with "),
    "import":           Text("import "),
    "raise":            Text("raise "),
    "return":           Text("return "),
    "none":             Text("None"),
    "try":              Text("try"),
    "except":           Text("except"),
    "lambda":           Text("lambda "),
    "assert":           Text("assert "),
    "delete":           Text("del "),
    "assign":           Text("= "),
    "compare eek":      Text("== "),
    "compare not eek":  Text("!= "),
    "compare greater":  Text("> "),
    "compare less":     Text("< "),
    "compare geck":     Text(">= "),
    "compare lack":     Text("<= "),
  }
rulePythonInsertion = RuleRef(PythonInsertion(), name="PythonInsertion")

class PrimitiveInsertion(CompoundRule):
  spec = "<insertion>"
  extras = [Alternative([
      ruleKeyInsertion,
      ruleSymbolInsertion,
      ruleIdentifierInsertion,
      ruleNestedInsertion,
      rulePythonInsertion,
      ruleSpellingInsertion,
    ], name="insertion")]

  def value(self, node):
    children = node.children[0].children[0].children
    return children[0].value()
rulePrimitiveInsertion = RuleRef(PrimitiveInsertion(), name="PrimitiveInsertion")

class PrimitiveInsertionRepetition(CompoundRule):
  spec = "<PrimitiveInsertion> [ parrot <count> ]"
  extras = [rulePrimitiveInsertion, ruleDigitalInteger[3]]

  def value(self, node):
    children = node.children[0].children[0].children
    value = children[0].value() * (children[1].value()[1] if children[1].value() else 1)
    return value
rulePrimitiveInsertionRepetition = RuleRef(PrimitiveInsertionRepetition(), name="PrimitiveInsertionRepetition")

class Insertion(CompoundRule):
  spec = "[<InsertModeEntry>] <PrimitiveInsertionRepetition>"
  extras = [rulePrimitiveInsertionRepetition, ruleInsertModeEntry]

  def value(self, node):
    children = node.children[0].children[0].children
    return [("i", (children[0].value(), children[1].value()))]
ruleInsertion = RuleRef(Insertion(), name="Insertion")
