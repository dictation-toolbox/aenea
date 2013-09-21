from dragonfly import MappingRule, CompoundRule, Dictation, RuleRef, Repetition, Alternative
from aenea import DigitalInteger, Nested
from proxy_nicknames import Text, Key
from raul import DIGITS, LETTERS

from verbal_emacs.identifiers import ruleIdentifierInsertion, ruleInsertModeEntry

class KeyInsertion(MappingRule):
  mapping = {
    "ace [<n>]":        Key("space:%(n)d"),
    "tab [<n>]":        Key("Tab:%(n)d"),
    "slap [<n>]":       Key("Return:%(n)d"),
    "chuck [<n>]":      Key("Delete:%(n)d"),
    "scratch [<n>]":    Key("BackSpace:%(n)d"),
    "ack":              Key("Escape"),
  }
  extras = [DigitalInteger("n", 1, 5)]
  defaults = {"n":1}
ruleKeyInsertion = RuleRef(KeyInsertion(), name="KeyInsertion")

class SymbolInsertion(MappingRule):
  mapping = {
    "amp":        Key("ampersand"),
    "star":       Key("asterisk"),
    "at sign":    Key("at"),
    "back ash":   Key("backslash"),
    "backtick":   Key("grave"),
    "bar":        Key("bar"),
    "hat":        Key("asciicircum"),
    "yeah":       Key("colon"),
    "drip":       Key("comma"),
    "dollar":     Key("dollar"),
    "dot":        Key("period"),
    "quote":      Key("quotedbl"),
    "eek":        Key("equal"),
    "bang":       Key("exclam"),
    "pound":      Key("numbersign"),
    "hyph":       Key("minus"),
    "percent":    Key("percent"),
    "cross":      Key("plus"),
    "quest":      Key("question"),
    "ash":        Key("slash"),
    "smote":      Key("apostrophe"),
    "tilde":      Key("asciitilde"),
    "rail":       Key("underscore"),
    "push":       Key("parenleft"),
    "pop":        Key("parenright"),
  }
ruleSymbolInsertion = RuleRef(SymbolInsertion(), name="SymbolInsertion")

class NestedInsertion(MappingRule):
  mapping = {
    "circle":           Nested("()"),
    "square":           Nested("[]"),
    "box":              Nested("[]"),
    "diamond":          Nested("<>"),
    "hexy":             Nested("{}"),
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
  spec = "<PrimitiveInsertion> [ parrot <repeat> ]"
  extras = [rulePrimitiveInsertion, DigitalInteger("repeat", 1, 3)]

  def value(self, node):
    children = node.children[0].children[0].children
    value = children[0].value() * (children[1].value()[1] if children[1].value() else 1)
    return value
rulePrimitiveInsertionRepetition = RuleRef(PrimitiveInsertionRepetition(), name="PrimitiveInsertionRepetition")

class Insertion(CompoundRule):
  spec = "[<InsertModeEntry>] <insertions>"
  extras = [Repetition(rulePrimitiveInsertionRepetition, max=None, name="insertions"),
            ruleInsertModeEntry]

  def value(self, node):
    children = node.children[0].children[0].children
    accumulate = children[1].value()[0]
    for child in children[1].value()[1:]:
      accumulate = accumulate + child
    return ("i", (children[0].value(), accumulate))
ruleInsertion = RuleRef(Insertion(), name="Insertion")