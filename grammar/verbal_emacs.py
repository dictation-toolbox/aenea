# Dragonfly module for controlling vim on Linux modelessly. Intended as eventual
# replacement for the vim module.

# For those who to deem vim without modes heresy, i implore you to consider the
# harsh realities of controlling it by voice. The magic of modal editing comes
# from the limited number of keys on the keyboard. while voice lacks this
# restriction, it imposes a much greater latency, making frequent manual switching
# extremely inconvenient, particularly when you consider the higher error rate.

try:
    import pkg_resources

    pkg_resources.require("dragonfly >= 0.6.5beta1.dev-r99")
except ImportError:
    pass

import aenea
from aenea import *
import raul

from dragonfly import *
from proxy_nicknames import *

LEADER_KEY = "comma"
LINE_COMMAND_KEY = "colon"

leader = Key(LEADER_KEY)
line_command_key = Key(LINE_COMMAND_KEY)
escape = Key("Escape")
escape_leader = escape + Pause("30") + leader

vim_context = aenea.global_context & AppRegexContext(name="(?i)^.* vim$") & AppRegexContext(name="(?i)^.*prototypeve.*$")

command_t_context = AppRegexContext(name="^GoToFile.*$") & vim_context
fugitive_index_context = AppRegexContext(name="^index.*\.git.*$") & vim_context

grammar = Grammar("verbal_emacs", context=vim_context)

class NumericDelegateRule(CompoundRule):
  def value(self, node):
    delegates = node.children[0].children[0].children
    value = delegates[1].value()
    if delegates[0].value() is not None:
      value = Text("%s" % delegates[0].value()) + value
    return value

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
    "dollin ":Text("$"),

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

class PrimitiveOperator(MappingRule):
  mapping = {
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

def format_snakeword(text):
  return text[0][0].upper() + text[0][1:] + ("_" if len(text) > 1 else "") + format_score(text[1:])

def format_score(text):
  return "_".join(text)

def format_camel(text):
  return text[0] + "".join([word[0].upper() + word[1:] for word in text[1:]])

def format_proper(text):
  return "".join(word.capitalize() for word in text)

def format_relpath(text):
  return "/".join(text)

def format_abspath(text):
  return "/" + format_relpath(text)

def format_scoperesolve(text):
  return "::".join(text)

def format_jumble(text):
  return "".join(text)

def format_dotword(text):
  return ".".join(text)

def format_dashword(text):
  return "-".join(text)

def format_natword(text):
  return " ".join(text)

def format_broodingnarrative(text):
  return ""

class IdentifierInsertion(CompoundRule):
  spec = ("[upper | natural] ( proper | camel | rel-path | abs-path | score | "
          "scope-resolve | jumble | dotword | dashword | natword | snakeword | brooding-narrative) [<dictation>]")
  extras = [Dictation(name="dictation")]
  
  def value(self, node):
    words = node.words()

    uppercase = words[0] == "upper"
    lowercase = words[0] != "natural"

    if lowercase:
      words = [word.lower() for word in words]
    if uppercase:
      words = [word.upper() for word in words]

    words = [word.split("\\", 1)[0].replace("-", "") for word in words]
    if words[0].lower() in ("upper", "natural"):
      del words[0]

    function = globals()["format_%s" % words[0].lower()]
    formatted = function(words[1:])

    return Text(formatted)

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

class SpellingInsertion(CompoundRule):
  def value(self, node):
    children = node.children[0].children[0].children
    return Text(children[-1].value())

class LettersInsertion(SpellingInsertion):
  spec = "letters [<letters>]"
  extras = [RuleRef(MappingRule(mapping=raul.LETTERS, name="letter"), name="letters")]

class NumbersInsertion(SpellingInsertion):
  spec = "numbers [<numbers>]"
  extras = [RuleRef(MappingRule(mapping=raul.NUMBERS, name="number"), name="numbers")]

class AlphanumericInsertion(SpellingInsertion):
  spec = "alphanumeric [<alphanumeric>]"
  extras = [RuleRef(MappingRule(mapping=raul.ALPHANUMERIC, name="an"), name="alphanumeric")]

class Operator(NumericDelegateRule):
  spec = "[<count>] <operator>"
  extras = [DigitalInteger("count", 1, 5), RuleRef(PrimitiveOperator(), name="operator")]

class Motion(NumericDelegateRule):
  spec = "[<count>] <motion>"
  extras = [DigitalInteger("count", 1, 5), RuleRef(PrimitiveMotion(), name="motion")]

class OperatorApplication(CompoundRule):
  spec = "[<operator>] <motion>"
  extras = [RuleRef(Operator(name="a"), name="operator"), RuleRef(Motion(name="b"), name="motion")]

  def value(self, node):
    children = node.children[0].children[0].children
    if children[0].value() is not None:
      return children[0].value() + children[1].value()
    else:
      return children[1].value()

class PrimitiveInsertion(CompoundRule):
  spec = "<insertion>"
  extras = [Alternative([
      RuleRef(KeyInsertion()),
      RuleRef(SymbolInsertion()),
      RuleRef(IdentifierInsertion()),
      RuleRef(NestedInsertion()),
      RuleRef(PythonInsertion()),
      RuleRef(LettersInsertion()),
      RuleRef(NumbersInsertion()),
      RuleRef(AlphanumericInsertion()),
    ], name="insertion")]

  def value(self, node):
    children = node.children[0].children[0].children
    return children[0].value()

class PrimitiveInsertionRepetition(CompoundRule):
  spec = "<insertion> [ parrot <repeat> ]"
  extras = [RuleRef(PrimitiveInsertion(), name="insertion"), DigitalInteger("repeat", 1, 3)]

  def value(self, node):
    children = node.children[0].children[0].children
    value = children[0].value() * (children[1].value()[1] if children[1].value() else 1)
    return value

class InsertModeEntry(MappingRule):
  mapping = {
    "inns":Key("i"),
    "syn":Key("a"),
  }

class Insertion(CompoundRule):
  spec = "[<mode_switch>] <insertions>"
  extras = [Repetition(RuleRef(PrimitiveInsertionRepetition()), max=12, name="insertions"),
            RuleRef(InsertModeEntry(), name="mode_switch")]

  def value(self, node):
    children = node.children[0].children[0].children
    accumulate = children[0].value()
    if accumulate is None:
      accumulate = Key("a")
    for child in children[1].value():
      accumulate = accumulate + child
    return accumulate + Key("Escape:2")

class VimCommand(CompoundRule):
  spec = ("<app>")
  extras = [RuleRef(Insertion(), name="app")]

  def _process_recognition(self, node, extras):
    extras["app"].execute()

grammar.add_rule(VimCommand())

#grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
