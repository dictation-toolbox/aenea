from dragonfly import CompoundRule, Dictation, RuleRef, MappingRule
from proxy_nicknames import Text, Key

from verbal_emacs.common import ruleDigitalInteger

class InsertModeEntry(MappingRule):
  mapping = {
    "inns":Key("i"),
    "syn":Key("a"),
    "phyllo":Key("o"),
    "phyhigh":Key("O"),
  }
ruleInsertModeEntry = RuleRef(InsertModeEntry(), name="InsertModeEntry")

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
ruleIdentifierInsertion = RuleRef(IdentifierInsertion(), name="IdentifierInsertion")

class LiteralIdentifierInsertion(CompoundRule):
  spec = "[<InsertModeEntry>] literal <IdentifierInsertion>"
  extras = [ruleIdentifierInsertion, ruleInsertModeEntry]

  def value(self, node):
    children = node.children[0].children[0].children
    return ("i", (children[0].value(), children[2].value()))
ruleLiteralIdentifierInsertion = RuleRef(LiteralIdentifierInsertion(), name="LiteralIdentifierInsertion")
