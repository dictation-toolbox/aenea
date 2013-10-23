# This file is a command-module for Dragonfly.
#
# (based on the multiedit module from dragonfly-modules project)
# (heavily modified)
# (the original copyright notice is reproduced below)
#
# (c) Copyright 2008 by Christo Butcher
# Licensed under the LGPL, see <http://www.gnu.org/licenses/>
#
# includes two sets of bindings: generic and vim. the latter is quite primitive
# and revolves around to spending all your time in insert mode. See verbal_emacs
# for my second (and in my opinion better and more faithful) attempt at vim
# bindings.
#
# Can be used either with proxy_actions (eg, to ship actions to another computer
# like most of aenea), or regularly in Windows like most Dragonfly modules --
# just set PLATFORM to "windows" for that. (line ~35)
#
# Requires raul.py to be in the Natlink dir to work correctly. Future versions
# may clean that up. No other aenea dependencies when operating in regular
# Windows mode. (Don't forget, it has to be called _multiedit, so if you're
# not using the reloadconfig module you'll need to rename manually.)

ENABLE_ECLIPSE_COMMANDS = True
ENABLE_VIM_COMMANDS = True
ENABLE_PYTHON_COMMANDS = True

import raul

try:
  import pkg_resources

  pkg_resources.require("dragonfly >= 0.6.5beta1.dev-r99")
except ImportError:
  pass

from dragonfly import (
    Alternative,
    AppContext,
    CompoundRule,
    Dictation,
    Grammar,
    IntegerRef,
    Key,
    Literal,
    MappingRule,
    Repetition,
    RuleRef,
    Sequence,
    Text,
  )

try:
  from config import PLATFORM
except ImportError:
  PLATFORM = "windows"

if PLATFORM == "proxy":
  import aenea
  from proxy_nicknames import *
  vim_context = AppContext(match="regex", title=".*VIM.*")
  disable_context = vim_context
  global_context = aenea.global_context
else:
  vim_context = AppContext(title="VIM")
  disable_context = ~AppContext(title="")
  global_context = AppContext(title="")

#---------------------------------------------------------------------------
# Set up this module's configuration.

def Nested(command):
  return Text(command) + Key("left:%i" % (len(command) / 2))

command_table = {
  # Spoken-form        normal command              VIM (None if same)

  #### Cursor manipulation
  "up [<n>]":(         Key("up:%(n)d"),            Key("escape") + Text("%(n)dki") ),
  "down [<n>]":(       Key("down:%(n)d"),          Key("escape") + Text("%(n)dji") ),
  "left [<n>]":(       Key("left:%(n)d"),          None),
  "right [<n>]":(      Key("right:%(n)d"),         None),

  "gope [<n>]":(       Key("pgup:%(n)d"),          None),
  "drop [<n>]":(       Key("pgdown:%(n)d"),        None),

  "lope [<n>]":(       Key("c-left:%(n)d"),        Key("escape") + Text("%(n)dbi") ),
  "yope [<n>]":(       Key("c-right:%(n)d"),       Key("escape") + Text("%(n)dwwi") ),

  "care":(             Key("home"),                None),
  "doll":(             Key("end"),                 None),

  "file top":(         Key("c-home"),              Key("escape, 1, s-g, i") ),
  "file toe":(         Key("c-end"),               Key("escape, s-g, i") ),

  #### Various keys
  "ace [<n>]":(        Key("space:%(n)d"),         None),
  "act":(              Key("escape"),              None),
  "chuck [<n>]":(      Key("del:%(n)d"),           None),
  "scratch [<n>]":(    Key("backspace:%(n)d"),     None),
  "slap [<n>]":(       Key("enter:%(n)d"),         None),
  "tab [<n>]":(        Key("tab:%(n)d"),           None),

  #### Symbols
  "amp [<n>]":(        Key("ampersand:%(n)d"),     None),
  "ash [<n>]":(        Key("slash:%(n)d"),         None),
  "at sign [<n>]":(    Key("at:%(n)d"),            None),
  "back ash [<n>]":(   Key("backslash:%(n)d"),     None),
  "backtick [<n>]":(   Key("backtick:%(n)d"),      None),
  "bang [<n>]":(       Key("exclamation:%(n)d"),   None),
  "bar [<n>]":(        Key("bar:%(n)d"),           None),
  "cross [<n>]":(      Key("plus:%(n)d"),          None),
  "dollar [<n>]":(     Key("dollar:%(n)d"),        None),
  "dot [<n>]":(        Key("dot:%(n)d"),           None),
  "drip [<n>]":(       Key("comma:%(n)d"),         None),
  "eek [<n>]":(        Key("equal:%(n)d"),         None),
  "hat [<n>]":(        Key("caret:%(n)d"),         None),
  "hyph [<n>]":(       Key("hyphen:%(n)d"),        None),
  "percent [<n>]":(    Key("percent:%(n)d"),       None),
  "pop [<n>]":(        Key("rparen:%(n)d"),        None),
  "pound [<n>]":(      Key("hash:%(n)d"),          None),
  "push [<n>]":(       Key("lparen:%(n)d"),        None),
  "quest [<n>]":(      Key("question:%(n)d"),      None),
  "quote [<n>]":(      Key("dquote:%(n)d"),        None),
  "rail [<n>]":(       Key("underscore:%(n)d"),    None),
  "semi [<n>]":(       Key("semicolon:%(n)d"),     None),
  "smote [<n>]":(      Key("squote:%(n)d"),        None),
  "star [<n>]":(       Key("asterisk:%(n)d"),      None),
  "tilde [<n>]":(      Key("tilde:%(n)d"),         None),
  "yeah [<n>]":(       Key("colon:%(n)d"),         None),

  #### Nested
  "box":(              Nested("{}"),               None),
  "circle":(           Nested("()"),               None),
  "diamond":(          Nested("<>"),               None),
  "nest quote":(       Nested("\"\""),             None),
  "nest smote":(       Nested("''"),               None),
  "square":(           Nested("[]"),               None),

  # Spoken-form      normal command       VIM (can set to None if same as normal)

  #### Lines
  "line down [<n>]":(Key("home:2, shift:down, end:2, shift:up, c-x, del, down:%(n)d, home:2, enter, up, c-v"),
                                          Key("escape") + Text("dd%(n)dj") + Key("home:2, 1, P, i") ),
  "lineup [<n>]":(   Key("home:2, shift:down, end:2, shift:up, c-x, del, up:%(n)d, home:2, enter, up, c-v"),
                                          Key("escape") + Text("dd%(n)dk") + Key("home:2, 1, P, i") ),
  "nab [<n>]":(      Key("home:2, shift:down, down:%(n)d, up, end:2, shift:up, c-c, end:2"),
                                          Key("escape") + Text("y%(n)dyi") ),
  "plop [<n>]":(     Key("c-v:%(n)d"),
                                          Key("escape, dollar") + Text("%(n)dpi") ),
  "squishy [<n>]":(  Key("end:2, del, space"),
                                          Key("escape") + Text("%(n)dJi") ),
  "strip":(          Key("s-end:2, del"),
                                          Key("escape, l, d, dollar, a") ),
  "striss":(         Key("s-home:2, del"),
                                          Key("escape, l, d, caret, i") ),
  "trance [<n>]":(   Key("home:2, shift:down, down:%(n)d, up, end:2, shift:up, c-c, end:2, enter, c-v"),
                                          Key("escape") + Text("y%(n)dy%(n)djkpi") ),
  "wipe [<n>]":(     Key("home:2, shift:down, down:%(n)d, up, end:2, del, shift:up, backspace"),
                                          Key("escape") + Text("d%(n)ddi") ),

  #### Words
  "bump [<n>]":(     Key("cs-right:%(n)d, del"),
                                          Key("escape") + Text("d%(n)dwi")),
  "whack [<n>]":(    Key("cs-left:%(n)d, del"),
                                          Key("escape") + Text("d%(n)dbi")),
  }

# VIM only commands
vim_command_table = {
  # Spoken-form                VIM (can set to None if same as normal)
  "squishy space [<n>]":       Key("escape") + Text("%(n)dgJi"),

  "slowly up [<n>]":           Key("up:%(n)d"),
  "slowly down [<n>]":         Key("down:%(n)d"),
  "slowly left [<n>]":         Key("left:%(n)d"),
  "slowly right [<n>]":        Key("right:%(n)d"),
  }

# Python specific
python_command_table = {
  # Spoken-form        normal command              VIM (can set to None if same as normal)
  "private":(          Nested("____"),             None),
  "dub dock string":(  Nested('""""""'),           None),
  "dock string":(      Nested("''''''"),           None),
  "values":(           Text("values"),             None),
  "get atter":(        Text("getattr"),            None),
  "set atter":(        Text("setattr"),            None),
  "has atter":(        Text("hasattr"),            None),
  "print":(            Text("print"),              None),
  "if test":(          Text("if "),                None),
  "elif":(             Text("elif "),              None),
  "else":(             Text("else"),               None),
  "deaf":(             Text("def "),               None),
  "log and":(          Text("and "),               None),
  "log or":(           Text("or "),                None),
  "log not":(          Text("not "),               None),
  "not":(              Text("not "),               None),
  "for loop":(         Text("for "),               None),
  "bit ore":(          Text("| "),                 None),
  "bit and":(          Text("& "),                 None),
  "bit ex or":(        Text("^ "),                 None),
  "times":(            Text("* "),                 None),
  "divided":(          Text("/ "),                 None),
  "plus":(             Text("+ "),                 None),
  "minus":(            Text("- "),                 None),
  "plus equal":(       Text("+= "),                None),
  "minus equal":(      Text("-= "),                None),
  "times equal":(      Text("*= "),                None),
  "divided equal":(    Text("/= "),                None),
  "mod equal":(        Text("%%= "),               None),
  "as name":(          Text("as "),                None),
  "in":(               Text("in "),                None),
  "is":(               Text("is "),                None),
  "while":(            Text("while "),             None),
  "class":(            Text("class "),             None),
  "with context":(     Text("with "),              None),
  "import":(           Text("import "),            None),
  "raise":(            Text("raise "),             None),
  "return":(           Text("return "),            None),
  "none":(             Text("None"),               None),
  "try":(              Text("try"),                None),
  "except":(           Text("except"),             None),
  "lambda":(           Text("lambda "),            None),
  "assert":(           Text("assert "),            None),
  "delete":(           Text("del "),               None),
  "assign":(           Text("= "),                 None),
  "compare eek":(      Text("== "),                None),
  "compare not eek":(  Text("!= "),                None),
  "compare greater":(  Text("> "),                 None),
  "compare less":(     Text("< "),                 None),
  "compare geck":(     Text(">= "),                None),
  "compare lack":(     Text("<= "),                None),
  }

# Eclipse only commands
eclipse_command_table = {
  # Spoken-form        normal command              VIM (can set to None if same as normal)
  "save file":(        Key("c-s"),                 None),
  "save all":(         Key("a-s"),                 None),
  "popup":(            Key("apps"),                None),
  "close tab":(        Key("c-w"),                 None),
  "close all tabs":(   Key("cs-w"),                None),
  "move tab left [<n>]":(Key("c-pgup:%(n)d"),      None),
  "move tab right [<n>]":(Key("c-pgdown:%(n)d"),   None),
  "cut that":(         Key("c-x"),                 None),
  "copy that":(        Key("c-c"),                 None),
  "who calls":(        Key("ca-h"),                None),
  "link":(             Key("f3"),                  None),
  "refresh":(          Key("f5"),                  None),
  "hierarchy":(        Key("c-t"),                 None),
  "open type":(        Key("cs-t"),                None),
  "open resource":(    Key("ca-r"),                None),
  "rename":(           Key("cs-r"),                None),
  "correct":(          Key("c-1"),                 None),
  "organize imports":( Key("cs-o"),                None),
  "new java docs":(    Key("sa-j"),                None),
  "find":(             Key("c-f"),                 None),
  "go home":(          Key("home:2"),              None),
  "go end":(           Key("end:2"),               None),
  "pair":(             Key("cs-p"),                None),
  "chirp":(            Key("c-q"),                 None),
  "choices":(          Key("c-space"),             None),
  "complete [<n>]":(   Key("a-slash:%(n)d"),       None),
  "undo [<n>]":(       Key("c-z:%(n)d"),           None),
  "redo [<n>]":(       Key("c-y:%(n)d"),           None),
  "spike [<n>]":(      Key("sc-up:%(n)d"),         None),
  "mud [<n>]":(        Key("sc-down:%(n)d"),       None),
  "block [<n>]":(      Key("sa-w:%(n)d"),          None),
  "shrink [<n>]":(     Key("sa-s:%(n)d"),          None),
  "squirt [<n>]":(     Key("sa-a:%(n)d"),          None),
  "glide [<n>]":(      Key("a-d:%(n)d"),           None),
  "slip [<n>]":(       Key("a-a:%(n)d"),           None),
  "slug [<n>]":(       Key("c-dot:%(n)d"),         None),
  "snail [<n>]":(      Key("c-comma:%(n)d"),       None),
  "stint [<n>]":(      Key("f5:%(n)d"),            None),
  "stover [<n>]":(     Key("f6:%(n)d"),            None),
  "stout [<n>]":(      Key("f7:%(n)d"),            None),
  "step continue":(    Key("f8"),                  None),
  "perspective [<n>]":(Key("ctrl:down, f8:%(n)d, ctrl:up"), None),
  "view [<n>]":(       Key("ctrl:down, f7:%(n)d, ctrl:up"), None),
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

def format_sentence(text):
  return " ".join([text[0].capitalize()] + text[1:])

class FormatRule(CompoundRule):
  spec = ("[upper | natural] ( proper | camel | rel-path | abs-path | score | sentence | "
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

# Set up optional rules (VIM, Eclipse, etc).
tables = []
if ENABLE_ECLIPSE_COMMANDS:
  tables.append(eclipse_command_table)
if ENABLE_PYTHON_COMMANDS:
  tables.append(python_command_table)
tables.append(command_table)
for table in tables:
  for (key, (command, vim_command)) in table.iteritems():
    if vim_command is None:
      table[key] = (command, command)

#---------------------------------------------------------------------------
# Here we define the keystroke rule.

# This rule maps spoken-forms to actions.  Some of these 
#  include special elements like the number with name "n" 
#  or the dictation with name "text".  This rule is not 
#  exported, but is referenced by other elements later on.
#  It is derived from MappingRule, so that its "value" when 
#  processing a recognition will be the right side of the 
#  mapping: an action.
# Note that this rule does not execute these actions, it
#  simply returns them when it's value() method is called.
#  For example "up 4" will give the value Key("up:4").
# More information about Key() actions can be found here:
#  http://dragonfly.googlecode.com/svn/trunk/dragonfly/documentation/actionkey.html
class KeystrokeRule(MappingRule):
  exported = False

  extras = [
    IntegerRef("n", 1, 100),
    Dictation("text"),
    Dictation("text2"),
    ]
  defaults = {
    "n": 1,
    }

#---------------------------------------------------------------------------
# Here we create an element which is the sequence of keystrokes.

# First we create an element that references the keystroke rule.
#  Note: when processing a recognition, the *value* of this element
#  will be the value of the referenced rule: an action.

for table in tables:
  command_table.update(table)

mapping = dict((key, value[0]) for (key, value) in command_table.iteritems())
vim_mapping = dict((key, value[1]) for (key, value) in command_table.iteritems())
vim_mapping.update(vim_command_table)

format_rule = RuleRef(name="format_rule", rule=FormatRule(name="i"))
alternatives = [
      RuleRef(rule=KeystrokeRule(mapping=mapping, name="c")),
      format_rule,
    ]

vim_alternatives = [
      RuleRef(rule=KeystrokeRule(mapping=vim_mapping, name="e")),
      format_rule,
    ]

single_action = Alternative(alternatives)
vim_single_action = Alternative(vim_alternatives)

# Can only be used as the last element
alphabet_mapping = dict((key, Text(value))
                        for (key, value) in raul.LETTERS.iteritems())
numbers_mapping = dict((key, Text(value))
                        for (key, value) in raul.DIGITS.iteritems())
alphanumeric_mapping = dict((key, Text(value))
                        for (key, value) in raul.ALPHANUMERIC.iteritems())

alphabet_rule = Sequence([Literal("letters"), Repetition(RuleRef(name="x", rule=MappingRule(name="t", mapping=alphabet_mapping)), min=1, max=20)])
numbers_rule = Sequence([Literal("digits"), Repetition(RuleRef(name="y", rule=MappingRule(name="u", mapping=numbers_mapping)), min=1, max=20)])
alphanumeric_rule = Sequence([Literal("alphanumeric"), Repetition(RuleRef(name="z", rule=MappingRule(name="v", mapping=alphanumeric_mapping)), min=1, max=20)])
finishes = [alphabet_rule, numbers_rule, alphanumeric_rule]

# Second we create a repetition of keystroke elements.
#  This element will match anywhere between 1 and 16 repetitions
#  of the keystroke elements.  Note that we give this element
#  the name "sequence" so that it can be used as an extra in
#  the rule definition below.
# Note: when processing a recognition, the *value* of this element
#  will be a sequence of the contained elements: a sequence of
#  actions.
sequence = Repetition(single_action, min=1, max=16, name="sequence")
vim_sequence = Repetition(vim_single_action, min=1, max=16, name="sequence")

extras = [
    sequence, # Sequence of actions defined above.
    IntegerRef("n", 1, 100), # Times to repeat the sequence.
    Alternative([Literal("hi")], name="finish"),
]

vim_extras = [
    vim_sequence, # Sequence of actions defined above.
    IntegerRef("n", 1, 100), # Times to repeat the sequence.
    Alternative([Literal("hi")], name="finish"),
]

#---------------------------------------------------------------------------
# Here we define the top-level rule which the user can say.

class LiteralRule(CompoundRule):
  spec = "literal <format_rule>"

  extras = [format_rule]

  def _process_recognition(self, node, extras):
    extras["format_rule"].execute()

# This is the rule that actually handles recognitions. 
#  When a recognition occurs, it's _process_recognition() 
#  method will be called.  It receives information about the 
#  recognition in the "extras" argument: the sequence of 
#  actions and the number of times to repeat them.
class RepeatRule(CompoundRule):
  # Here we define this rule's spoken-form and special elements.
  spec = "[ <sequence> ] [ ( literal <format_rule> )  | <finish> ] [repeat <n> times]"

  defaults = {
    "n": 1, # Default repeat count.
  }

  # This method gets called when this rule is recognized.
  # Arguments:
  #  - node -- root node of the recognition parse tree.
  #  - extras -- dict of the "extras" special elements:
  #   . extras["sequence"] gives the sequence of actions.
  #   . extras["n"] gives the repeat count.
  def _process_recognition(self, node, extras):
    sequence = extras.get("sequence", [])
    count = extras["n"]
    for i in range(count):
      for action in sequence:
        action.execute()
      if "format_rule" in extras:
        extras["format_rule"].execute()
      if "finish" in extras:
        for action in extras["finish"][1]:
          action.execute()

#---------------------------------------------------------------------------
# Create and load this module's grammar.

grammar = Grammar("multiedit", context=global_context & ~disable_context)
if ENABLE_VIM_COMMANDS:
  grammar.add_rule(RepeatRule(extras=vim_extras + [format_rule, Alternative(finishes, name="finish")], name="b", context=vim_context))
grammar.add_rule(RepeatRule(extras=extras + [format_rule, Alternative(finishes, name="finish")], name="a", context=(~vim_context)))
grammar.add_rule(LiteralRule())

grammar.load()

# Unload function which will be called at unload time.
def unload():
  global grammar
  if grammar:
    grammar.unload()
  grammar = None
