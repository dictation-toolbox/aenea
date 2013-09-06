# This file is a command-module for Dragonfly.
#
# (based on the multiedit module from dragonfly-modules project)
# (heavily modified, you probably want the original)
# (the original copyright notice is reproduced below)
#
# (c) Copyright 2008 by Christo Butcher
# Licensed under the LGPL, see <http://www.gnu.org/licenses/>
#

try:
    import pkg_resources

    pkg_resources.require("dragonfly >= 0.6.5beta1.dev-r99")
except ImportError:
    pass

from dragonfly import Config, Section, Item, MappingRule, CompoundRule, Grammar, IntegerRef, Dictation, RuleRef, Alternative, Repetition
from proxy_nicknames import *

vim_context = AppRegexContext(name=".*VIM.*")

#---------------------------------------------------------------------------
# Here we globally defined the release action which releases all
#  modifier-keys used within this grammar.  It is defined here
#  because this functionality is used in many different places.
#  Note that it is harmless to release ("...:up") a key multiple
#  times or when that key is not held down at all.

release = Key("Shift_L:up, Control_L:up")

#---------------------------------------------------------------------------
# Set up this module's configuration.

command_table = {
  # Spoken-form        normal command              VIM (can set to None if same as normal)

  #### Cursor manipulation
  "up [<n>]":(         Key("Up:%(n)d"),            None),
  "down [<n>]":(       Key("Down:%(n)d"),          None),
  "left [<n>]":(       Key("Left:%(n)d"),          None),
  "right [<n>]":(      Key("Right:%(n)d"),         None),
  "gope [<n>]":(       Key("Prior:%(n)d"),         None),
  "drop [<n>]":(       Key("Next:%(n)d"),          None),
  "port [<n>]":(       Key("c-Left:%(n)d"),        Key("Escape, [ %(n)dbi ]") ),
  "yope [<n>]":(       Key("c-Right:%(n)d"),       Key("Escape, [ %(n)dwwi ]") ),
  "care":(             Key("Home"),                None),
  "doll":(             Key("End"),                 None),
  "top":(              Key("c-Home"),              Key("Escape, 1, s-g, i") ),
  "toe":(              Key("c-End"),               Key("Escape, s-g, i") ),
  }

# Set up vim default values.
for (key, (command, vim_command)) in command_table.iteritems():
  if vim_command is None:
    command_table[key] = (command, command)

a = """

  #### Various keys
  "space [<n>]":(      Key("space:%(n)d"),
  "punch [<n>]":(      Key("tab:%(n)d"),
  "slap [<n>]":(       Key("enter:%(n)d"),
  "chuck [<n>]":(      Key("del:%(n)d"),
  "back [<n>]":(       Key("backspace:%(n)d"),
  "fly":(              Key("escape"),
  "pop":(              Key("apps"), # right click

  #### Symbols
  "amp [<n>]":(        Key("ampersand:%(n)d"),
  "apostrophe [<n>]":( Key("apostrophe:%(n)d"),
  "star [<n>]":(       Key("asterisk:%(n)d"),
  "at [<n>]":(         Key("at:%(n)d"),
  "slosh [<n>]":(      Key("backslash:%(n)d"),
  "backtick [<n>]":(   Key("backtick:%(n)d"),
  "bar [<n>]":(        Key("bar:%(n)d"),
  "hat [<n>]":(        Key("caret:%(n)d"),
  "colon [<n>]":(      Key("colon:%(n)d"),
  "drip [<n>]":(       Key("comma:%(n)d"),
  "doll [<n>]":(       Key("dollar:%(n)d"),
  "dot [<n>]":(        Key("dot:%(n)d"),
  "quote [<n>]":(      Key("dquote:%(n)d"),
  "gets [<n>]":(       Key("equal:%(n)d"),
  "bang [<n>]":(       Key("exclamation:%(n)d"),
  "hash [<n>]":(       Key("hash:%(n)d"),
  "hyph [<n>]":(       Key("hyphen:%(n)d"),
  "minus [<n>]":(       Key("minus:%(n)d"),
  "percent [<n>]":(     Key("percent:%(n)d"),
  "plus [<n>]":(       Key("plus:%(n)d"),
  "quest [<n>]":(       Key("question:%(n)d"),
  "slash [<n>]":(       Key("slash:%(n)d"),
  "smote [<n>]":(       Key("squote:%(n)d"),
  "tilde [<n>]":(       Key("tilde:%(n)d"),
  "rail [<n>]":(       Key("underscore:%(n)d"),

  #### Lines
  "wipe [<n>]":(               release + Key("home, s-down:%(n)d, s-home, del"),
  "weiss [<n>]":(               release + Key("home, s-up:%(n)d, s-home, del"),
  "strip":(                     release + Key("s-end, del"),
  "striss":(                   release + Key("s-home, del"),
  "nab [<n>]":(                 release + Key("home, s-down:%(n)d, s-home, c-c, right"),
  "trance [<n>]":(             release + Key("home, s-down:%(n)d, s-home, c-c, home, c-v"),

  ### words
  "bump [<n>]":(               release + Key("right:2, c-left, cs-right:%(n)d, del"),
  "whack [<n>]":(               release + Key("left, c-right, cs-left:%(n)d, del"),
  "yose [<n>]":(               release + Key("right:2, c-left, cs-right:%(n)d, c-c, right"),
  "porche [<n>]":(             release + Key("left, c-right, cs-left:%(n)d, c-c, left"),

  ### copy/paste
  "pace":(                     release + Key("c-v"),
  "dupe <n>":(                 release + Key("c-c, c-v:%(n)d"),
  "cop":(                       release + Key("c-c"),
  "cut":(                       release + Key("c-x"),
  "gob":(                       release + Key("c-a"),

  "(shift|mark)":(             Key("shift:down"),
  "wave":(                     Key("shift:up, right"),
  "release [all]":(             release,

  ### other
  "switch":(                   release + Key("ctrl:down, tab"),
  "say <text>":(               release + Text("%(text)s"),
  "mimic <text>":(             release + Mimic(extra="text"),
        },
        """

  
a="""
#---------------------------------------------------------------------------
# Here we prepare the list of formatting functions from the config file.

# Retrieve text-formatting functions from this module's config file.
#  Each of these functions must have a name that starts with "format_".
format_functions = {}
if namespace:
    for name, function in namespace.items():
        if name.startswith("format_") and callable(function):
            spoken_form = function.__doc__.strip()

            # We wrap generation of the Function action in a function so
            #  that its *function* variable will be local.  Otherwise it
            #  would change during the next iteration of the namespace loop.
            def wrap_function(function):
                def _function(dictation):
                    formatted_text = function(dictation)
                    Text(formatted_text).execute()

                return Function(_function)

            action = wrap_function(function)
            format_functions[spoken_form] = action


# Here we define the text formatting rule.
# The contents of this rule were built up from the "format_*"
#  functions in this module's config file.
if format_functions:
    class FormatRule(MappingRule):
        mapping = format_functions
        extras = [Dictation("dictation")]

else:
    FormatRule = None
"""
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

    def _get_mode_index(self):
      if vim_context.matches(None, None, None):
        return 1
      else:
        return 0

#---------------------------------------------------------------------------
# Here we create an element which is the sequence of keystrokes.

# First we create an element that references the keystroke rule.
#  Note: when processing a recognition, the *value* of this element
#  will be the value of the referenced rule: an action.

mapping = dict((key, value[0]) for (key, value) in command_table.iteritems())
vim_mapping = dict((key, value[1]) for (key, value) in command_table.iteritems())

alternatives = []
vim_alternatives = []

alternatives.append(RuleRef(rule=KeystrokeRule(mapping=mapping, name="c")))
vim_alternatives.append(RuleRef(rule=KeystrokeRule(mapping=vim_mapping, name="d")))
#if FormatRule:
#    alternatives.append(RuleRef(rule=FormatRule()))
single_action = Alternative(alternatives)
vim_single_action = Alternative(vim_alternatives)

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
]

vim_extras = [
    vim_sequence, # Sequence of actions defined above.
    IntegerRef("n", 1, 100), # Times to repeat the sequence.
]

#---------------------------------------------------------------------------
# Here we define the top-level rule which the user can say.

# This is the rule that actually handles recognitions. 
#  When a recognition occurs, it's _process_recognition() 
#  method will be called.  It receives information about the 
#  recognition in the "extras" argument: the sequence of 
#  actions and the number of times to repeat them.
class RepeatRule(CompoundRule):
    # Here we define this rule's spoken-form and special elements.
    spec = "<sequence> [[[and] repeat [that]] <n> times]"

    defaults = {
        "n": 1, # Default repeat count.
    }

    # This method gets called when this rule is recognized.
    # Arguments:
    #  - node -- root node of the recognition parse tree.
    #  - extras -- dict of the "extras" special elements:
    #     . extras["sequence"] gives the sequence of actions.
    #     . extras["n"] gives the repeat count.
    def _process_recognition(self, node, extras):
        sequence = extras["sequence"]   # A sequence of actions.
        count = extras["n"]             # An integer repeat count.
        for i in range(count):
            for action in sequence:
                action.execute()
                #release.execute()

#---------------------------------------------------------------------------
# Create and load this module's grammar.

grammar = Grammar("multi edit")
grammar.add_rule(RepeatRule(extras=vim_extras, name="b", context=vim_context))
grammar.add_rule(RepeatRule(extras=extras, name="a", context=(~vim_context)))
grammar.load()

# Unload function which will be called at unload time.
def unload():
    global grammar
    if grammar: grammar.unload()
    grammar = None
