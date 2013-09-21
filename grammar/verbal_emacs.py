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

from verbal_emacs import *

LEADER_KEY = "comma"
LINE_COMMAND_KEY = "colon"

leader = Key(LEADER_KEY)
line_command_key = Key(LINE_COMMAND_KEY)
escape = Key("Escape")
escape_leader = escape + Pause("30") + leader

vim_context = AppRegexContext(name="(?i).*VIM.*") & ~AppRegexContext(name="(?i)^.*verbal_emacs.*$")

command_t_context = AppRegexContext(name="^GoToFile.*$") & vim_context
fugitive_index_context = AppRegexContext(name="^index.*\.git.*$") & vim_context

grammar = Grammar("verbal_emacs", context=vim_context)

def execute_insertion_buffer(insertion_buffer):
  if not insertion_buffer:
    return

  if insertion_buffer[0][0] is not None:
    insertion_buffer[0][0].execute()
  else:
    Key("a").execute()
  
  for insertion in insertion_buffer:
    insertion[1].execute()

  Key("escape:2").execute()

class VimCommand(CompoundRule):
  spec = ("[<app>] [<literal>]")
  extras = [Repetition(Alternative([RuleRef(Command()), RuleRef(Insertion())]), max=20, name="app"),
            RuleRef(LiteralIdentifierInsertion(), name="literal")]

  def _process_recognition(self, node, extras):
    insertion_buffer = []
    commands = []
    if "app" in extras:
      commands.extend(extras["app"])
    if "literal" in extras:
      commands.append(extras["literal"])
    for command in commands:
      mode, command = command
      if mode == "i":
        insertion_buffer.append(command)
      else:
        execute_insertion_buffer(insertion_buffer)
        insertion_buffer = []
        command.execute()
    execute_insertion_buffer(insertion_buffer)

grammar.add_rule(VimCommand())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
