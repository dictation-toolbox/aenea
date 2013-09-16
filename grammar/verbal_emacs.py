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
import raul

from dragonfly import *
from proxy_nicknames import *

LEADER_KEY = "comma"
LINE_COMMAND_KEY = "colon"

leader = Key(LEADER_KEY)
line_command_key = Key(LINE_COMMAND_KEY)
escape = Key("Escape")
escape_leader = escape + Pause("30") + leader

vim_context = aenea.global_context & AppRegexContext(name="(?i)^.* vim$")

command_t_context = AppRegexContext(name="^GoToFile.*$") & vim_context
fugitive_index_context = AppRegexContext(name="^index.*\.git.*$") & vim_context

grammar = Grammar("verbal_emacs", context=vim_context)

class Motion(MappingRule):
  mapping = {
    "[<number>] up":Text("%(number)ik"),
    "[<number>] down":Text("%(number)ij"),
    "[<number>] left":Text("%(number)ih"),
    "[<number>] right":Text("%(number)il"),

    "[<number>] lope":Text("%(number)ib"),
    "[<number>] yope":Text("%(number)iw"),
    "[<number>] elope":Text("%(number)ige"),
    "[<number>] e yope":Text("%(number)ie"),

    "[<number>] lopert":Text("%(number)iB"),
    "[<number>] yopert":Text("%(number)iW"),
    "[<number>] elopert":Text("%(number)igE"),
    "[<number>] eyopert":Text("%(number)iE"),
    
    "[<number>] apla":Text("%(number)i{"),
    "[<number>] anla":Text("%(number)i}"),
    "[<number>] sapla":Text("%(number)i("),
    "[<number>] sanla":Text("%(number)i)"),

    "[<number>] paven":Text("%(number)i^"),
    "[<number>] riven":Text("%(number)i$"),

    "[<number>] screepaven":Text("%(number)ig^"),
    "[<number>] screeriven":Text("%(number)ig$"),

    "[<number>] scree up":Text("%(number)igk"),
    "[<number>] scree down":Text("%(number)igj"),

    "[<number>] wynac":Text("%(number)iG"),
    "tect":Text("%%"),

    "matu":Text("M"),

    "[<number>] wynac top":Text("%(number)iH"),
    "[<number>] wynac toe":Text("%(number)iL"),

    "[<number>] phytic":Text("%(number)if"),
    "[<number>] fitton":Text("%(number)iF"),

    "[<number>] pre phytic":Text("%(number)it"),
    "[<number>] pre fitton":Text("%(number)iT"),
    }
  
  extras = [IntegerRef("number", 1, 1000)]
  defaults = {"number":1}

class VimCommand(CompoundRule):
  spec = ("<motion>")
  extras = [RuleRef(Motion(), name="motion")]

  def _process_recognition(self, node, extras):
    extras["motion"].execute()

grammar.add_rule(VimCommand())

# grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
