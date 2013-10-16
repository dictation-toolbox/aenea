from dragonfly import (Grammar, AppContext, CompoundRule, MappingRule, Repetition, RuleRef, Key)

import config, raul

modifier_keys = {"control":"c", "altar":"a",
                 "shift":"s", "super":"w"}
if config.PLATFORM == "proxy":
  from proxy_nicknames import Key
  grammar_context = AppContext(executable="notepad")
  grammar = Grammar("nato", context=grammar_context)
  modifier_keys.update(hyper="h", meta="m")
else:
  grammar = Grammar("nato")
  modifier_keys.update(windows="w", flag="w")

class MetaKey(CompoundRule):
  spec = "<modifiers> <key>"

  extras = [Repetition(RuleRef(MappingRule(mapping=modifier_keys, name="modmap"), name="modifier_atom"), 1, len(modifier_keys), name="modifiers"),
            RuleRef(MappingRule(mapping=raul.ALPHANUMERIC_EXTENDED, name="keymap"), name="key")]
  
  def _process_recognition (self, node, extras):
    delegates = node.children[0].children[0].children
    modifiers = delegates[0].value()
    key = delegates[1].value()

    Key("%s-%s" % (''.join(modifiers), key)).execute()

grammar.add_rule(MetaKey())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
