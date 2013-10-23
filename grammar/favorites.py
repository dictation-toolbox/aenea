from dragonfly import Grammar, AppContext, MappingRule, Text

import config
import personal

if config.PLATFORM == "proxy":
  grammar_context = AppContext(executable="notepad")
  grammar = Grammar("favorites", context=grammar_context)
  from proxy_nicknames import Text
else:
  grammar = Grammar("favorites")

class Favorites(MappingRule):
  mapping = dict(("fave " + key, Text(value))
                 for (key, value) in personal.FAVORITES.iteritems())

grammar.add_rule(Favorites())
grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
