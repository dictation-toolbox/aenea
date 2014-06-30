from dragonfly import Grammar, MappingRule, Text

grammar = Grammar('hello world')

class TestRule(MappingRule):
    mapping = {
          'test hello world grammar': Text('Hello world grammar: recognition successful!'),
        }

grammar.add_rule(TestRule())
grammar.load()

def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
