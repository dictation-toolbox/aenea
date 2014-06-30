from aenea.proxy_nicknames import Grammar, MappingRule, Text

grammar = Grammar('hello world aenea')

print 'Aenea hello world grammar: Loaded.'

class TestRule(MappingRule):
    mapping = {
          'test hello world remote grammar': Text('Aenea remote setup operational'),
        }

grammar.add_rule(TestRule())
grammar.load()

def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
