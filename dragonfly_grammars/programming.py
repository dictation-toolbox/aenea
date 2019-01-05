# commands for controlling various programs

from aenea import *

javascript_syntax = {
    # A constant or let
    'const ': Text('const '),
    'let ': Text('let '),
    # Easy console logging
    'console log [<text>]': Text('console.log(%(text)s)')
}

general_syntax = {
    # A smart equals command
    'quail': Text(' = '),
    'Finn': Text('end'),
    'if': Text('if'),
}

html_syntax = {
    'Dave': Text('div'),
}

class ProgrammingRule(MappingRule):
    mapping = dict(general_syntax.items() + html_syntax.items() + javascript_syntax.items() )
    extras = [
    Dictation("text"),
    IntegerRef("n", 1, 100),
    ]
    defaults = {
    "text": "",
    "n": 1,
    }
