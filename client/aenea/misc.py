'''Contains generic utility data and functions useful when writing grammars.'''

import dragonfly

import aenea.vocabulary

LOWERCASE_LETTERS = aenea.vocabulary.make_grammar_commands('misc', {
    'alpha': 'a',
    'bravo': 'b',
    'charlie': 'c',
    'delta': 'd',
    'echo': 'e',
    'foxtrot': 'f',
    'golf': 'g',
    'hotel': 'h',
    'indigo': 'i',
    'juliet': 'j',
    'kilo': 'k',
    'lima': 'l',
    'mike': 'm',
    'november': 'n',
    'oscar': 'o',
    'poppa': 'p',
    'quiche': 'q',
    'romeo': 'r',
    'sierra': 's',
    'tango': 't',
    'uniform': 'u',
    'victor': 'v',
    'whiskey': 'w',
    'x-ray': 'x',
    'yankee': 'y',
    'zulu': 'z'
    }, 'letters.lower')

UPPERCASE_LETTERS = aenea.vocabulary.make_grammar_commands('misc', {
    'upper alpha': 'A',
    'upper bravo': 'B',
    'upper charlie': 'C',
    'upper delta': 'D',
    'upper echo': 'E',
    'upper foxtrot': 'F',
    'upper golf': 'G',
    'upper hotel': 'H',
    'upper indigo': 'I',
    'upper juliet': 'J',
    'upper kilo': 'K',
    'upper lima': 'L',
    'upper mike': 'M',
    'upper november': 'N',
    'upper oscar': 'O',
    'upper poppa': 'P',
    'upper quiche': 'Q',
    'upper romeo': 'R',
    'upper sierra': 'S',
    'upper tango': 'T',
    'upper uniform': 'U',
    'upper victor': 'V',
    'upper whiskey': 'W',
    'upper x-ray': 'X',
    'upper yankee': 'y',
    'upper zulu': 'Z'
    }, 'letters.upper')

DIGITS = aenea.vocabulary.make_grammar_commands('misc', {
    'zero': '0',
    'one': '1',
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'niner': '9'
    }, 'digits')

LETTERS = LOWERCASE_LETTERS.copy()
LETTERS.update(UPPERCASE_LETTERS)

ALPHANUMERIC = LETTERS.copy()
ALPHANUMERIC.update(DIGITS)


class DigitalInteger(dragonfly.Repetition):
    '''An integer element spelled digit by digit (eg, enter 50 by saying
       'five zero'. Useful in places where Dragon would complain of the
       grammar's complexity if regular integers were used. min and max are
       number of digits, not value of the number.'''
    child = dragonfly.Choice('digit', DIGITS)

    def __init__(self, name, min, max, *args, **kw):
        dragonfly.Repetition.__init__(
            self,
            self.child,
            min,
            max,
            name=name,
            *args,
            **kw
            )

    def value(self, node):
        return int(''.join(dragonfly.Repetition.value(self, node)))
