import json
import os

from dragonfly import Choice, AppContext, Repetition, Pause, Mimic
import dragonfly
from aenea.proxy_nicknames import Text, Key, MousePhantomClick, NoAction, ContextAction
import aenea.config

def SelfChoice(name, ch):
    return Choice(name, dict(zip(ch, ch)))

LETTERS = ['alpha', 'bravo', 'charlie', 'delta', 'echo', 'foxtrot', 'golf',
           'hotel', 'indigo', 'juliet', 'kilo', 'lima', 'mike', 'november',
           'oscar', 'poppa', 'quiche', 'romeo', 'sierra', 'tango', 'uniform',
           'victor', 'whiskey', 'x-ray', 'yankee', 'zulu']
LETTERS = dict(zip(LETTERS, (chr(ord('a') + i) for i in range(26))))
temp = {}
for (spoken, written) in LETTERS.iteritems():
    temp[spoken] = written
    temp['upper ' + spoken] = written.upper()
LETTERS = temp

DIGITS = ['zero', 'one', 'to', '3', 'for', '5', '6', '7', '8', 'nine']
DIGITS = dict(zip(DIGITS, (chr(ord('0') + i) for i in range(10))))
DIGITS['niner'] = '9'

ALPHANUMERIC = LETTERS.copy()
ALPHANUMERIC.update(DIGITS)

global_context = (AppContext(executable='python',
                             title='Aenea client - Dictation capturing') |
                  AppContext(executable='notepad'))

class DigitalInteger(Repetition):
    digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    child = Choice('digit', dict(zip(digits, digits)))

    def __init__(self, name, min, max, *args, **kw):
        Repetition.__init__(self, self.child, min, max, name=name, *args, **kw)

    def value(self, node):
        return int(''.join(Repetition.value(self, node)))


def Nested(command):
    return Text(command) + Key('left:%i' % (len(command) / 2))
