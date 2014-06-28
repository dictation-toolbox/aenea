from dragonfly import AppContext, Repetition, Choice
from proxy_nicknames import Text, Key

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
