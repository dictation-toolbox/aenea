# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (2014) Alex Roper
# Alex Roper <alex@aroper.net>

'''Mock module to allow importing aenea on linux (mostly so you can run tests
   locally.'''

print 'Unable to import Dragonfly (safe to ignore if running tests).'

class ActionBase(object):
    def __init__(self, spec):
        self.actions = self._parse_spec(spec)

    def execute(self, data=None):
        self._execute_events(self.actions)


class DynStrActionBase(ActionBase):
    pass

DictList = lambda name: dict()
List = lambda name: list()


class _WindowInfo(object):
    executable = None
    title = None
    handle = None


class Window(object):
    @staticmethod
    def get_foreground():
        return _WindowInfo


class Repetition(object):
    def __init__(self, child, min=1, max=None, name=None, default=None):
        pass


class Choice(object):
    def __init__(self, name, choices, extras=None, default=None):
        pass


class AppContext(object):
    def __init__(self, *a, **kw):
        pass

    def matches(self, *a, **kw):
       return True


class Grammar:
   pass

class ActionError:
   pass

class Alternative:
   pass

class Clipboard:
   pass

class Compound:
   pass

class CompoundRule:
   pass

class Config:
   pass

class ConnectionGrammar:
   pass

class Context:
   pass

class DictList:
   pass

class DictListRef:
   pass

class Dictation:
   pass

class Digits:
   pass

class DigitsRef:
   pass

class ElementBase:
   pass

class Empty:
   pass

class FocusWindow:
   pass

class Function:
   pass

class HardwareInput:
   pass

class Integer:
   pass

class IntegerRef:
   pass

class Item:
   pass

class Key:
   pass

class Keyboard:
   pass

class KeyboardInput:
   pass

class List:
   pass

class ListBase:
   pass

class ListRef:
   pass

class Literal:
   pass

class MappingRule:
   pass

class Mimic:
   pass

class Monitor:
   pass

class Mouse:
   pass

class MouseInput:
   pass

class Number:
   pass

class NumberRef:
   pass

class Optional:
   pass

class Paste:
   pass

class Pause:
   pass

class Playback:
   pass

class PlaybackHistory:
   pass

class Point:
   pass

class RecognitionHistory:
   pass

class RecognitionObserver:
   pass

class Rectangle:
   pass

class Repeat:
   pass

class Rule:
   pass

class RuleRef:
   pass

class Section:
   pass

class Sequence:
   pass

class Text:
   pass

class Typeable:
   pass

class WaitWindow:
   pass
