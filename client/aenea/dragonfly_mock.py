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


class Grammar(object):
   pass

class ActionError(object):
   pass

class Alternative(object):
   pass

class Clipboard(object):
   pass

class Compound(object):
   pass

class CompoundRule(object):
   pass

class Config(object):
   pass

class ConnectionGrammar(object):
   pass

class Context(object):
   pass

class DictListRef(object):
   pass

class Dictation(object):
   pass

class Digits(object):
   pass

class DigitsRef(object):
   pass

class ElementBase(object):
   pass

class Empty(object):
   pass

class FocusWindow(object):
   pass

class Function(object):
   pass

class HardwareInput(object):
   pass

class Integer(object):
   pass

class IntegerRef(object):
   pass

class Item(object):
   pass

class Key(object):
   pass

class Keyboard(object):
   pass

class KeyboardInput(object):
   pass

class ListBase(object):
   pass

class ListRef(object):
   pass

class Literal(object):
   pass

class MappingRule(object):
   pass

class Mimic(object):
   pass

class Monitor(object):
   pass

class Mouse(object):
   pass

class MouseInput(object):
   pass

class Number(object):
   pass

class NumberRef(object):
   pass

class Optional(object):
   pass

class Paste(object):
   pass

class Pause(object):
   pass

class Playback(object):
   pass

class PlaybackHistory(object):
   pass

class Point(object):
   pass

class RecognitionHistory(object):
   pass

class RecognitionObserver(object):
   pass

class Rectangle(object):
   pass

class Repeat(object):
   pass

class Rule(object):
   pass

class RuleRef(object):
   pass

class Section(object):
   pass

class Sequence(object):
   pass

class Text(object):
   pass

class Typeable(object):
   def __init__(self, *a, **kw):
      '''Just swallow the arguments for now; this class is not currently used
         by tests.'''

class WaitWindow(object):
   pass

typeables = {
    # Lowercase letter keys
    "a":                Typeable(char='a'),
    "alpha":            Typeable(char='a'),
    "b":                Typeable(char='b'),
    "bravo":            Typeable(char='b'),
    "c":                Typeable(char='c'),
    "charlie":          Typeable(char='c'),
    "d":                Typeable(char='d'),
    "delta":            Typeable(char='d'),
    "e":                Typeable(char='e'),
    "echo":             Typeable(char='e'),
    "f":                Typeable(char='f'),
    "foxtrot":          Typeable(char='f'),
    "g":                Typeable(char='g'),
    "golf":             Typeable(char='g'),
    "h":                Typeable(char='h'),
    "hotel":            Typeable(char='h'),
    "i":                Typeable(char='i'),
    "india":            Typeable(char='i'),
    "j":                Typeable(char='j'),
    "juliet":           Typeable(char='j'),
    "k":                Typeable(char='k'),
    "kilo":             Typeable(char='k'),
    "l":                Typeable(char='l'),
    "lima":             Typeable(char='l'),
    "m":                Typeable(char='m'),
    "mike":             Typeable(char='m'),
    "n":                Typeable(char='n'),
    "november":         Typeable(char='n'),
    "o":                Typeable(char='o'),
    "oscar":            Typeable(char='o'),
    "p":                Typeable(char='p'),
    "papa":             Typeable(char='p'),
    "q":                Typeable(char='q'),
    "quebec":           Typeable(char='q'),
    "r":                Typeable(char='r'),
    "romeo":            Typeable(char='r'),
    "s":                Typeable(char='s'),
    "sierra":           Typeable(char='s'),
    "t":                Typeable(char='t'),
    "tango":            Typeable(char='t'),
    "u":                Typeable(char='u'),
    "uniform":          Typeable(char='u'),
    "v":                Typeable(char='v'),
    "victor":           Typeable(char='v'),
    "w":                Typeable(char='w'),
    "whisky":           Typeable(char='w'),
    "x":                Typeable(char='x'),
    "xray":             Typeable(char='x'),
    "y":                Typeable(char='y'),
    "yankee":           Typeable(char='y'),
    "z":                Typeable(char='z'),
    "zulu":             Typeable(char='z'),

    # Uppercase letter keys
    "A":                Typeable(char='A'),
    "Alpha":            Typeable(char='A'),
    "B":                Typeable(char='B'),
    "Bravo":            Typeable(char='B'),
    "C":                Typeable(char='C'),
    "Charlie":          Typeable(char='C'),
    "D":                Typeable(char='D'),
    "Delta":            Typeable(char='D'),
    "E":                Typeable(char='E'),
    "Echo":             Typeable(char='E'),
    "F":                Typeable(char='F'),
    "Foxtrot":          Typeable(char='F'),
    "G":                Typeable(char='G'),
    "Golf":             Typeable(char='G'),
    "H":                Typeable(char='H'),
    "Hotel":            Typeable(char='H'),
    "I":                Typeable(char='I'),
    "India":            Typeable(char='I'),
    "J":                Typeable(char='J'),
    "Juliet":           Typeable(char='J'),
    "K":                Typeable(char='K'),
    "Kilo":             Typeable(char='K'),
    "L":                Typeable(char='L'),
    "Lima":             Typeable(char='L'),
    "M":                Typeable(char='M'),
    "Mike":             Typeable(char='M'),
    "N":                Typeable(char='N'),
    "November":         Typeable(char='N'),
    "O":                Typeable(char='O'),
    "Oscar":            Typeable(char='O'),
    "P":                Typeable(char='P'),
    "Papa":             Typeable(char='P'),
    "Q":                Typeable(char='Q'),
    "Quebec":           Typeable(char='Q'),
    "R":                Typeable(char='R'),
    "Romeo":            Typeable(char='R'),
    "S":                Typeable(char='S'),
    "Sierra":           Typeable(char='S'),
    "T":                Typeable(char='T'),
    "Tango":            Typeable(char='T'),
    "U":                Typeable(char='U'),
    "Uniform":          Typeable(char='U'),
    "V":                Typeable(char='V'),
    "Victor":           Typeable(char='V'),
    "W":                Typeable(char='W'),
    "Whisky":           Typeable(char='W'),
    "X":                Typeable(char='X'),
    "Xray":             Typeable(char='X'),
    "Y":                Typeable(char='Y'),
    "Yankee":           Typeable(char='Y'),
    "Z":                Typeable(char='Z'),
    "Zulu":             Typeable(char='Z'),

    # Number keys
    "0":                Typeable(char='0'),
    "zero":             Typeable(char='0'),
    "1":                Typeable(char='1'),
    "one":              Typeable(char='1'),
    "2":                Typeable(char='2'),
    "two":              Typeable(char='2'),
    "3":                Typeable(char='3'),
    "three":            Typeable(char='3'),
    "4":                Typeable(char='4'),
    "four":             Typeable(char='4'),
    "5":                Typeable(char='5'),
    "five":             Typeable(char='5'),
    "6":                Typeable(char='6'),
    "six":              Typeable(char='6'),
    "7":                Typeable(char='7'),
    "seven":            Typeable(char='7'),
    "8":                Typeable(char='8'),
    "eight":            Typeable(char='8'),
    "9":                Typeable(char='9'),
    "nine":             Typeable(char='9'),

    # Symbol keys
    "bang":             Typeable(char='!'),
    "exclamation":      Typeable(char='!'),
    "at":               Typeable(char='@'),
    "hash":             Typeable(char='#'),
    "dollar":           Typeable(char='$'),
    "percent":          Typeable(char='%'),
    "caret":            Typeable(char='^'),
    "and":              Typeable(char='&'),
    "ampersand":        Typeable(char='&'),
    "star":             Typeable(char='*'),
    "asterisk":         Typeable(char='*'),
    "leftparen":        Typeable(char='('),
    "lparen":           Typeable(char='('),
    "rightparen":       Typeable(char=')'),
    "rparen":           Typeable(char=')'),
    "minus":            Typeable(char='-'),
    "hyphen":           Typeable(char='-'),
    "underscore":       Typeable(char='_'),
    "plus":             Typeable(char='+'),
    "backtick":         Typeable(char='`'),
    "tilde":            Typeable(char='~'),
    "leftbracket":      Typeable(char='['),
    "lbracket":         Typeable(char='['),
    "rightbracket":     Typeable(char=']'),
    "rbracket":         Typeable(char=']'),
    "leftbrace":        Typeable(char='{'),
    "lbrace":           Typeable(char='{'),
    "rightbrace":       Typeable(char='}'),
    "rbrace":           Typeable(char='}'),
    "backslash":        Typeable(char='\\'),
    "bar":              Typeable(char='|'),
    "colon":            Typeable(char=':'),
    "semicolon":        Typeable(char=';'),
    "apostrophe":       Typeable(char="'"),
    "singlequote":      Typeable(char="'"),
    "squote":           Typeable(char="'"),
    "quote":            Typeable(char='"'),
    "doublequote":      Typeable(char='"'),
    "dquote":           Typeable(char='"'),
    "comma":            Typeable(char=','),
    "dot":              Typeable(char='.'),
    "slash":            Typeable(char='/'),
    "lessthan":         Typeable(char='<'),
    "leftangle":        Typeable(char='<'),
    "langle":           Typeable(char='<'),
    "greaterthan":      Typeable(char='>'),
    "rightangle":       Typeable(char='>'),
    "rangle":           Typeable(char='>'),
    "question":         Typeable(char='?'),
    "equal":            Typeable(char='='),
    "equals":           Typeable(char='='),

    # Whitespace and editing keys
    "enter":            Typeable(code='win32con.VK_RETURN', name='enter'),
    "tab":              Typeable(code='win32con.VK_TAB', name='tab'),
    "space":            Typeable(code='win32con.VK_SPACE', name='space'),
    "backspace":        Typeable(code='win32con.VK_BACK', name='backspace'),
    "delete":           Typeable(code='win32con.VK_DELETE', name='delete'),
    "del":              Typeable(code='win32con.VK_DELETE', name='del'),

    # Main modifier keys
    "shift":            Typeable(code='win32con.VK_SHIFT', name='shift'),
    "control":          Typeable(code='win32con.VK_CONTROL', name='control'),
    "ctrl":             Typeable(code='win32con.VK_CONTROL', name='ctrl'),
    "alt":              Typeable(code='win32con.VK_MENU', name='alt'),

    # Right modifier keys
    "ralt":             Typeable(code='win32con.VK_RMENU', name='ralt'),
    "rshift":           Typeable(code='win32con.VK_RSHIFT', name='rshift'),
    "rcontrol":         Typeable(code='win32con.VK_RCONTROL', name='rcontrol'),
    "rctrl":            Typeable(code='win32con.VK_RCONTROL', name='rctrl'),

    # Special keys
    "escape":           Typeable(code='win32con.VK_ESCAPE', name='escape'),
    "insert":           Typeable(code='win32con.VK_INSERT', name='insert'),
    "pause":            Typeable(code='win32con.VK_PAUSE', name='pause'),
    "win":              Typeable(code='win32con.VK_LWIN', name='win'),
    "rwin":             Typeable(code='win32con.VK_RWIN', name='rwin'),
    "apps":             Typeable(code='win32con.VK_APPS', name='apps'),
    "popup":            Typeable(code='win32con.VK_APPS', name='popup'),
    "snapshot":         Typeable(code='win32con.VK_SNAPSHOT', name='snapshot'),
    "printscreen":      Typeable(code='win32con.VK_SNAPSHOT', name='printscreen'),

    # Lock keys
    # win32api.GetKeyState(code) could be used to toggle lock keys sensibly
    # instead of using the up/down modifiers.
    "scrolllock":       Typeable(code='win32con.VK_SCROLL', name='scrolllock'),
    "numlock":          Typeable(code='win32con.VK_NUMLOCK', name='numlock'),
    "capslock":         Typeable(code='win32con.VK_CAPITAL', name='capslock'),

    # Navigation keys
    "up":               Typeable(code='win32con.VK_UP', name='up'),
    "down":             Typeable(code='win32con.VK_DOWN', name='down'),
    "left":             Typeable(code='win32con.VK_LEFT', name='left'),
    "right":            Typeable(code='win32con.VK_RIGHT', name='right'),
    "pageup":           Typeable(code='win32con.VK_PRIOR', name='pageup'),
    "pgup":             Typeable(code='win32con.VK_PRIOR', name='pgup'),
    "pagedown":         Typeable(code='win32con.VK_NEXT', name='pagedown'),
    "pgdown":           Typeable(code='win32con.VK_NEXT', name='pgdown'),
    "home":             Typeable(code='win32con.VK_HOME', name='home'),
    "end":              Typeable(code='win32con.VK_END', name='end'),

    # Number pad keys
    "npmul":            Typeable(code='win32con.VK_MULTIPLY', name='npmul'),
    "npadd":            Typeable(code='win32con.VK_ADD', name='npadd'),
    "npsep":            Typeable(code='win32con.VK_SEPARATOR', name='npsep'),
    "npsub":            Typeable(code='win32con.VK_SUBTRACT', name='npsub'),
    "npdec":            Typeable(code='win32con.VK_DECIMAL', name='npdec'),
    "npdiv":            Typeable(code='win32con.VK_DIVIDE', name='npdiv'),
    "numpad0":          Typeable(code='win32con.VK_NUMPAD0', name='numpad0'),
    "np0":              Typeable(code='win32con.VK_NUMPAD0', name='np0'),
    "numpad1":          Typeable(code='win32con.VK_NUMPAD1', name='numpad1'),
    "np1":              Typeable(code='win32con.VK_NUMPAD1', name='np1'),
    "numpad2":          Typeable(code='win32con.VK_NUMPAD2', name='numpad2'),
    "np2":              Typeable(code='win32con.VK_NUMPAD2', name='np2'),
    "numpad3":          Typeable(code='win32con.VK_NUMPAD3', name='numpad3'),
    "np3":              Typeable(code='win32con.VK_NUMPAD3', name='np3'),
    "numpad4":          Typeable(code='win32con.VK_NUMPAD4', name='numpad4'),
    "np4":              Typeable(code='win32con.VK_NUMPAD4', name='np4'),
    "numpad5":          Typeable(code='win32con.VK_NUMPAD5', name='numpad5'),
    "np5":              Typeable(code='win32con.VK_NUMPAD5', name='np5'),
    "numpad6":          Typeable(code='win32con.VK_NUMPAD6', name='numpad6'),
    "np6":              Typeable(code='win32con.VK_NUMPAD6', name='np6'),
    "numpad7":          Typeable(code='win32con.VK_NUMPAD7', name='numpad7'),
    "np7":              Typeable(code='win32con.VK_NUMPAD7', name='np7'),
    "numpad8":          Typeable(code='win32con.VK_NUMPAD8', name='numpad8'),
    "np8":              Typeable(code='win32con.VK_NUMPAD8', name='np8'),
    "numpad9":          Typeable(code='win32con.VK_NUMPAD9', name='numpad9'),
    "np9":              Typeable(code='win32con.VK_NUMPAD9', name='np9'),

    # Function keys
    "f1":               Typeable(code='win32con.VK_F1', name='f1'),
    "f2":               Typeable(code='win32con.VK_F2', name='f2'),
    "f3":               Typeable(code='win32con.VK_F3', name='f3'),
    "f4":               Typeable(code='win32con.VK_F4', name='f4'),
    "f5":               Typeable(code='win32con.VK_F5', name='f5'),
    "f6":               Typeable(code='win32con.VK_F6', name='f6'),
    "f7":               Typeable(code='win32con.VK_F7', name='f7'),
    "f8":               Typeable(code='win32con.VK_F8', name='f8'),
    "f9":               Typeable(code='win32con.VK_F9', name='f9'),
    "f10":              Typeable(code='win32con.VK_F10', name='f10'),
    "f11":              Typeable(code='win32con.VK_F11', name='f11'),
    "f12":              Typeable(code='win32con.VK_F12', name='f12'),
    "f13":              Typeable(code='win32con.VK_F13', name='f13'),
    "f14":              Typeable(code='win32con.VK_F14', name='f14'),
    "f15":              Typeable(code='win32con.VK_F15', name='f15'),
    "f16":              Typeable(code='win32con.VK_F16', name='f16'),
    "f17":              Typeable(code='win32con.VK_F17', name='f17'),
    "f18":              Typeable(code='win32con.VK_F18', name='f18'),
    "f19":              Typeable(code='win32con.VK_F19', name='f19'),
    "f20":              Typeable(code='win32con.VK_F20', name='f20'),
    "f21":              Typeable(code='win32con.VK_F21', name='f21'),
    "f22":              Typeable(code='win32con.VK_F22', name='f22'),
    "f23":              Typeable(code='win32con.VK_F23', name='f23'),
    "f24":              Typeable(code='win32con.VK_F24', name='f24'),

    # Multimedia keys
    "volumeup":         Typeable(code='win32con.VK_VOLUME_UP', name='volumeup'),
    "volup":            Typeable(code='win32con.VK_VOLUME_UP', name='volup'),
    "volumedown":       Typeable(code='win32con.VK_VOLUME_DOWN', name='volumedown'),
    "voldown":          Typeable(code='win32con.VK_VOLUME_DOWN', name='voldown'),
    "volumemute":       Typeable(code='win32con.VK_VOLUME_MUTE', name='volumemute'),
    "volmute":          Typeable(code='win32con.VK_VOLUME_MUTE', name='volmute'),
    "tracknext":        Typeable(code='win32con.VK_MEDIA_NEXT_TRACK', name='tracknext'),
    "trackprev":        Typeable(code='win32con.VK_MEDIA_PREV_TRACK', name='trackprev'),
    "playpause":        Typeable(code='win32con.VK_MEDIA_PLAY_PAUSE', name='playpause'),
    "browserback":      Typeable(code='win32con.VK_BROWSER_BACK', name='browserback'),
    "browserforward":   Typeable(code='win32con.VK_BROWSER_FORWARD', name='browserforward'),
    }
