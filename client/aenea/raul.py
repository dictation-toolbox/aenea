import json
import os

from dragonfly import Choice, AppContext, Repetition
from aenea.proxy_nicknames import Text, Key
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

ALPHANUMERIC_EXTENDED = ALPHANUMERIC.copy()

ALPHANUMERIC_EXTENDED['enter'] = 'enter'
ALPHANUMERIC_EXTENDED['comma'] = 'comma'

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

def load_grammar_config(module_name, defaults=None):
    """Loads the configuration for the specified grammar name. A missing file
       is interpreted as an empty one. defaults may be a dict of defaults."""
    grammar_config_path = os.path.join(aenea.config.PROJECT_ROOT, 'grammar_config')

    module_name += '.json'

    conf_path = os.path.join(grammar_config_path, module_name)

    # Handle defaults.
    if defaults is None:
        configuration = {}
    else:
        configuration = defaults.copy()

    # Load the configuration if it exists. If anything goes wrong, we print
    # an error and fallback to defaults.
    try:
        if os.path.exists(conf_path):
            with open(conf_path, "r") as fd:
                configuration.update(json.load(fd))
    except Exception as e:
        print("Could not load config file for grammar %s: %s" %
              (module_name, str(e)))
    return configuration


def make_grammar_commands(module_name, mapping, config_key='commands'):
    """Given the command map from default spoken phrase to actions in mapping,
       constructs a mapping that takes user config, if specified, into account.
       config_key may be a key in the JSON to use (for modules with multiple
       mapping rules.) If a user phrase starts with !,
       no mapping is generated for that phrase."""
    conf = load_grammar_config(module_name).get(config_key, {})
    commands = mapping.copy()

    # Nuke the default if the user sets one or more aliases.
    for default_phrase in set(conf.itervalues()):
        del commands[str(default_phrase)]

    for (user_phrase, default_phrase) in conf.iteritems():
        # Dragonfly chokes on unicode, JSON's default.
        user_phrase = str(user_phrase)
        default_phrase = str(default_phrase)
        assert default_phrase in mapping, ("Invalid mapping value in module %s config_key %s: %s" % (module_name, config_key, default_phrase))

        # Allow users to nuke a command with !
        if not user_phrase.startswith('!'):
            commands[user_phrase] = mapping[default_phrase]
    return commands

