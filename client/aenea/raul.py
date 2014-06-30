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

ALPHANUMERIC_EXTENDED = ALPHANUMERIC.copy()

ALPHANUMERIC_EXTENDED['enter'] = 'enter'
ALPHANUMERIC_EXTENDED['comma'] = 'comma'

global_context = (AppContext(executable='python',
                             title='Aenea client - Dictation capturing') |
                  AppContext(executable='notepad'))

dynamic_vocabulary = {}
static_vocabulary = {}

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
    # an error and fallback to defaults. An empty or missing file is not
    # an error.
    try:
        if os.path.exists(conf_path):
            with open(conf_path, "r") as fd:
                configuration.update(json.load(fd))
    except Exception as e:
        print("Could not load config file for grammar %s: %s" %
              (module_name, str(e)))
    return configuration


def build_action(action):
    actions = {'Text': Text, 'Key': Key, 'Pause': Pause, 'Mimic': Mimic,
               'MousePhantomClick': MousePhantomClick,
               'ContextAction': ContextAction, 'NoAction': NoAction}
    return actions[str(action['type'])](*map(str, action['args']))


def build_action_list(command):
    if len(command) == 0:
        return Text('') # Nop
    else:
        agg = build_action(command[0])
        for action in command[1:]:
            agg += build_action(action)
        return agg


def update_vocabulary(vocabulary, name, tags, vocab, shortcuts):
    global dynamic_vocabulary
    global static_vocabulary
    if vocabulary == 'dynamic':
        vocabulary_global = dynamic_vocabulary
        container = lambda tag_name: dragonfly.DictList('dynamic %s' % str(tag_name))
    elif vocabulary == 'static':
        vocabulary_global = static_vocabulary
        container = lambda tag_name: dict()
    else:
        assert 0

    this_file = {}
    for (dataset, default) in ((vocab, Text), (shortcuts, Key)):
        for phrase, command in dataset.iteritems():
            if isinstance(command, basestring):
                this_file[str(phrase)] = default(str(command))
            else:
                this_file[str(phrase)] = build_action_list(command)

    # Each DictList can only belong to a single grammar, so each module gets
    # its own DictList containing all its vocab.
    for t in tags:
        if str(t) not in vocabulary_global:
            vocabulary_global[str(t)] = container(name)
        vocabulary_global[str(t)].update(this_file)


def load_vocabulary(vocabulary):
    global dynamic_vocabulary
    global static_vocabulary
    if vocabulary == 'dynamic':
        vocabulary_global = dynamic_vocabulary
    elif vocabulary == 'static':
        vocabulary_global = static_vocabulary
    else:
        assert 0

    for dlist in vocabulary_global.itervalues():
        dlist.clear()

    vocab_dir = os.path.join(aenea.config.PROJECT_ROOT, 'vocabulary_config', vocabulary)
    for fn in os.listdir(vocab_dir):
        if fn.endswith('.json'):
            try:
                with open(os.path.join(vocab_dir, fn)) as fd:
                    v = json.load(fd)
                    update_vocabulary(
                        vocabulary,
                        v['name'],
                        v['tags'],
                        v.get('vocabulary', {}),
                        v.get('shortcuts', {})
                        )
            except Exception:
                print 'Error loading dynamic vocabulary file %s.' % fn
                raise


def get_static_vocabulary(tag):
    global static_vocabulary
    load_vocabulary('static')
    if tag not in static_vocabulary:
        # Dragonfly chokes on unicode.
        static_vocabulary[str(tag)] = {}
    return static_vocabulary[tag]


def get_dynamic_vocabulary(tag):
    global dynamic_vocabulary
    load_vocabulary('dynamic')
    if tag not in dynamic_vocabulary:
        # Dragonfly chokes on unicode.
        dynamic_vocabulary[str(tag)] = dragonfly.DictList('dynamic %s' % str(tag))
    return dynamic_vocabulary[tag]


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

