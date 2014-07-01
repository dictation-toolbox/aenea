'''The vocabulary module provides control of dynamic and static
vocabulary.  These are user-configurable mappings of phrases to
actions that can be dynamically updated and en/dis-abled on demand,
and shared across modules.

The module also provides functionality to parse per-grammar config files.'''

import json
import os

import dragonfly

import aenea.config

from aenea.proxy_nicknames import (
    Text,
    Key,
    Pause,
    Mimic,
    MousePhantomClick,
    NoAction
    )


_dynamic_vocabulary = {}
_static_vocabulary = {}

# Hack because Dragonfly is weird about when you can clear a DictList.
# We have native dicts store the data, and sync to whichever DictList
# most recently was registered. This is necessary so grammars can be
# reloaded with mic off/on.
_dynamic_lists = {}

_last_vocabulary_mtime = 0


def load_grammar_config(module_name, defaults=None):
    '''Loads the configuration for the specified grammar name. A missing file
       is interpreted as an empty one. defaults may be a dict of defaults.
       See also make_grammar_commands.'''
    grammar_config_path = os.path.join(
        aenea.config.PROJECT_ROOT,
        'grammar_config'
        )

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
            with open(conf_path, 'r') as fd:
                configuration.update(json.load(fd))
    except Exception as e:
        print('Could not load config file for grammar %s: %s' %
              (module_name, str(e)))
    return configuration


def make_grammar_commands(module_name, mapping, config_key='commands'):
    '''Given the command map from default spoken phrase to actions in mapping,
       constructs a mapping that takes user config, if specified, into account.
       config_key may be a key in the JSON to use (for modules with multiple
       mapping rules.) If a user phrase starts with !,
       no mapping is generated for that phrase.'''
    conf = load_grammar_config(module_name).get(config_key, {})
    commands = mapping.copy()

    # Nuke the default if the user sets one or more aliases.
    for default_phrase in set(conf.itervalues()):
        del commands[str(default_phrase)]

    for (user_phrase, default_phrase) in conf.iteritems():
        # Dragonfly chokes on unicode, JSON's default.
        user_phrase = str(user_phrase)
        default_phrase = str(default_phrase)
        assert default_phrase in mapping, ('Invalid mapping value in module %s config_key %s: %s' % (module_name, config_key, default_phrase))

        # Allow users to nuke a command with !
        if not user_phrase.startswith('!'):
            commands[user_phrase] = mapping[default_phrase]
    return commands


def refresh_vocabulary():
    '''Reloads all static and dynamic vocabulary files, if any have changed
       since they were last read. Note that changes to a static file will not
       be active until the grammar is reloaded (in practice, Dragon restarted
       unless you change the grammar file and turn mic off then on).'''
    if not _need_reload:
        return

    for vocabulary in 'static', 'dynamic':
        global _dynamic_vocabulary
        global _static_vocabulary
        global _last_vocabulary_mtime
        if vocabulary == 'dynamic':
            vocabulary_global = _dynamic_vocabulary
        elif vocabulary == 'static':
            vocabulary_global = _static_vocabulary
        else:
            assert 0

        for dlist in vocabulary_global.itervalues():
            dlist.clear()

        if vocabulary == 'dynamic':
            for t, dlist in _dynamic_lists.iteritems():
                if dlist:
                    dlist.clear()
                    dlist.update(vocabulary_global[t])

        vocab_dir = os.path.join(
            aenea.config.PROJECT_ROOT,
            'vocabulary_config',
            vocabulary
            )

        if not os.path.exists(vocab_dir):
            continue

        for fn in os.listdir(vocab_dir):
            if fn.endswith('.json'):
                try:
                    with open(os.path.join(vocab_dir, fn)) as fd:
                        vox = json.load(fd)
                        if isinstance(vox, dict):
                            vox = [vox]
                        for v in vox:
                            _update_one_vocabulary(
                                vocabulary,
                                v['name'],
                                v['tags'],
                                v.get('vocabulary', {}),
                                v.get('shortcuts', {})
                                )
                except Exception:
                    print 'Error loading %s vocabulary file %s.' % (vocabulary, fn)
                    raise


def get_static_vocabulary(tag):
    '''Returns a dict of string to dragonfly.ActionBase-derived.'''
    global _static_vocabulary
    refresh_vocabulary()
    if tag not in _static_vocabulary:
        # Dragonfly chokes on unicode.
        _static_vocabulary[str(tag)] = {}
    return _static_vocabulary[tag]


def unregister_dynamic_vocabulary(tag):
    '''Call this to unregister a dynamic vocabulary in the unload.'''
    global _dynamic_lists
    _dynamic_lists.pop(str(tag), None)


def register_dynamic_vocabulary(tag):
    '''Call this to register a dynamic vocabulary hook in your grammar that
       users can configure. It returns a DictList, which will always be
       kept up to date with user updates. You need to call unregister when
       the grammar is unloaded or your module won't successfully reload without
       restarting Dragon.'''
    global _dynamic_vocabulary

    _dynamic_lists.pop(str(tag), None)

    refresh_vocabulary()

    # Intentionally overwrite old one with new one every time this fxn is
    # called. Necessary for reloading grammars not to die.
    _dynamic_lists[str(tag)] = dragonfly.DictList('dynamic %s' % str(tag))
    _dynamic_lists[str(tag)].update(_dynamic_vocabulary[str(tag)])

    return _dynamic_lists[str(tag)]


def _build_action(action):
    '''Processes a single custom dynamic grammar action.'''
    actions = {'Text': Text, 'Key': Key, 'Pause': Pause, 'Mimic': Mimic,
               'MousePhantomClick': MousePhantomClick, 'NoAction': NoAction}
    return actions[str(action['type'])](*map(str, action['args']))


def _build_action_list(command):
    '''Processes a list of custom dynamic grammar actions into a single
       dragonfly.ActionBase subclass.'''
    if len(command) == 0:
        return NoAction()
    else:
        agg = _build_action(command[0])
        for action in command[1:]:
            agg += _build_action(action)
        return agg


def _update_one_vocabulary(vocabulary, name, tags, vocab, shortcuts):
    global _dynamic_vocabulary
    global _static_vocabulary
    if vocabulary == 'dynamic':
        vocabulary_global = _dynamic_vocabulary
    elif vocabulary == 'static':
        vocabulary_global = _static_vocabulary
    else:
        assert 0

    this_file = {}
    for (dataset, default) in ((vocab, Text), (shortcuts, Key)):
        for phrase, command in dataset.iteritems():
            if isinstance(command, basestring):
                this_file[str(phrase)] = default(str(command))
            else:
                this_file[str(phrase)] = _build_action_list(command)

    for t in tags:
        if str(t) not in vocabulary_global:
            vocabulary_global[str(t)] = {}
        vocabulary_global[str(t)].update(this_file)
        if vocabulary == 'dynamic':
            if _dynamic_lists.get(t, None) is not None:
                _dynamic_lists[t].update(this_file)


def _need_reload():
    global _last_vocabulary_mtime
    for vocabulary in 'static', 'dynamic':
        vocab_dir = os.path.join(
            aenea.config.PROJECT_ROOT,
            'vocabulary_config',
            vocabulary
            )

        # Have any been modified since we last read them?
        dirty = False
        if os.path.exists(vocab_dir):
            for fn in os.listdir(vocab_dir):
                mtime = os.stat(os.path.join(vocab_dir, fn)).st_mtime
                if mtime > _last_vocabulary_mtime:
                    _last_vocabulary_mtime = mtime
                    dirty = True
                    # don't early abort because we want latest mtime.

        return dirty
