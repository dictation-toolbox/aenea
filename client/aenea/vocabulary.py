'''The vocabulary module provides control of dynamic and static
vocabulary.  These are user-configurable mappings of phrases to
actions that can be dynamically updated and en/dis-abled on demand,
and shared across modules.

The module also provides functionality to parse per-grammar config files.'''

import json
import os

try:
    import dragonfly
    from aenea.proxy_nicknames import (
        Text,
        Key,
        Pause,
        Mimic,
        MousePhantomClick,
        NoAction
        )
except ImportError:
    import dragonfly_mock as dragonfly
    print 'Unable to import Dragonfly (safe to ignore if running tests)'
    # TODO: mock this better

    class ActionMock(object):
        def __init__(self, *a, **kw):
            pass

        def __add__(self, other):
            return self

    class Text(ActionMock):
        pass

    class Pause(ActionMock):
        pass

    class Mimic(ActionMock):
        pass

    class MousePhantomClick(ActionMock):
        pass

    class NoAction(ActionMock):
        pass

    class Key(ActionMock):
        pass

import aenea.config

_ENABLED_JSON_PATH = os.path.join(
    aenea.config.PROJECT_ROOT,
    'vocabulary_config',
    'enabled.json'
    )


_vocabulary = {'static': {}, 'dynamic': {}}

# dynamic only
_disabled_vocabularies = set()

# Hack because Dragonfly is weird about when you can clear a DictList.
# We have native dicts store the data, and sync to whichever DictList
# most recently was registered. This is necessary so grammars can be
# reloaded with mic off/on. We store static the same way for simplicity.
_lists = {'static': {}, 'dynamic': {}}

_last_vocabulary_mtime = 0
_last_vocabulary_filenames = set(), set()

# mapping from inhibited_vocab_name to list of (inhibit_context, inhibiting_grammar)
_vocabulary_inhibitions = {}

# global DictList that takes inhibitions into account. Reserved for _vocabulary.
_global_list = None

# global List that lists all dynamic vocabularies. Reserved for _vocabulary.
_list_of_dynamic_vocabularies = None

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


def refresh_vocabulary(force_reload=False):
    '''Reloads all static and dynamic vocabulary files, if any have changed
       since they were last read. Note that changes to a static file will not
       be active until the grammar is reloaded (in practice, Dragon restarted
       unless you change the grammar file and turn mic off then on).

       Also updates the active lists, so you'll need to call this whenever the
       context changes if you want the global inhibitions to update.

       The module _vocabulary.py includes a rule to call this whenever the user
       starts to say anything.'''
    global _vocabulary

    if force_reload or _need_reload():
        for vocabulary in 'static', 'dynamic':
            for kind in _vocabulary.itervalues():
                kind.clear()

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
            if vocabulary == 'static':
                _rebuild_lists('static')

    _rebuild_lists('dynamic')
    _load_enabled_from_disk()

def _load_enabled_from_disk():
    '''Sets the set of enabled grammars from the disk file.'''
    if not os.path.exists(_ENABLED_JSON_PATH):
        _save_enabled_to_disk()
    else:
        try:
            disabled = set()
            for (name, status) in json.load(open(_ENABLED_JSON_PATH)).items():
                if not status:
                    disabled.add(name)
            _disabled_vocabularies.clear()
            _disabled_vocabularies.update(disabled)
        except Exception:
            print 'Error loading vocabulary state file ' + _ENABLED_JSON_PATH

def _save_enabled_to_disk():
    if os.path.exists(os.path.split(_ENABLED_JSON_PATH)[0]):
        conf = {}
        for name in _vocabulary['dynamic']:
            conf[name] = name not in _disabled_vocabularies
        with open(_ENABLED_JSON_PATH, 'w') as fd:
            json.dump(conf, fd)


def _rebuild_lists(vocabulary):
    global _disabled_vocabularies
    global _global_list
    global _lists
    global _vocabulary_inhibitions
    global _list_of_dynamic_vocabularies

    if vocabulary == 'dynamic':
        if _global_list is not None:
            _global_list.clear()

    for t, dlist in _lists[vocabulary].iteritems():
        if dlist:
            dlist.clear()
    for name, vocabs in _vocabulary[vocabulary].iteritems():
        for (tags, vocab) in vocabs:
            if name not in _disabled_vocabularies:
                if ('global' in tags and vocabulary == 'dynamic' and _global_list is not None):
                    global_inhibited = False
                    for tag in tags:
                        if any(c is None or c.matches(None, None, None)
                                for (c, _) in _vocabulary_inhibitions.get(tag, [])):
                            global_inhibited = True
                            break
                    if not global_inhibited:
                        _global_list.update(vocab)

                for tag in tags:
                    if vocabulary == 'static':
                        _lists[vocabulary].setdefault(tag, {})
                    # If it's dynamic, we'll build the list on
                    # demand when someone registers it, so do
                    # nothing here.
                    if tag in _lists[vocabulary]:
                        _lists[vocabulary][tag].update(vocab)

    if _list_of_dynamic_vocabularies is not None:
        _list_of_dynamic_vocabularies.set(_vocabulary['dynamic'])


def get_static_vocabulary(tag):
    '''Returns a dict of string to dragonfly.ActionBase-derived.'''
    if tag not in _lists['static']:
        # Dragonfly chokes on unicode.
        _lists['static'][str(tag)] = {}
    return _lists['static'][str(tag)]


def unregister_dynamic_vocabulary(tag):
    '''Call this to unregister a dynamic vocabulary in the unload.'''
    global _lists
    _lists['dynamic'].pop(str(tag), None)


def register_dynamic_vocabulary(tag):
    '''Call this to register a dynamic vocabulary hook in your grammar that
       users can configure. It returns a DictList, which will always be
       kept up to date with user updates. You need to call unregister when
       the grammar is unloaded or your module won't successfully reload without
       restarting Dragon.'''
    global _lists
    _lists['dynamic'][str(tag)] = dragonfly.DictList('dynamic %s' % str(tag))
    refresh_vocabulary()
    return _lists['dynamic'][str(tag)]


def disable_dynamic_vocabulary(name):
    '''Disables all dynamic vocabularies with the specified name immediately.
       No reload or mic off/on is necessary.'''
    _disabled_vocabularies.add(name)
    _rebuild_lists('dynamic')
    _save_enabled_to_disk()


def enable_dynamic_vocabulary(name):
    '''Enables all dynamic vocabularies with the specified name immediately.
       No reload or mic off/on is necessary.'''
    if name in _disabled_vocabularies:
        _disabled_vocabularies.remove(name)
    _rebuild_lists('dynamic')
    _save_enabled_to_disk()


def register_global_dynamic_vocabulary():
    '''Returns a DictList of the global vocabulary. This is like all vocabs
       tagged 'global', EXCEPT grammars may inhibit a name from being here by
       calling inhibit_name_globally when a given context is active. This is
       intended to allow more complex editing grammars (eg, multiedit), which
       have their own custom hooks for vocabs which enable advanced features
       to avoid conflicting with the global grammar.

       Note that this is NOT the same as calling
       register_dynamic_vocabulary('global'); which will ignore inhibited
       vocabularies.'''
    global _global_list
    _global_list = dragonfly.DictList(name='global inhibited')
    refresh_vocabulary()
    return _global_list


def unregister_global_dynamic_vocabulary():
    '''Unregisters the global dynamic vocabulary. You need to call this in
       any grammar that registers it at unload if you want it to be reloadable
       without restarting Dragon.'''
    global _global_list
    _global_list = None


def inhibit_global_dynamic_vocabulary(grammar_name, tag, context=None):
    '''Ensures that whenever the specified context is active, all vocabularies
       with the specified tag(s) will not be active in the global dynamic
       vocabulary.'''
    global _vocabulary_inhibitions
    if isinstance(tag, basestring):
        _vocabulary_inhibitions.setdefault(tag, [])
        _vocabulary_inhibitions[tag].append((context, grammar_name))
        _rebuild_lists('dynamic')
    else:
        for t in tag:
            inhibit_global_dynamic_vocabulary(grammar_name, t, context)


def uninhibit_global_dynamic_vocabulary(grammar_name, tag):
    '''Remove all inhibitions by grammar_name on specified tags.'''
    global _vocabulary_inhibitions
    if isinstance(tag, basestring):
        _vocabulary_inhibitions.setdefault(tag, [])
        _vocabulary_inhibitions[tag] = [
            (c, g) for (c, g) in _vocabulary_inhibitions[tag]
            if g != grammar_name
            ]
        _rebuild_lists('dynamic')
    else:
        for t in tag:
            uninhibit_global_dynamic_vocabulary(grammar_name, t)


def register_list_of_dynamic_vocabularies():
    global _list_of_dynamic_vocabularies
    _list_of_dynamic_vocabularies = dragonfly.List('list of vocabularies')
    return _list_of_dynamic_vocabularies


def unregister_list_of_dynamic_vocabularies():
    global _list_of_dynamic_vocabularies
    _list_of_dynamic_vocabularies = None


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
            agg = agg + _build_action(action)
        return agg


def _update_one_vocabulary(vocabulary, name, tags, vocab, shortcuts):
    global _vocabulary

    this_chunk = {}
    for (dataset, default) in ((vocab, Text), (shortcuts, Key)):
        for phrase, command in dataset.iteritems():
            if isinstance(command, basestring):
                this_chunk[str(phrase)] = default(str(command))
            else:
                this_chunk[str(phrase)] = _build_action_list(command)
    _vocabulary[vocabulary].setdefault(str(name), [])
    _vocabulary[vocabulary][str(name)].append((map(str, tags), this_chunk))


def _need_reload():
    global _last_vocabulary_mtime
    global _last_vocabulary_filenames
    dirty = False
    if not os.path.exists(_ENABLED_JSON_PATH):
        dirty = True
    elif os.stat(_ENABLED_JSON_PATH).st_mtime > _last_vocabulary_mtime:
        _last_vocabulary_mtime = os.stat(_ENABLED_JSON_PATH).st_mtime
        dirty = True
    for vocabulary, last_seen in zip(('static', 'dynamic'), _last_vocabulary_filenames):
        vocab_dir = os.path.join(
            aenea.config.PROJECT_ROOT,
            'vocabulary_config',
            vocabulary
            )

        if os.path.exists(vocab_dir) != bool(last_seen):
            dirty = True

        # Have any been modified since we last read them?
        if os.path.exists(vocab_dir):
            files = os.listdir(vocab_dir)
            if set(files) != last_seen:
                dirty = True
            last_seen.clear()
            last_seen.update(files)
            for fn in files:
                mtime = os.stat(os.path.join(vocab_dir, fn)).st_mtime
                if mtime > _last_vocabulary_mtime:
                    _last_vocabulary_mtime = mtime
                    dirty = True
                    # don't early abort because we want latest mtime.

    return dirty
