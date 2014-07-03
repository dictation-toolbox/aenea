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

'''The vocabulary module provides control of dynamic and static
vocabulary.  These are user-configurable mappings of phrases to
actions that can be dynamically updated and en/dis-abled on demand,
and shared across modules.'''

from aenea.lax import (
    Text,
    Key
    )

from wrappers import NoAction

from aenea.proxy_actions import ProxyMousePhantomClick as MousePhantomClick

try:
    import dragonfly
    from dragonfly import (
        Pause,
        Mimic
        )
except ImportError:
    import dragonfly_mock as dragonfly
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
import aenea.configuration

_vocabulary = {'static': {}, 'dynamic': {}}

# dynamic only
_disabled_vocabularies = set()

# Hack because Dragonfly is weird about when you can clear a DictList.
# We have native dicts store the data, and sync to whichever DictList
# most recently was registered. This is necessary so grammars can be
# reloaded with mic off/on. We store static the same way for simplicity.
_lists = {'static': {}, 'dynamic': {}}

# mapping from inhibited_vocab_name to list of (inhibit_context, inhibiting_grammar)
_vocabulary_inhibitions = {}

# global DictList that takes inhibitions into account. Reserved for _vocabulary.
_global_list = None

# global List that lists all dynamic vocabularies. Reserved for _vocabulary.
_list_of_dynamic_vocabularies = None

_watchers = {
    'dynamic': aenea.configuration.ConfigDirWatcher(('vocabulary_config', 'dynamic')),
    'static': aenea.configuration.ConfigDirWatcher(('vocabulary_config', 'static'))
    }

_enabled_watcher = aenea.configuration.ConfigWatcher(('vocabulary_config', 'enabled'))


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

    if force_reload or any(w.refresh() for w in _watchers.itervalues()):
        for vocabulary in 'static', 'dynamic':
            for kind in _vocabulary[vocabulary].itervalues():
                del kind[:]

            for (fn, watcher) in _watchers[vocabulary].files.iteritems():
                vox = watcher.conf
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
            _rebuild_lists('static')

    _load_enabled_from_disk()
    _rebuild_lists('dynamic')


def _load_enabled_from_disk():
    '''Sets the set of enabled grammars from the disk file.'''
    if _enabled_watcher.refresh():
        disabled = set()
        for (name, status) in _enabled_watcher.conf.iteritems():
            if not status:
                disabled.add(name)
        _disabled_vocabularies.clear()
        _disabled_vocabularies.update(disabled)
        _enabled_watcher.write()


def _save_enabled_to_disk():
    _enabled_watcher.conf.clear()
    for name in _vocabulary['dynamic']:
        _enabled_watcher.conf[name] = name not in _disabled_vocabularies
    _enabled_watcher.write()


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
    win = aenea.config.get_window_foreground()
    for name, vocabs in _vocabulary[vocabulary].iteritems():
        for (tags, vocab) in vocabs:
            if name not in _disabled_vocabularies:
                if ('global' in tags and vocabulary == 'dynamic' and _global_list is not None):
                    global_inhibited = False
                    for tag in tags:
                        if any(c is None or c.matches(win.executable, win.title, win.handle)
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
    _save_enabled_to_disk()


def register_dynamic_vocabulary(tag):
    '''Call this to register a dynamic vocabulary hook in your grammar that
       users can configure. It returns a DictList, which will always be
       kept up to date with user updates. You need to call unregister when
       the grammar is unloaded or your module won't successfully reload without
       restarting Dragon.'''
    global _lists
    _lists['dynamic'][str(tag)] = dragonfly.DictList('dynamic %s' % str(tag))
    refresh_vocabulary()
    _load_enabled_from_disk()
    _save_enabled_to_disk()
    return _lists['dynamic'][str(tag)]


def disable_dynamic_vocabulary(name):
    '''Disables all dynamic vocabularies with the specified name immediately.
       No reload or mic off/on is necessary.'''
    _load_enabled_from_disk()
    _disabled_vocabularies.add(name)
    _rebuild_lists('dynamic')
    _save_enabled_to_disk()


def enable_dynamic_vocabulary(name):
    '''Enables all dynamic vocabularies with the specified name immediately.
       No reload or mic off/on is necessary.'''
    _load_enabled_from_disk()
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
    # Don't rebuild here because the grammar may not yet have loaded.
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
