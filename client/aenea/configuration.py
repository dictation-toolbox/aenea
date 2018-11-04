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

import json
import os

from aenea.alias import Alias
import aenea.config
from proxy_contexts import ProxyAppContext

try:
    import dragonfly
except ImportError:
    import dragonfly_mock as dragonfly


class ConfigWatcher(object):
    '''Watches a config file for changes, and reloads as necessary based on
       mtime. Path is relative to project root and may be string or list.
       Exceptions are squelched with a warning. File not existing is
       never an error.'''

    def __init__(self, path, default={}):
        if not isinstance(path, basestring):
            path = os.path.join(*path)
        self._path = path = os.path.join(aenea.config.PROJECT_ROOT, path) + '.json'
        self._mtime_size = (0, 0)
        self.conf = default
        self._exists = False
        self._default = default
        self._first = True

        self.read()

    def __getitem__(self, item):
        self.refresh()
        return self.conf[item]

    def __setitem__(self, item, value):
        self.refresh()
        self.conf[item] = value

    def write(self):
        '''Writes the config file to disk.'''
        if not os.path.exists(os.path.split(self._path)[0]):
            os.makedirs(os.path.split(self._path)[0])
        try:
            with open(self._path, 'w') as fd:
                json.dump(self.conf, fd)
        except Exception as e:
            print 'Error writing config file %s: %s.' % (self._path, str(e))

    def read(self):
        '''Forces to read the file regardless of whether its mtime has
           changed.'''
        self._exists = os.path.exists(self._path)
        if not os.path.exists(self._path):
            self.conf = self._default.copy()
            return
        stat = os.stat(self._path)
        self._mtime_size = stat.st_mtime, stat.st_size
        try:
            with open(self._path) as fd:
                self.conf = json.load(fd)
        except Exception as e:
            print 'Error reading config file %s: %s.' % (self._path, str(e))

    def refresh(self):
        '''Rereads the file if it has changed. Returns True if it changed. As a
           special case, always returns True on the first call.'''
        first = self._first
        self._first = False
        if os.path.exists(self._path) != self._exists:
            self.read()
            return True

        if os.path.exists(self._path):
            try:
                stat = os.stat(self._path)
                mtime = stat.st_mtime, stat.st_size
            except OSError:
                self.read()
                return True
            if mtime != self._mtime_size:
                self.read()
                return True
        return first


class ConfigDirWatcher(object):
    '''Watches a config directory for changes in it or its files, and reloads
       as necessary based on mtime. Path is relative to project root and may
       be string or list. Exceptions are squelched with a warning. Directory
       not existing is never an error.'''

    def __init__(self, path, default={}):
        if not isinstance(path, basestring):
            path = os.path.join(*path)
        self._rawpath = path
        self._path = path = os.path.join(aenea.config.PROJECT_ROOT, path)
        self.files = {}
        self._exists = False
        self._default = default
        self._first = True

        self.read()

    def refresh(self):
        '''Rereads the directory if it has changed. Returns True if any files
           have changed. As a special case, always returns True on the first
           call.'''
        first = self._first
        self._first = False
        if os.path.exists(self._path) != self._exists:
            self.read()
            return True

        if os.path.exists(self._path):
            files = set(x[:-5] for x in os.listdir(self._path)
                        if x.endswith('.json'))
            if set(files) != set(self.files):
                self.read()
                return True
            elif any(c.refresh() for c in self.files.itervalues()):
                return True

        return first

    def read(self):
        self._exists = os.path.exists(self._path)
        if not self._exists:
            self.files.clear()
            return

        files = set(x[:-5] for x in os.listdir(self._path)
                    if x.endswith('.json'))
        for k in self.files.keys():
            if k not in files:
                del self.files[k]

        for fn in files:
            if fn not in self.files:
                self.files[fn] = ConfigWatcher(
                    (self._rawpath, fn), self._default)
            else:
                self.files[fn].refresh()


def make_grammar_commands(module_name, mapping, config_key='commands', alias = Alias()):
    '''Given the command map from default spoken phrase to actions in mapping,
       constructs a mapping that takes user config, if specified, into account.
       config_key may be a key in the JSON to use (for modules with multiple
       mapping rules.) If a user phrase starts with !,
       no mapping is generated for that phrase.'''
    conf_path = ('grammar_config', module_name)
    conf = ConfigWatcher(conf_path).conf.get(config_key, {})
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
    return alias.make_mapping_spec(commands)


def make_local_disable_context(grammar_conf):
    """
    Given a grammar config generate a local grammar disabled context.

    Example: to locally disable multiedit where window titles contain
     "VIM" multiedit.json should contain the following keys:
    {
      "local_disable_context": "VIM"
    }

    :param grammar_conf:
    :return: Context
    """
    local_disable_setting = grammar_conf.get('local_disable_context', None)
    local_disable_context = dragonfly.NeverContext()
    if local_disable_setting is not None:
        if not isinstance(local_disable_setting, basestring):
            print 'Local disable context may only be a string.'
        else:
            local_disable_context = dragonfly.AppContext(str(local_disable_setting))
    return local_disable_context


def make_proxy_disable_context(grammar_conf):
    """
    Given a grammar config generate a local grammar disabled context.

    Example: to disable multiedit in proxy contexts where window titles
    contain "VIM" multiedit.json should contain the following keys:
    {
      "proxy_disable_context": {
        "match": "regex",
        "title": ".*VIM.*"
      }
    }

    :param grammar_conf:
    :return: Context
    """
    proxy_disable_setting = grammar_conf.get('proxy_disable_context', None)
    proxy_disable_context = dragonfly.NeverContext()
    if proxy_disable_setting is not None:
        if isinstance(proxy_disable_setting, dict):
            d = {}
            for k, v in proxy_disable_setting.iteritems():
                d[str(k)] = str(v)
            proxy_disable_context = ProxyAppContext(**d)
        else:
            proxy_disable_context = ProxyAppContext(
                title=str(proxy_disable_setting),
                match='substring'
                )
    return proxy_disable_context
