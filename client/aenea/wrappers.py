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

'''Wrappers provide classes that respect the current value of config.PLATFORM.
   When Aenea server is enabled, we will dispatch all requests and actions
   there. When it is not, we execute locally. The value of PLATFORM when the
   action.execute() or context.matches() is called  is what determines what
   happens.'''

try:
    import dragonfly
except ImportError:
    import aenea.dragonfly_mock as dragonfly

try:
   from dragonfly import (
      Grammar,
      ActionBase,
      ActionError,
      Alternative,
      AppContext,
      Choice,
      Clipboard,
      Compound,
      CompoundRule,
      Config,
      ConnectionGrammar,
      Context,
      DictList,
      DictListRef,
      Dictation,
      Digits,
      DigitsRef,
      DynStrActionBase,
      ElementBase,
      Empty,
      FocusWindow,
      FormatState,
      Function,
      HardwareInput,
      Integer,
      IntegerRef,
      Item,
      Key,
      Keyboard,
      KeyboardInput,
      List,
      ListBase,
      ListRef,
      Literal,
      MappingRule,
      Mimic,
      Monitor,
      Mouse,
      MouseInput,
      Number,
      NumberRef,
      Optional,
      Paste,
      Pause,
      Playback,
      PlaybackHistory,
      Point,
      RecognitionHistory,
      RecognitionObserver,
      Rectangle,
      Repeat,
      Repetition,
      Rule,
      RuleRef,
      Section,
      Sequence,
      Text,
      Typeable,
      WaitWindow,
      Window,
      Word
      )
except ImportError:
   # Suppress so we can import on Linux.
   pass


import aenea.config
import aenea.communications
import aenea.proxy_contexts


def ensure_execution_context(data):
    '''Populates the data field of execute with context information if not
      present.'''
    if data is None:
        data = {}
    if '_proxy' not in data:
        data['_proxy'] = aenea.config.proxy_active()
    if '_server_info' not in data:
        data['_server_info'] = aenea.proxy_contexts._server_info()
    if '_proxy_context' not in data:
        data['_proxy_context'] = aenea.proxy_contexts._get_context()
    if '_context' not in data:
        data['_context'] = dragonfly.Window.get_foreground()
    return data


class NoAction(dragonfly.ActionBase):
    '''Does nothing. Useful for constructing compound actions.'''
    def execute(self, data=None):
        pass


class AlwaysContext(dragonfly.Context):
    '''Always matches. Useful for constructing compound contexts.'''
    def matches(self, windows_executable, windows_title, windows_handle):
        return True


class NeverContext(dragonfly.Context):
    '''Never matches. Useful for constructing compound contexts.'''
    def matches(self, windows_executable, windows_title, windows_handle):
        return False


class AeneaContext(dragonfly.Context):
    '''A context that delegates to either a local or proxy context object
       as appropriate. See also ProxyPlatformContext; which matches one of
       several contexts via proxy based on the OS on the other end.'''

    def __init__(self, proxy_context, local_context):
        '''proxy_context and remote_context may be dragonfly.Context
           subclasses or callables.'''
        assert(hasattr(proxy_context, 'matches') or
               hasattr(proxy_context, '__call__'))
        assert(hasattr(local_context, 'matches') or
               hasattr(local_context, '__call__'))
        self._proxy_context = proxy_context
        self._local_context = local_context
        dragonfly.Context.__init__(self)

    def matches(self, executable, title, handle):
        if aenea.config.PLATFORM == 'proxy':
            context = self._proxy_context
        else:
            context = self._local_context
        if hasattr(context, 'matches'):
            return context.matches(executable, title, handle)
        else:
            return context(executable, title, handle)


class AeneaAction(dragonfly.ActionBase):
    '''Performs one action when config.PLATFORM is proxy, another for local.
       Useful for things that need to break out of the grammar system (eg,
       to query the filesystem to provide shell autocomplete), as well as
       providing grammars that work both on the VM and remote.'''

    def __init__(self, proxy_action, local_action):
        '''proxy_action and remote_action may be dragonfly.ActionBase
            subclasses or callables.'''
        assert(hasattr(proxy_action, 'execute') or
               hasattr(proxy_action, '__call__'))
        assert(hasattr(local_action, 'execute') or
               hasattr(local_action, '__call__'))
        self._proxy_action = proxy_action
        self._local_action = local_action
        dragonfly.ActionBase.__init__(self)

    def execute(self, data=None):
        data = ensure_execution_context(data)
        if data['_proxy']:
            action = self._proxy_action
        else:
            action = self._local_action
        if hasattr(action, 'execute'):
            action.execute(data)
        else:
            action(data)


class AeneaDynStrActionBase(dragonfly.DynStrActionBase):
    def __init__(self, proxy, local, spec=None, static=False):
        self._proxy = proxy
        self._local = local
        dragonfly.DynStrActionBase.__init__(self, spec=spec, static=static)

    def _execute(self, data=None):
        # Crude, but better than copy-pasting the execute code.
        self._data = ensure_execution_context(data)
        dragonfly.DynStrActionBase._execute(self, data)

    def get_data(self):
        '''Returns the execution data.'''
        return self._data

    def _parse_spec(self, spec):
        proxy = self._proxy._parse_spec(spec)
        local = self._local._parse_spec(spec)
        return (proxy, local)

    def _execute_events(self, commands):
        if self.get_data()['_proxy']:
            return self._proxy._execute_events(commands[0])
        else:
            return self._local._execute_events(commands[1])


class ContextAction(dragonfly.ActionBase):
    '''Take a different action depending on which context is currently
       active.'''
    def __init__(self, default=None, actions=[]):
        self.actions = actions
        self.default = default

    def add_context(self, context, action):
        self.actions.append((context, action))

    def execute(self, data=None):
        data = ensure_execution_context(data)
        win = data['_context']
        for (context, action) in self.actions:
            if context.matches(win.executable, win.title, win.handle):
                return action.execute(data)
        else:
            return self.default.execute(data)
