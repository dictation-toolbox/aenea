'''Wrappers provide classes that respect the current value of config.PLATFORM.
   When Aenea server is enabled, we will dispatch all requests and actions
   there. When it is not, we execute locally. The value of PLATFORM when the
   action.execute() or context.matches() is called  is what determines what
   happens.'''

import dragonfly

import aenea.config


class NoAction(dragonfly.ActionBase):
    '''Does nothing. Can be composed with +, etc and other actions.'''
    def execute(self):
        pass


class PlatformContext(dragonfly.Context):
    '''A context that delegates to either a local or proxy context object
       as appropriate.'''

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


class PlatformAction(dragonfly.ActionBase):
    '''Performs one action when config.PLATFORM is proxy, another for local.
       Useful for things that need to break out of the grammar system (eg,
       to query the filesystem to provide shell autocomplete).'''

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

    def execute(self):
        if aenea.config.PLATFORM == 'proxy':
            action = self._proxy_action
        else:
            action = self._local_action
        if hasattr(action, 'execute'):
            action.execute()
        else:
            action()
