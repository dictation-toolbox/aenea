'''Lax wrappers will only fail at EXECUTION/MATCH time if the active platform
   can't handle the spec. So for example if you create a Key using Linux
   keysyms, everything will work fine unless you try to execute the action
   locally, in which case the key won't be pressed and a warning will be
   printed to the Natlink window. If you want your grammar to work the same
   way on all platforms, use aenea.wrappers.strict instead.'''

import aenea.proxy_actions

try:
    import dragonfly
except ImportError:
    import aenea.dragonfly_mock as dragonfly


import common


class _WarnUserUnsupportedAction(dragonfly.ActionBase):
    def execute(self):
        print 'Warning: Current platform cannot handle this action.'


def _spec(klass, a, kw):
    try:
        return klass(*a, **kw)
    except Exception:
        return _WarnUserUnsupportedAction()


class Key(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = _spec(aenea.proxy_actions.ProxyKey, a, kw)
        local = _spec(dragonfly.Key, a, kw)
        common.AeneaAction.__init__(self, proxy, local)


class Text(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = _spec(aenea.proxy_actions.ProxyText, a, kw)
        local = _spec(dragonfly.Text, a, kw)
        common.AeneaAction.__init__(self, proxy, local)


class Mouse(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = _spec(aenea.proxy_actions.ProxyMouse, a, kw)
        local = _spec(dragonfly.Mouse, a, kw)
        common.AeneaAction.__init__(self, proxy, local)

__all__ = [
    'Key',
    'Text',
    'Mouse'
    ]
