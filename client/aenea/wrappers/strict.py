'''Strict wrappers fail at CONSTRUCTION if the spec is not valid for BOTH
   platforms. This is preferred if you want your grammar to work identically
   on both platforms.'''

import dragonfly

import aenea.wrappers.common


class Key(aenea.wrappers.common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyKey(*a, **kw)
        local = dragonfly.Key(*a, **kw)
        aenea.wrappers.common.AeneaAction.__init__(self, proxy, local)


class Text(aenea.wrappers.common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyText(*a, **kw)
        local = dragonfly.Text(*a, **kw)
        aenea.wrappers.common.AeneaAction.__init__(self, proxy, local)


class Mouse(aenea.wrappers.common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyMouse(*a, **kw)
        local = dragonfly.Mouse(*a, **kw)
        aenea.wrappers.common.AeneaAction.__init__(self, proxy, local)


__all__ = [
    'Key',
    'Text',
    'Mouse'
    ]
