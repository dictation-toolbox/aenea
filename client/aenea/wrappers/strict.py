'''Strict wrappers fail at CONSTRUCTION if the spec is not valid for BOTH
   platforms. This is preferred if you want your grammar to work identically
   on both platforms.'''

try:
    import dragonfly
except ImportError:
    import aenea.dragonfly_mock as dragonfly



import common


class Key(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyKey(*a, **kw)
        local = dragonfly.Key(*a, **kw)
        common.AeneaAction.__init__(self, proxy, local)


class Text(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyText(*a, **kw)
        local = dragonfly.Text(*a, **kw)
        common.AeneaAction.__init__(self, proxy, local)


class Mouse(common.AeneaAction):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyMouse(*a, **kw)
        local = dragonfly.Mouse(*a, **kw)
        common.AeneaAction.__init__(self, proxy, local)


__all__ = [
    'Key',
    'Text',
    'Mouse'
    ]
