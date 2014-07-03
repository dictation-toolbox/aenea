'''Strict wrappers fail at CONSTRUCTION if the spec is not valid for BOTH
   platforms. This is preferred if you want your grammar to work identically
   on both platforms.'''

try:
    import dragonfly
except ImportError:
    import aenea.dragonfly_mock as dragonfly


import aenea.config
import aenea.proxy_actions

from aenea.wrappers import *


class Key(AeneaDynStrActionBase):
    def __init__(self, spec):
        proxy = aenea.proxy_actions.ProxyKey(spec)
        local = dragonfly.Key(spec)
        AeneaDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec,
            '%' not in spec
            )


class Text(AeneaDynStrActionBase):
    def __init__(self, *a, **kw):
        if len(a) == 2:
            kw['spec'], kw['static'] = a
        elif len(a) == 1:
            kw['spec'] = a[0]
        a = []
        proxy = aenea.proxy_actions.ProxyText(a, kw)
        local = dragonfly.Text(a, kw)
        AeneaDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec=kw.get('spec', None),
            static=kw.get('static', False)
            )


class Mouse(AeneaDynStrActionBase):
    def __init__(self, *a, **kw):
        proxy = aenea.proxy_actions.ProxyMouse(*a, **kw)
        local = dragonfly.Mouse(*a, **kw)
        AeneaDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec=kw.get('spec', None),
            static=kw.get('static', False)
            )


__all__ = [
    'Key',
    'Text',
    'Mouse'
    ]
