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
    def __init__(self, spec, **kwargs):
        proxy = aenea.proxy_actions.ProxyKey(spec)
        local = dragonfly.Key(spec, **kwargs)
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
        proxy = aenea.proxy_actions.ProxyText(*a, **kw)
        local = dragonfly.Text(*a, **kw)
        AeneaDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec=kw.get('spec', None),
            static=kw.get('static', False)
            )

class Mouse(AeneaDynStrActionBase):
    def __init__(self, *a, **kw):
        if len(a) == 2:
            kw['spec'], kw['static'] = a
        elif len(a) == 1:
            kw['spec'] = a[0]
        a = []
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
