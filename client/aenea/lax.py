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
    import dragonfly_mock as dragonfly


import traceback

from aenea.wrappers import *


def _spec(call, a, kw):
    try:
        return call(*a, **kw)
    except Exception as e:
        return _WarnUserUnsupportedAction(e)


class _WarnUserUnsupportedAction(dragonfly.ActionBase):
    def __init__(self, exception):
        self._exception = exception

    def execute(self, data=None):
        print ('Warning: Current platform cannot handle this action. This '
               'exception was thrown at grammar load time.')
        traceback.print_tb(self._exception)

    def _parse_spec(self, spec):
        pass

    def _execute_events(self, commands):
        pass


class AeneaLaxDynStrActionBase(AeneaDynStrActionBase):
    def _parse_spec(self, spec):
        proxy = None
        local = None
        self._proxy_exception = None
        self._local_exception = None
        try:
            proxy = self._proxy._parse_spec(spec)
        except Exception as e:
            self._proxy_exception = e
        try:
            local = self._local._parse_spec(spec)
        except Exception as e:
            self._local_exception = e
        return (proxy, local)

    def _execute_events(self, commands):
        if self.get_data()['_proxy']:
            if self._proxy_exception is not None:
                traceback.print_tb(self._proxy_exception)
        else:
            if self._local_exception is not None:
                traceback.print_tb(self._local_exception)
        AeneaDynStrActionBase._execute_events(self, commands)


class Key(AeneaLaxDynStrActionBase):
    def __init__(self, spec):
        proxy = _spec(aenea.proxy_actions.ProxyKey, [spec], {})
        local = _spec(dragonfly.Key, [spec], {})
        AeneaLaxDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec,
            '%' not in spec
            )


class Text(AeneaLaxDynStrActionBase):
    def __init__(self, *a, **kw):
        if len(a) == 2:
            kw['spec'], kw['static'] = a
        elif len(a) == 1:
            kw['spec'] = a[0]
        a = []
        proxy = _spec(aenea.proxy_actions.ProxyText, a, kw)
        local = _spec(dragonfly.Text, a, kw)
        AeneaLaxDynStrActionBase.__init__(
            self,
            proxy,
            local,
            spec=kw.get('spec', None),
            static=kw.get('static', False)
            )


class Mouse(AeneaLaxDynStrActionBase):
    def __init__(self, *a, **kw):
        proxy = _spec(aenea.proxy_actions.ProxyMouse, a, kw)
        local = _spec(dragonfly.Mouse, a, kw)
        AeneaLaxDynStrActionBase.__init__(
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
