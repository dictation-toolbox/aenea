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

'''provides proxy contexts for currently active application matching'''

import re
import time

import aenea.communications
import aenea.config

try:
    import dragonfly
except ImportError:
    import dragonfly_mock as dragonfly

# Match only if no value set
VALUE_NOT_SET = object()

# Match only if value set but any value
VALUE_SET = object()

# Do not consider this argument.
VALUE_DONT_CARE = object()

# Hate to do this, but currently we hit the server once per context,
# and the get_context() call is actually rather expensive. Ideally we
# could propagate state through Dragonfly contexts, but that would involve
# deep surgery or re-implementation.
_last_context = None
_last_server_info = None
_last_context_time = 0


class _Warn(dragonfly.Context):
    def matches(self, windows_executable, windows_title, windows_handle):
        pf = _server_info().get('platform', None)
        print 'Warning: grammar can\'t handle server platform %s' % pf
        return False


def _refresh_server():
    global _last_context
    global _last_context_time
    global _last_server_info
    if (
            _last_context_time is None or
            _last_context_time + aenea.config.STALE_CONTEXT_DELTA < time.time()):
        _last_context = aenea.communications.server.get_context()
        _last_server_info = aenea.communications.server.server_info()
        _last_context_time = time.time()

        # If the RPC call fails for whatever reason, we return an empty dict.
        if _last_context is None:
            _last_context = {}
        if _last_server_info is None:
            _last_server_info = {}


def _get_context():
    global _last_context
    _refresh_server()
    return _last_context


def _server_info():
    global _last_server_info
    _refresh_server()
    return _last_server_info


class ProxyCustomAppContext(dragonfly.Context):
    '''Matches based on the properties of the currently active window.
       Match may be 'substring', 'exact', or 'regex'. logic may be 'and',
       'or' or an integer (to match if at least N clauses satisfied.)'''
    def __init__(self, match='substring', logic='and', case_sensitive=False,
                 query=None, **kw):
        if query is None:
            query = {}
        query.update(kw)
        self._str = 'ProxyCustomAppContext'
        self.match = match
        self.logic = logic
        self.case_sensitive = case_sensitive
        self.arguments = query
        dragonfly.Context.__init__(self)

        assert match in ('exact', 'substring', 'regex')
        if logic not in ('and', 'or'):
            assert int(logic) >= 0 and int(logic) <= len(query)

    def _check_properties(self):
        properties = _get_context()
        matches = {}
        for (key, value) in self.arguments.iteritems():
            if value == VALUE_DONT_CARE:
                continue
            matches[key] = False
            if value == VALUE_NOT_SET:
                matches[key] = (key not in properties)
            elif value == VALUE_SET:
                matches[key] = (key in properties)
            elif key in properties:
                matches[key] = self._property_match(key, properties[key],
                                                    self.arguments[key])
        return matches

    def _property_match(self, key, actual, desired):
        '''Overload to change how we should compare actual and
           desired properties.'''
        if not self.case_sensitive:
            actual = actual.lower()
            desired = desired.lower()
        if self.match == 'substring':
            return desired in actual
        elif self.match == 'exact':
            return desired == actual
        else:
            return bool(re.match(desired, actual))

    def _reduce_matches(self, matches):
        '''Overload to change the logic that should be used to combine
           the results of the matching function.'''
        if self.logic == 'and':
            return all(matches.itervalues())
        elif self.logic == 'or':
            return any(matches.itervalues())
        else:
            return len(filter(None, matches.itervalues())) >= int(self.logic)

    def matches(self, windows_executable, windows_title, windows_handle):
        return self._reduce_matches(self._check_properties())


def ProxyAppContext(
        title=VALUE_DONT_CARE,  # active window title, as determined by server
        app_id=VALUE_DONT_CARE,  # active app name, as determined by server
        cls=VALUE_DONT_CARE,
        cls_name=VALUE_DONT_CARE,
        executable=VALUE_DONT_CARE,
        match='substring',
        logic='and',
        case_sensitive=False
        ):

    query = {
        'title': title,
        'id': app_id,
        'cls': cls,
        'cls_name': cls_name,
        'executable': executable
        }

    return ProxyCustomAppContext(match=match, logic=logic, query=query,
                                 case_sensitive=case_sensitive)


class ProxyPlatformContext(dragonfly.Context):
    '''Class that matches based on the platform the server reports. None will
       match the server not sending platform or it being set to None.
       If running locally (aenea's global context does not match or it is
       disabled), this context never matches.'''

    def __init__(self, platform):
        '''mapping is mapping from platform as string to Context.'''
        self._platform = platform
        self._str = 'ProxyPlatformContext'
        
    def matches(self, windows_executable, windows_title, windows_handle):
        enabled = aenea.config.proxy_active((
            windows_executable,
            windows_title,
            windows_handle
            ))
        return (enabled and
                (_server_info().get('platform', None) == self._platform))


class ProxyCrossPlatformContext(dragonfly.Context):
    '''Class to choose between several contexts based on what the server says
       platform is. None key may be used for none of the above as a default.'''

    def __init__(self, mapping):
        '''mapping is mapping from platform as string to Context.'''
        assert all(hasattr(x, 'matches') for x in mapping)
        self._mapping = mapping
        self._str = 'ProxyCrossPlatformContext'
        
    def matches(self, windows_executable, windows_title, windows_handle):
        platform = _server_info().get('platform', None)
        chosen = self._mapping.get(platform, self._mapping.get(None, _Warn()))
        return chosen.matches(
            windows_executable,
            windows_title,
            windows_handle
            )


__all__ = [
    'ProxyAppContext',
    'ProxyCustomAppContext',
    'ProxyPlatformContext',
    'ProxyCrossPlatformContext',
    'VALUE_NOT_SET',
    'VALUE_SET',
    'VALUE_DONT_CARE'
    ]
