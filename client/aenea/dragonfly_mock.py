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

'''Mock module to allow importing aenea on linux (mostly so you can run tests
   locally.'''

print 'Unable to import Dragonfly (safe to ignore if running tests).'

class ActionBase(object):
    def __init__(self, spec):
        self.actions = self._parse_spec(spec)

    def execute(self, data=None):
        self._execute_events(self.actions)


class DynStrActionBase(ActionBase):
    pass


class Context(object):
    pass

DictList = lambda name: dict()
List = lambda name: list()


class _WindowInfo(object):
    executable = None
    title = None
    handle = None


class Window(object):
    @staticmethod
    def get_foreground():
        return _WindowInfo


class Repetition(object):
    def __init__(self, child, min=1, max=None, name=None, default=None):
        pass


class Choice(object):
    def __init__(self, name, choices, extras=None, default=None):
        pass


class AppContext(object):
    def __init__(self, *a, **kw):
        pass

    def matches(self, *a, **kw):
       return True
