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

class MockConfigWatcher(object):
    '''Provides similar API to ConfigWatcher but can be easilycontrolled
       by tests by changing the conf and dirty attributes.'''
    def __init__(self, path, default={}):
        self.dirty = True
        self.conf = default

    def __getitem__(self, item):
        return self.conf[item]

    def __setitem__(self, item, value):
        self.conf[item] = value

    def write(self):
        pass

    def read(self):
        self.dirty = False

    def refresh(self):
        dirty, self.dirty = self.dirty, False
        return dirty


class MockConfigDirWatcher(object):
    '''Provides similar API to ConfigDirWatcher but can be easilycontrolled
       by tests by changing the files and dirty attributes.'''

    def __init__(self, path, default={}):
        self.dirty = True
        self.files = {}

    def read(self):
        self.dirty = False

    def refresh(self):
        dirty, self.dirty = self.dirty, False
        return dirty


def make_mock_conf(conf):
    c = MockConfigWatcher(None)
    c.conf = conf
    return c


def make_mock_dir(files):
    c = MockConfigDirWatcher(None)
    c.files = dict((name, make_mock_conf(conf))
                   for (name, conf) in files.iteritems())
    return c
