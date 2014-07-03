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

import unittest
import mock

from aenea.proxy_contexts import *


def match(ctx):
    return ctx.matches(None, None, None)


class TestTrivialContexts(unittest.TestCase):
    def test_always(self):
        self.assertTrue(match(AlwaysContext()))

    def test_never(self):
        self.assertFalse(match(NeverContext()))


class TestProxyCustomAppContext(unittest.TestCase):
    @mock.patch('aenea.proxy_contexts._get_context')
    def test_match_method(self, get):
        get.return_value = {'title': 'Hello World'}
        self.assertTrue(match(ProxyCustomAppContext(match='substring', title='Wor')))
        self.assertTrue(match(ProxyCustomAppContext(match='substring', title='World')))
        self.assertFalse(match(ProxyCustomAppContext(match='substring', title='Wirld')))

        self.assertFalse(match(ProxyCustomAppContext(match='regex', title='ello [W|x]')))
        self.assertTrue(match(ProxyCustomAppContext(match='regex', title='Hello [W|x]orld')))
        self.assertTrue(match(ProxyCustomAppContext(match='regex', title='.*H.*W.*')))
        self.assertFalse(match(ProxyCustomAppContext(match='regex', title='.*H.*X.*')))

        self.assertTrue(match(ProxyCustomAppContext(match='exact', title='Hello World')))
        self.assertFalse(match(ProxyCustomAppContext(match='exact', title='World')))

    @mock.patch('aenea.proxy_contexts._get_context')
    def test_logic_method(self, get):
        get.return_value = {'title': 'Hello World', 'executable': '/bin/yes'}
        self.assertTrue(match(ProxyCustomAppContext(logic='and', title='Hello World')))
        self.assertTrue(match(ProxyCustomAppContext(logic='and', title='Hello World', executable='/bin/yes')))
        self.assertFalse(match(ProxyCustomAppContext(logic='and', title='Hello World', executable='/bin/no')))
        self.assertTrue(match(ProxyCustomAppContext(logic='or', title='Hello World', executable='/bin/no')))

        self.assertFalse(match(ProxyCustomAppContext(logic='or')))
        self.assertTrue(match(ProxyCustomAppContext(logic='and')))

        self.assertTrue(match(ProxyCustomAppContext(logic=1, title='Hello World', executable='/bin/no')))
        self.assertFalse(match(ProxyCustomAppContext(logic=2, title='Hello World', executable='/bin/no')))

        self.assertTrue(match(ProxyCustomAppContext(logic=0, title='bees', executable='/bin/no')))

    @mock.patch('aenea.proxy_contexts._get_context')
    def test_special_values(self, get):
        get.return_value = {'title': 'Hello World', 'executable': '/bin/yes'}
        self.assertTrue(match(ProxyCustomAppContext(title=VALUE_DONT_CARE)))
        self.assertFalse(match(ProxyCustomAppContext(logic='or', title=VALUE_DONT_CARE)))

        self.assertTrue(match(ProxyCustomAppContext(title=VALUE_SET)))
        self.assertFalse(match(ProxyCustomAppContext(cls=VALUE_SET)))

        self.assertFalse(match(ProxyCustomAppContext(title=VALUE_NOT_SET)))
        self.assertTrue(match(ProxyCustomAppContext(cls=VALUE_NOT_SET)))

    @mock.patch('aenea.proxy_contexts._get_context')
    def test_case_sensitivity(self, get):
        get.return_value = {'title': 'Hello World'}
        self.assertTrue(match(ProxyCustomAppContext(title='hello', case_sensitive=False)))
        self.assertTrue(match(ProxyCustomAppContext(title='Hello', case_sensitive=False)))

        self.assertFalse(match(ProxyCustomAppContext(title='hello', case_sensitive=True)))
        self.assertTrue(match(ProxyCustomAppContext(title='Hello', case_sensitive=True)))

if __name__ == '__main__':
    unittest.main()
