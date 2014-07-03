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

import os
import unittest
import mock
import StringIO

import aenea.config
import aenea.configuration

from configuration_mock import make_mock_conf


class TestMakeGrammarCommands(unittest.TestCase):
    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_simple(self, lgc):
        lgc.return_value = make_mock_conf({})
        self.assertEquals(
            aenea.configuration.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'sting': '10k bees'}
            )

    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_multiple(self, lgc):
        commands = {'ouch': 'sting', 'pain': 'sting', 'sting': 'sting'}
        lgc.return_value = make_mock_conf({'commands': commands})
        self.assertEquals(
            aenea.configuration.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'ouch': '10k bees', 'pain': '10k bees', 'sting': '10k bees'}
            )

    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_multiple_undef(self, lgc):
        commands = {'ouch': 'sting', 'pain': 'sting'}
        lgc.return_value = make_mock_conf({'commands': commands})
        self.assertEquals(
            aenea.configuration.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'ouch': '10k bees', 'pain': '10k bees'}
            )

    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_explicit_undefine(self, lgc):
        commands = {'!anythinggoeshere': 'sting'}
        lgc.return_value = make_mock_conf({'commands': commands})
        self.assertEquals(
            aenea.configuration.make_grammar_commands('foo', {'sting': '10k bees'}), {})

    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_implicit_undefine(self, lgc):
        commands = {'honey': 'sting'}
        lgc.return_value = make_mock_conf({'commands': commands})
        self.assertEquals(
            aenea.configuration.make_grammar_commands('foo', {'sting': '10k bees'}), {'honey': '10k bees'})

    @mock.patch('aenea.configuration.ConfigWatcher')
    def test_illegal_command(self, lgc):
        commands = {'wasp': 'nest'}
        lgc.return_value = make_mock_conf({'commands': commands})
        self.assertRaises(
            KeyError,
            aenea.configuration.make_grammar_commands,
            'foo', {'sting': '10k bees'}
            )

if __name__ == '__main__':
    unittest.main()
