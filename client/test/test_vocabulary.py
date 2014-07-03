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
import aenea.vocabulary

from configuration_mock import make_mock_conf, make_mock_dir


class TestMakeRefreshVocabulary(unittest.TestCase):
    def setUp(self):
        aenea.vocabulary._vocabulary = {'static': {}, 'dynamic': {}}
        aenea.vocabulary._disabled_vocabularies = set()
        aenea.vocabulary._lists = {'static': {}, 'dynamic': {}}
        aenea.vocabulary._watchers = {
            'static': make_mock_dir({}),
            'dynamic': make_mock_dir({})
            }
        aenea.vocabulary._enabled_watcher = make_mock_conf({})

        aenea.vocabulary._watchers['static'].files['barbaz'] = self.simple1()
        aenea.vocabulary._watchers['dynamic'].files['foobaz'] = self.simple2()
        aenea.vocabulary._watchers['dynamic'].files['foobar'] = self.simple3()

        self.foo_foobar = {
            'plus': 'Text("+ ")',
            'literal entry': 'Key("c-v")'
            }

        self.foo_foobaz = {
            'bit ore': 'Text("| ")',
            'kill': 'Key("c-backslash")'
            }

        self.foo = self.foo_foobar.copy()
        self.foo.update(self.foo_foobaz)

        self.bar = {
            'plus': 'Text("+ ")',
            'literal entry': 'Key("c-v")'
            }

        self.baz = {
            'bit ore': 'Text("| ")',
            'kill': 'Key("c-backslash")'
            }

        self.s_foo = {}
        self.s_bar = self.s_baz = {
            'compare eek': 'Text("== ")',
            'interrupt': 'Key("c-c")'
            }

    def simple1(self):
        return make_mock_conf({
            'name': 'barbaz',
            'tags': ['bar', 'baz'],
            'vocabulary': {
                'compare eek': '== '
                },
            'shortcuts': {
                'interrupt': 'c-c'
                }
            })

    def simple2(self):
        return make_mock_conf({
            'name': 'foobaz',
            'tags': ['foo', 'baz', 'global'],
            'vocabulary': {
                'bit ore': '| '
                },
            'shortcuts': {
                'kill': 'c-backslash'
                }
            })

    def simple3(self):
        return make_mock_conf({
            'name': 'foobar',
            'tags': ['foo', 'bar'],
            'vocabulary': {
                'plus': '+ '
                },
            'shortcuts': {
                'literal entry': 'c-v'
                }
            })

    def mocker(self, key):
        return lambda n: '%s("%s")' % (key, n)

    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_load(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')
        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')
        bar = aenea.vocabulary.register_dynamic_vocabulary('bar')
        baz = aenea.vocabulary.register_dynamic_vocabulary('baz')
        s_foo = aenea.vocabulary.get_static_vocabulary('foo')
        s_bar = aenea.vocabulary.get_static_vocabulary('bar')
        s_baz = aenea.vocabulary.get_static_vocabulary('baz')
        self.assertEquals(foo, self.foo)
        self.assertEquals(bar, self.bar)
        self.assertEquals(baz, self.baz)
        self.assertEquals(s_foo, self.s_foo)
        self.assertEquals(s_bar, self.s_bar)
        self.assertEquals(s_baz, self.s_baz)

        aenea.vocabulary.unregister_dynamic_vocabulary('foo')
        aenea.vocabulary.unregister_dynamic_vocabulary('bar')
        aenea.vocabulary.unregister_dynamic_vocabulary('baz')

    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_enable_disable_simple(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')
        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')
        self.assertEquals(foo, self.foo)
        aenea.vocabulary.disable_dynamic_vocabulary('foobar')
        self.assertEquals(foo, self.foo_foobaz)
        aenea.vocabulary.disable_dynamic_vocabulary('foobaz')
        self.assertEquals(foo, {})
        aenea.vocabulary.enable_dynamic_vocabulary('foobar')
        self.assertEquals(foo, self.foo_foobar)
        aenea.vocabulary.enable_dynamic_vocabulary('foobaz')
        self.assertEquals(foo, self.foo)

        aenea.vocabulary.unregister_dynamic_vocabulary('foo')

    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_inhibitions(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')

        g = aenea.vocabulary.register_global_dynamic_vocabulary()
        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')

        self.assertEquals(g, self.foo_foobaz)
        self.assertEquals(foo, self.foo)

        gall = aenea.vocabulary.register_dynamic_vocabulary('global')

        self.assertEquals(gall, self.foo_foobaz)

        aenea.vocabulary.inhibit_global_dynamic_vocabulary('test', 'foo')

        self.assertEquals(g, {})
        self.assertEquals(foo, self.foo)
        self.assertEquals(gall, self.foo_foobaz)

        aenea.vocabulary.disable_dynamic_vocabulary('foobaz')

        self.assertEquals(g, {})
        self.assertEquals(foo, self.foo_foobar)
        self.assertEquals(gall, {})

        aenea.vocabulary.uninhibit_global_dynamic_vocabulary('test', 'foo')

        self.assertEquals(g, {})
        self.assertEquals(foo, self.foo_foobar)
        self.assertEquals(gall, {})

        aenea.vocabulary.enable_dynamic_vocabulary('foobaz')

        self.assertEquals(g, self.foo_foobaz)
        self.assertEquals(foo, self.foo)
        self.assertEquals(gall, self.foo_foobaz)

        aenea.vocabulary.unregister_global_dynamic_vocabulary()
        aenea.vocabulary.unregister_dynamic_vocabulary('foo')
        aenea.vocabulary.unregister_dynamic_vocabulary('global')

    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_enable_synched_to_conf(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')

        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')

        self.assertTrue(aenea.vocabulary._enabled_watcher['foobar'])
        self.assertEquals(foo, self.foo)

        aenea.vocabulary._enabled_watcher.dirty = True
        aenea.vocabulary._enabled_watcher['foobar'] = False

        aenea.vocabulary.refresh_vocabulary()

        self.assertEquals(foo, self.foo_foobaz)

        aenea.vocabulary.unregister_dynamic_vocabulary('foo')

    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_change_vocabulary_online(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')

        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')

        self.assertEquals(foo, self.foo)

        conf = aenea.vocabulary._watchers['dynamic'].files['foobar'].conf
        conf['vocabulary']['minus'] = '- '
        aenea.vocabulary._watchers['dynamic'].dirty = True

        aenea.vocabulary.refresh_vocabulary()
        good = {'minus': 'Text("- ")'}
        good.update(self.foo)
        self.assertEquals(foo, good)

        g = aenea.vocabulary.register_global_dynamic_vocabulary()
        self.assertEquals(g, self.foo_foobaz)

        conf['tags'].append('global')
        conf = aenea.vocabulary._watchers['dynamic'].dirty = True

        aenea.vocabulary.refresh_vocabulary()

        good.update(self.foo)
        self.assertEquals(g, good)

        aenea.vocabulary.unregister_dynamic_vocabulary('foo')
        aenea.vocabulary.unregister_global_dynamic_vocabulary()


    @mock.patch('aenea.vocabulary.Text')
    @mock.patch('aenea.vocabulary.Key')
    def test_remove_vocabulary_online(self, key, text):
        text.side_effect = self.mocker('Text')
        key.side_effect = self.mocker('Key')

        foo = aenea.vocabulary.register_dynamic_vocabulary('foo')
        self.assertEquals(foo, self.foo)
        del aenea.vocabulary._watchers['dynamic'].files['foobar']
        aenea.vocabulary.refresh_vocabulary()
        self.assertEquals(foo, self.foo_foobaz)

        aenea.vocabulary.unregister_dynamic_vocabulary('foo')

if __name__ == '__main__':
    unittest.main()
