import os
import unittest
import mock
import StringIO

import aenea.config
import aenea.vocabulary


VOCABULARY_CONFIG_PATH = os.path.join(
    aenea.config.PROJECT_ROOT,
    'vocabulary_config'
    )

GRAMMAR_CONFIG_PATH = os.path.join(
    aenea.config.PROJECT_ROOT,
    'grammar_config'
    )


def mock_open(files):
    def opener(fn, mode='r'):
        if fn not in files:
            raise IOError('file %s not mocked.' % fn)
        else:
            contents = StringIO.StringIO(files[fn])
        fd = mock.MagicMock()
        fd.__exit__.return_value = None
        fd.__enter__.return_value = contents
        fd.read.side_effect = contents.read
        return fd
    return opener


class TestLoadGrammar(unittest.TestCase):
    def setUp(self):
        self.foobar = os.path.join(GRAMMAR_CONFIG_PATH, 'foobar.json')

    @mock.patch('os.path.exists')
    def test_noexist(self, exists):
        exists.return_value = False
        self.assertEqual(
            aenea.vocabulary.load_grammar_config('foobar', {'foo': 'bar'}),
            {'foo': 'bar'}
            )

    @mock.patch('os.path.exists')
    @mock.patch('__builtin__.open')
    def test_empty(self, fopen, exists):
        exists.return_value = True
        fopen.side_effect = mock_open({self.foobar: '{"foo": "bar"}'})
        self.assertEqual(
            aenea.vocabulary.load_grammar_config('foobar', {'foo': 'bar'}),
            {'foo': 'bar'}
            )

    @mock.patch('os.path.exists')
    @mock.patch('__builtin__.open')
    def test_additional(self, fopen, exists):
        exists.return_value = True
        fopen.side_effect = mock_open({self.foobar: '{"baz": 3}'})
        self.assertEqual(
            aenea.vocabulary.load_grammar_config('foobar', {'foo': 'bar'}),
            {'foo': 'bar', 'baz': 3}
            )

    @mock.patch('os.path.exists')
    @mock.patch('__builtin__.open')
    def test_override(self, fopen, exists):
        exists.return_value = True
        fopen.side_effect = mock_open({self.foobar: '{"baz": 3, "boo": true}'})
        self.assertEqual(
            aenea.vocabulary.load_grammar_config('foobar', {'foo': 'bar', 'baz': 4}),
            {'foo': 'bar', 'baz': 3, 'boo': True}
            )

    @mock.patch('os.path.exists')
    @mock.patch('__builtin__.open')
    def test_defaults_not_mutated(self, fopen, exists):
        exists.return_value = True
        fopen.side_effect = mock_open({self.foobar: '{"baz": 3, "boo": true}'})
        defaults = {'foo': 'bar', 'baz': 4}
        d_pre = defaults.copy()
        self.assertEqual(
            aenea.vocabulary.load_grammar_config('foobar', defaults),
            {'foo': 'bar', 'baz': 3, 'boo': True}
            )
        self.assertEqual(d_pre, defaults)


class TestMakeGrammarCommands(unittest.TestCase):
    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_simple(self, lgc):
        lgc.return_value = {}
        self.assertEquals(
            aenea.vocabulary.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'sting': '10k bees'}
            )

    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_multiple(self, lgc):
        lgc.return_value = {'commands': {'ouch': 'sting', 'pain': 'sting', 'sting': 'sting'}}
        self.assertEquals(
            aenea.vocabulary.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'ouch': '10k bees', 'pain': '10k bees', 'sting': '10k bees'}
            )

    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_multiple_undef(self, lgc):
        lgc.return_value = {'commands': {'ouch': 'sting', 'pain': 'sting'}}
        self.assertEquals(
            aenea.vocabulary.make_grammar_commands('foo', {'sting': '10k bees'}),
            {'ouch': '10k bees', 'pain': '10k bees'}
            )

    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_explicit_undefine(self, lgc):
        lgc.return_value = {'commands': {'!anythinggoeshere': 'sting'}}
        self.assertEquals(
            aenea.vocabulary.make_grammar_commands('foo', {'sting': '10k bees'}), {})

    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_implicit_undefine(self, lgc):
        lgc.return_value = {'commands': {'honey': 'sting'}}
        self.assertEquals(
            aenea.vocabulary.make_grammar_commands('foo', {'sting': '10k bees'}), {'honey': '10k bees'})

    @mock.patch('aenea.vocabulary.load_grammar_config')
    def test_illegal_command(self, lgc):
        lgc.return_value = {'commands': {'wasp': 'nest'}}
        self.assertRaises(
            KeyError,
            aenea.vocabulary.make_grammar_commands,
            'foo', {'sting': '10k bees'}
            )


class TestMakeRefreshVocabulary(unittest.TestCase):
    def setUp(self):
        aenea.vocabulary._vocabulary = {'static': {}, 'dynamic': {}}
        aenea.vocabulary._disabled_vocabularies = set()
        aenea.vocabulary._lists = {'static': {}, 'dynamic': {}}
        aenea.vocabulary._last_vocabulary_mtime = 0

        self.sfoobar = os.path.join(VOCABULARY_CONFIG_PATH, 'static', 'foobar.json')
        self.dfoobar = os.path.join(VOCABULARY_CONFIG_PATH, 'dynamic', 'foobar.json')
        self.boring = '{"name": "foo", "tags": ["bar"], "shortcuts": {"baz": "bazaz"}}'

    def tearDown(self):
        self.setUp()

    # @mock.patch('aenea.vocabulary._need_reload')
    # def test_clean_does_not_modify(self, need_reload):
    #     aenea.vocabulary._vocabulary['dynamic']['clear'] = {}
    #     need_reload.return_value = False
    #     aenea.vocabulary.refresh_vocabulary()
    #     self.assertEquals(aenea.vocabulary._vocabulary['dynamic'], {})

    # Overly complex, but we need to test different flows.
    @mock.patch('__builtin__.open')
    @mock.patch('os.listdir')
    @mock.patch('aenea.vocabulary._need_reload')
    @mock.patch('os.path.exists')
    def test(self, exists, need_reload, listdir,
                                         fopen):
        fopen.side_effect = mock_open({self.dfoobar: self.boring})
        aenea.vocabulary._vocabulary['static']['clear'] = {}
        need_reload.return_value = True
        exists.side_effect = lambda x: x.endswith('dynamic')
        listdir.return_value = ['foobar.json']
        aenea.vocabulary.refresh_vocabulary()

        dy = aenea.vocabulary._vocabulary['dynamic']
        self.assertEqual(dy.keys(), ['foo'])
        self.assertEqual(len(dy['foo']), 1)
        tags, vocab = dy['foo'][0]
        self.assertEqual(tags, ['bar'])
        self.assertEqual(vocab.keys(), ['baz'])
        self.assertIsInstance(vocab['baz'], aenea.vocabulary.Key)
        self.assertEquals(aenea.vocabulary._vocabulary['static'], {})

        for i in xrange(3):
            aenea.vocabulary.enable_dynamic_vocabulary('foo')

            full = aenea.vocabulary.register_dynamic_vocabulary('bar')
            empty = aenea.vocabulary.register_dynamic_vocabulary('foo')

            self.assertEqual(empty, {})
            self.assertEqual(full.keys(), ['baz'])
            self.assertIsInstance(full['baz'], aenea.vocabulary.Key)

            aenea.vocabulary.disable_dynamic_vocabulary('foo')
            self.assertEqual(empty, {})
            self.assertEqual(full, {})

            aenea.vocabulary.enable_dynamic_vocabulary('foo')
            self.assertEqual(empty, {})
            self.assertEqual(full.keys(), ['baz'])
            self.assertIsInstance(full['baz'], aenea.vocabulary.Key)

            aenea.vocabulary.unregister_dynamic_vocabulary('bar')
            aenea.vocabulary.unregister_dynamic_vocabulary('foo')

            aenea.vocabulary.disable_dynamic_vocabulary('foo')

    @mock.patch('aenea.vocabulary._need_reload')
    @mock.patch('os.path.exists')
    def test_no_config_no_problem(self, exists, need_reload):
        need_reload.return_value = True
        exists.return_value = False
        aenea.vocabulary._vocabulary['dynamic']['clear'] = {}
        aenea.vocabulary._vocabulary['static']['clear'] = {}
        aenea.vocabulary.refresh_vocabulary()
        self.assertEquals(aenea.vocabulary._vocabulary['dynamic'], {})
        self.assertEquals(aenea.vocabulary._vocabulary['static'], {})


class TestGlobalVocabulary(unittest.TestCase):
    def setUp(self):
        aenea.vocabulary._vocabulary = {'static': {}, 'dynamic': {
            'foo': [(['global', 'multiedit'], {'baz': 'bazaz'})],
            'bar': [(['global', 'vim', 'multiedit'], {'bar': 'barbar'})]
            }}
        aenea.vocabulary._disabled_vocabularies = set()
        aenea.vocabulary._lists = {'static': {}, 'dynamic': {}}

    @mock.patch('aenea.vocabulary._need_reload')
    def test(self, need_reload):
        need_reload.return_value = False

        v = aenea.vocabulary.register_global_dynamic_vocabulary()
        self.assertEqual(v, {'baz': 'bazaz', 'bar': 'barbar'})

        aenea.vocabulary.disable_dynamic_vocabulary('bar')
        self.assertEqual(v, {'baz': 'bazaz'})

        m = mock.MagicMock()
        m.matches.return_value = True
        aenea.vocabulary.inhibit_global_dynamic_vocabulary('test', 'vim', m)

        self.assertEqual(v, {'baz': 'bazaz'})

        aenea.vocabulary.enable_dynamic_vocabulary('bar')

        self.assertEqual(v, {'baz': 'bazaz'})

        m.matches.return_value = False
        aenea.vocabulary.refresh_vocabulary()

        self.assertEqual(v, {'baz': 'bazaz', 'bar': 'barbar'})

if __name__ == '__main__':
    unittest.main()
