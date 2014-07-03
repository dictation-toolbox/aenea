# This is a command module for Dragonfly. It provides support for several of
# Aenea's built-in capabilities. This module is NOT required for Aenea to
# work correctly, but it is strongly recommended.

import os

import dragonfly

try:
    import aenea
    import aenea.proxy_contexts
    import aenea.vocabulary
    import aenea.config
except ImportError:
    print 'Unable to import Aenea client-side modules.'
    raise

print 'Aenea client-side modules loaded successfully'
print 'Settings:'
print '\tHOST:', aenea.config.DEFAULT_SERVER_ADDRESS[0]
print '\tPORT:', aenea.config.DEFAULT_SERVER_ADDRESS[1]
print '\tPLATFORM:', aenea.config.PLATFORM
print '\tUSE_MULTIPLE_ACTIONS:', aenea.config.USE_MULTIPLE_ACTIONS
print '\tSCREEN_RESOLUTION:', aenea.config.SCREEN_RESOLUTION

try:
    aenea.proxy_contexts._get_context()
    print 'Aenea: Successfully connected to server.'
except:
    print 'Aenea: Unable to connect to server.'


# Commands that can be rebound.
command_table = [
    'set proxy server to <proxy>',
    'disable proxy server',
    'enable proxy server',
    'force natlink to reload all grammars'
    ]
command_table = aenea.vocabulary.make_grammar_commands(
    'aenea',
    dict(zip(command_table, command_table))
    )


class DisableRule(dragonfly.CompoundRule):
    spec = command_table['disable proxy server']

    def _process_recognition(self, node, extras):
        aenea.config.disable_proxy()


class EnableRule(dragonfly.CompoundRule):
    spec = command_table['enable proxy server']

    def _process_recognition(self, node, extras):
        aenea.config.enable_proxy()


# Note that you must also turn mic off then on after saying this.
class ReloadGrammarsRule(dragonfly.CompoundRule):
    spec = command_table['force natlink to reload all grammars']

    def _process_recognition(self, node, extras):
        path = 'C:\\NatLink\\NatLink\\MacroSystem'
        for g in os.listdir(path):
            fn = os.path.join(path, g)
            with open(fn, 'a') as fd:
                fd.write(' ')


server_list = dragonfly.DictList('aenea servers')


class ChangeServer(dragonfly.CompoundRule):
    spec = command_table['set proxy server to <proxy>']
    extras = [dragonfly.DictListRef('proxy', server_list)]

    def _process_recognition(self, node, extras):
        aenea.config.set_server_address((extras['proxy']['host'], extras['proxy']['port']))

    def _process_begin(self):
        #TODO: this has to be ugly but it doesn'th ave to be this ugly.
        server_list.clear()
        for k, v in aenea.vocabulary.load_grammar_config('aenea').get('servers', {}).iteritems():
            server_list[str(k)] = v

grammar = dragonfly.Grammar('aenea')

grammar.add_rule(EnableRule())
grammar.add_rule(DisableRule())
grammar.add_rule(ReloadGrammarsRule())
grammar.add_rule(ChangeServer())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    aenea.vocabulary.unregister_list_of_dynamic_vocabularies()
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
