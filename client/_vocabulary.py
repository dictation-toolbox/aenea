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

'''Manages Aenea's dynamic and static vocabulary and their global context.
   While this grammar is not strictly necessary to use them, you'll need it if
   you want a global context or being able to reload configurations on the
   fly rather than restarting Dragon.'''

import aenea.vocabulary
import aenea.configuration

import dragonfly

vocabulary_list = aenea.vocabulary.register_list_of_dynamic_vocabularies()

# Commands that can be rebound.
command_table = [
    '[refresh|reload] dynamic [vocabulary|vocabularies]',
    'enable vocabulary <vocabulary>',
    'disable vocabulary <vocabulary>',
    '<static>',
    '<dynamic>'
    ]
command_table = aenea.configuration.make_grammar_commands(
    'vocabulary',
    dict(zip(command_table, command_table))
    )


class RefreshRule(dragonfly.CompoundRule):
    spec = '[refresh|reload] dynamic [vocabulary|vocabularies]'

    def _process_begin(self):
        # Refresh every time the user starts to say anything. Refresh is
        # indeed intended to be used thus. Short of inotify and threads, this
        # is how to do it.
        aenea.vocabulary.refresh_vocabulary()

    def _process_recognition(self, node, extras):
        aenea.vocabulary.refresh_vocabulary(force_reload=True)


class EnableRule(dragonfly.CompoundRule):
    spec = command_table['enable vocabulary <vocabulary>']
    extras = [dragonfly.ListRef('vocabulary', vocabulary_list)]

    def _process_recognition(self, node, extras):
        aenea.vocabulary.enable_dynamic_vocabulary(extras['vocabulary'])


class DisableRule(dragonfly.CompoundRule):
    spec = command_table['disable vocabulary <vocabulary>']
    extras = [dragonfly.ListRef('vocabulary', vocabulary_list)]

    def _process_recognition(self, node, extras):

        aenea.vocabulary.disable_dynamic_vocabulary(extras['vocabulary'])


class StaticRule(dragonfly.CompoundRule):
    spec = command_table['<static>']

    extras = [dragonfly.DictListRef(
        'static',
        dragonfly.DictList(
            'static global',
            aenea.vocabulary.get_static_vocabulary('global')
            )
        )]

    def _process_recognition(self, node, extras):
        extras['static'].execute(extras)


class DynamicRule(dragonfly.CompoundRule):
    spec = command_table['<dynamic>']

    extras = [dragonfly.DictListRef(
        'dynamic',
        aenea.vocabulary.register_global_dynamic_vocabulary()
        )]

    def _process_recognition(self, node, extras):
        extras['dynamic'].execute(extras)


grammar = dragonfly.Grammar('vocabulary')
grammar.add_rule(RefreshRule())
grammar.add_rule(EnableRule())
grammar.add_rule(DisableRule())

grammar.add_rule(DynamicRule())
grammar.add_rule(StaticRule())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    aenea.vocabulary.unregister_list_of_dynamic_vocabularies()
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
