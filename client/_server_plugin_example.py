# This is a command module for Dragonfly. It provides support for several of
# Aenea's built-in capabilities. This module is NOT required for Aenea to
# work correctly, but it is strongly recommended.

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


# This is an example grammar to demonstrate the use of server plugins. It
# provides a command to call the greet_user rpc added by the example plugin.

import os

import dragonfly

try:
    import aenea.communications
except ImportError:
    print 'Unable to import Aenea client-side modules.'
    raise


def greeter(name):
    # We get a DictationContainer object since we parameterized the function
    # with a Dictation object. See anonymous_greeter for a simpler example.
    aenea.communications.server.greet_user(name.format())


def anonymous_greeter():
    aenea.communications.server.greet_user('anonymous')


class GreetUser(dragonfly.MappingRule):
    mapping = {
        'greet user example <name>': dragonfly.Function(greeter),
        'greet user anonymous example': dragonfly.Function(anonymous_greeter)
        }
    extras = [dragonfly.Dictation(name='name')]

grammar = dragonfly.Grammar('server_plugin_example')

grammar.add_rule(GreetUser())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
