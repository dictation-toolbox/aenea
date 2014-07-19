# This is a command module for Dragonfly. It lets you enable/disable the
# dictation capture client by voice.

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

import dragonfly

try:
    import aenea
except ImportError:
    print 'Unable to import Aenea client-side modules.'
    raise


_config = aenea.configuration.ConfigWatcher(
    'dictation_capture_state',
    {'enabled': True})


# Commands that can be rebound.
command_table = [
    'enable dictation capture',
    'disable dictation capture'
    ]
command_table = aenea.configuration.make_grammar_commands(
    'capture_client_control',
    dict(zip(command_table, command_table))
    )


def enable_capture():
    _config.refresh()
    _config.conf['enabled'] = True
    _config.write()


def disable_capture():
    _config.refresh()
    _config.conf['enabled'] = False
    _config.write()


class ControlRule(dragonfly.MappingRule):
    mapping = {
        'enable dictation capture': dragonfly.Function(enable_capture),
        'disable dictation capture': dragonfly.Function(disable_capture)
        }


grammar = dragonfly.Grammar('capture_client_control')

grammar.add_rule(ControlRule())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
